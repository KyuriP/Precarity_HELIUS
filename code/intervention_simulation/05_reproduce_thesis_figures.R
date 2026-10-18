# Reproduce corrected thesis Figures 6.4 and 6.8 without running the historical
# exploratory pipeline. Run from the repository root. See THESIS_CORRECTIONS.txt.
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(jsonlite); library(ggplot2)
  library(tidyr); library(patchwork)
})
args <- commandArgs(trailingOnly=TRUE)
if(length(args)<1) stop('Supply the path to the authorized HELIUS_LEONIE.sav file.')
repo <- normalizePath('.')
python <- if(length(args)>1) args[2] else 'python3'
build <- file.path(repo,'data/intervention/thesis_correction')
output <- file.path(repo,'draft/thesis_corrections/figures')
dir.create(build,recursive=TRUE,showWarnings=FALSE)
dir.create(output,recursive=TRUE,showWarnings=FALSE)
env <- new.env(parent=globalenv())
env$dat <- read_sav(args[1])
for(e in parse(file.path(repo,'code/utils/preprocess_dat.R'))) {
  if(is.call(e)&&identical(e[[1]],as.name('source'))) next
  if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('dat'))) next
  eval(e,env)
  if(exists('clust_data',env,inherits=FALSE)) break
}
x <- as.matrix(env$clust_data[,c('PHQsum','P.soc','S.fin')])
cv <- cov(x)
inputs <- list(n=nrow(x),S_min=min(x[,3]),
  moments=c(1,cv[2,2],cv[3,3],cv[1,2],cv[1,3],cv[2,3]))
expected <- c(1,.80712852965242976,.74280611561567922,
              .33553322010641318,.30806409724712824,.19756368477893649)
stopifnot(nrow(x)==21628,max(abs(inputs$moments-expected))<1e-12)
write_json(inputs,file.path(build,'inputs.json'),pretty=TRUE,digits=16,auto_unbox=TRUE)
stress <- data.frame(value=sort(unique(x[,3])))
stress$count <- vapply(stress$value,function(z)sum(x[,3]==z),integer(1))
write.csv(stress,file.path(build,'stress_distribution.csv'),row.names=FALSE)
ex <- parse(file.path(repo,'code/intervention_simulation/04_linear_model.R'))
expr <- function(name) {
  z <- Filter(function(e)is.call(e)&&identical(e[[1]],as.name('<-'))&&
                identical(e[[2]],as.name(name)),ex)
  stopifnot(length(z)==1); z[[1]]
}
# Reuse the corrected calibration and original plotting expressions only.
eval(expr('get_model_parameters_from_alpha_dp'))
momentargs <- as.list(setNames(inputs$moments,c('varD','varP','varS','covDP','covDS','covPS')))
pars <- function(a)do.call(get_model_parameters_from_alpha_dp,c(list(alpha_dp=a),momentargs))
u <- momentargs$varD-momentargs$covDS^2/momentargs$varS
v <- momentargs$varP-momentargs$covPS^2/momentargs$varS
w <- momentargs$covDP-momentargs$covDS*momentargs$covPS/momentargs$varS
C <- matrix(c(u,w,w,v),2,2)
residuals <- sapply(seq(0,2*w/v,length.out=401),function(a) {
  p <- pars(a); A <- matrix(c(-1,p$alpha_pd,a,-1),2,2)
  stopifnot(max(Re(eigen(A)$values))<0)
  max(abs(A%*%C+C%*%t(A)+diag(c(p$sigma_d^2,p$sigma_p^2))))
})
stopifnot(max(residuals)<1e-12)
param_grid <- bind_rows(lapply(seq(0,.65,.05),function(a)data.frame(alpha_dp=a,as.list(pars(a)))))
eval(expr('param_labels'));eval(expr('param_plot'))
ggsave(file.path(output,'Figure_6_4_corrected.pdf'),param_plot,width=9,height=7)
ggsave(file.path(output,'Figure_6_4_corrected.png'),param_plot,width=9,height=7,dpi=150)
run_python <- function(script,arguments) {
  status <- system2(python,c(shQuote(file.path(repo,'code/intervention_simulation',script)),shQuote(arguments)))
  if(status!=0)stop(paste(script,'failed'))
}
run_python('thesis_snapshot_means.py',build)
summary_snap_filt <- read.csv(file.path(build,'A-means.csv'))
eval(expr('snapshot_plot'))
snapshot_plot <- snapshot_plot+labs(title='State Means Across Interventions at Key Times')
approved_A <- snapshot_plot+labs(tag='A')+theme(
  plot.tag=element_text(face='bold',size=20),plot.title=element_text(face='bold',size=20,hjust=.5),
  plot.subtitle=element_text(size=14,hjust=.5),plot.margin=margin(10,10,10,10),legend.title=element_text(face='bold'))
ggsave(file.path(build,'A-approved.png'),approved_A,width=11,height=8.5,dpi=150)
# Preserve the archived B seed, sample order, innovations and faint paths.
N <- 500L; T <- 3000L; dt <- .01
intervention_start <- 300L; intervention_end <- 1500L
set.seed(123)
means <- list(); paths <- list()
for(a in c(0,.65)) {
  p <- pars(a); ss <- sample(env$clust_data$S.fin,N,replace=TRUE)
  Z <- array(0,c(T-1L,2L,N))
  for(i in seq_len(N))Z[,,i] <- matrix(rnorm(2L*(T-1L),0,sqrt(dt)),ncol=2,byrow=TRUE)
  D <- P <- matrix(0,T,N)
  for(t in 2:T) {
    S <- if(t>intervention_start&&t<=intervention_end)rep(inputs$S_min,N) else ss
    D[t,] <- D[t-1,]+dt*(p$alpha_ds*S+a*P[t-1,]-D[t-1,])+.3*p$sigma_d*Z[t-1,1,]
    P[t,] <- P[t-1,]+dt*(p$alpha_ps*S+p$alpha_pd*D[t-1,]-P[t-1,])+.3*p$sigma_p*Z[t-1,2,]
  }
  means[[length(means)+1]] <- data.frame(alpha_dp=a,time=rep(1:T,2),
    Variable=rep(c('D','P'),each=T),mean=c(rowMeans(D),rowMeans(P)))
  paths[[length(paths)+1]] <- data.frame(alpha_dp=a,time=rep(1:T,2*N),
    Variable=rep(c('D','P'),each=T*N),ID=rep(rep(1:N,each=T),2),value=c(D,P))
}
summary_traj <- bind_rows(means);traj_df_filtered <- bind_rows(paths)
write.csv(summary_traj,file.path(build,'B-means.csv'),row.names=FALSE)
eval(expr('vars_colors'));eval(expr('traj_plot'))
traj_plot <- traj_plot+labs(title='Response During and After Support')
eval(expr('combined_fig'))
ggsave(file.path(build,'combined-B.png'),combined_fig,width=11,height=17,dpi=300)
run_python('compose_thesis_figure.py',c(build,output))
write_json(list(N=N,dt=dt,A_seed=2026101807,B_seed=123,noise_multiplier=.3,
  max_stationarity_residual=max(residuals),admissible_alpha_DP_bound=2*w/v,
  max_feedback_product=w^2/u/v,expected_mean_change_from_noise_scaling=0),
  file.path(output,'validation.json'),pretty=TRUE,auto_unbox=TRUE,digits=16)
writeLines(capture.output(sessionInfo()),file.path(build,'R-session-info.txt'))
message('Corrected thesis figures saved to ',output)
