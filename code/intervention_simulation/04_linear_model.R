## =============================================================================
## Linear Feedback SDE Model Analysis
## =============================================================================
#' This script implements a stochastic linear feedback model of Depression (D)
#' and Precarity (P), influenced by Stressor (S), calibrated using empirical covariances.
#'
#' **Workflow:**
#'  0. Load data & compute covariance structure  
#'  1. Calibrate model by estimating α_dp (effect of P → D) from covariances  
#'  2. Visualize parameter trade-offs across α_dp grid  
#'  3. Run snapshot simulations at selected times → intervention effects  
#'  4. Simulate full trajectories with intervention ON/OFF → recovery dynamics  
#'  5. Combine plots using patchwork for publication-ready output
## =============================================================================

# Load libraries & data
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(furrr)
library(scales)
library(viridis)
library(patchwork)

source("Precarity_HELIUS/code/utils/preprocess_dat.R")  # Load HELIUS data

set.seed(123)

# ======================================
# 0. Model Setup
# ======================================

# Empirical covariance values from HELIUS dataset
varD  <- 1
varP  <- var(clust_data$P.soc)
varS  <- var(clust_data$S.fin)
covDP <- cov(clust_data$PHQsum, clust_data$P.soc)
covDS <- cov(clust_data$PHQsum, clust_data$S.fin)
covPS <- cov(clust_data$P.soc,   clust_data$S.fin)

# Calibration function
#' Estimate model parameters from α_dp and empirical covariances
#'
#' @param alpha_dp one of numeric feedback coef P->D
#' @param varD,varP,varS variances of D, P, S
#' @param covDP,covDS,covPS covariances among Depression, Precarity, Stressor
#' @return Named list with alpha_ds, alpha_ps, alpha_pd, sigma_d, sigma_p
get_model_parameters_from_alpha_dp <- function(alpha_dp, varD, varP, varS, covDP, covDS, covPS) {
  alpha_ds <- (covDS - alpha_dp * covPS) / varS
  
  alpha_pd <- (
    -alpha_dp * covPS^2 + alpha_dp * varP * varS -
      2 * covDP * varS + 2 * covDS * covPS
  ) / (covDS^2 - varD * varS)
  
  alpha_ps <- (
    -covDS^2 * covPS +
      covDS * (alpha_dp * covPS^2 - alpha_dp * varP * varS + 2 * covDP * varS) -
      covPS * varD * varS
  ) / (varS * (covDS^2 - varD * varS))
  
  sigma_d2 <- -(
    2 * (covDS^2 - varD * varS - covDS * covPS * alpha_dp + covDP * varS * alpha_dp)
  ) / varS
  
  sigma_p2 <- (
    8 * covDP * covDS * covPS * varS -
      4 * covDP^2 * varS^2 -
      2 * covDS^2 * (covPS^2 + varP * varS) +
      2 * covDS * covPS * (covPS^2 - varP * varS) * alpha_dp +
      2 * varS * (-covPS^2 + varP * varS) * (varD + covDP * alpha_dp)
  ) / (varS * ( -covDS^2 + varD * varS ))
  
  list(
    alpha_ds = alpha_ds,
    alpha_ps = alpha_ps,
    alpha_pd = alpha_pd,
    sigma_d  = sqrt(sigma_d2),
    sigma_p  = sqrt(sigma_d2)
  )
}

# Simulate a trajectory for given α_dp and S0
#' @param alpha_dp one of numeric feedback coef P->D
#' @param s0 initial stressor level
#' @param T,dt time horizon and step size
#' @param intervention_time step index when intervention starts (not used here)
#' @param var*,cov* empirical covariance values
#' @param lambda_D,P scaling constants (default = 1)
#' @param d0,p0 initial values for D and P
#' @return tibble with columns time, D, P, S
simulate_trajectory <- function(alpha_dp, s0, T, dt, intervention_time,
                                varD, varP, varS, covDP, covDS, covPS,
                                lambda_D = 1, lambda_P = 1,
                                d0 = 0, p0 = 0) {
  params <- get_model_parameters_from_alpha_dp(alpha_dp, varD, varP, varS, covDP, covDS, covPS)
  with(params, {
    S <- rep(s0, T)
    D <- numeric(T); P <- numeric(T)
    D[1] <- d0;    P[1] <- p0
    for (t in 2:T) {
      dW1 <- rnorm(1, 0, sqrt(dt)); dW2 <- rnorm(1, 0, sqrt(dt))
      drift_D <- lambda_D * (alpha_ds * S[t] + alpha_dp * P[t-1] - D[t-1])
      drift_P <- lambda_P * (alpha_ps * S[t] + alpha_pd * D[t-1] - P[t-1])
      D[t] <- D[t-1] + drift_D * dt + sigma_d * dW1
      P[t] <- P[t-1] + drift_P * dt + sigma_p * dW2
    }
    tibble(time = 1:T, D = D, P = P, S = S)
  })
}


# Simulate trajectory with intervention and recovery phases
#' @param alpha_dp Numeric: strength of P → D feedback
#' @param s0 Numeric: initial stressor level
#' @param T Integer: total number of time steps
#' @param dt Numeric: time-step size
#' @param intervention_time Integer: time step when intervention begins 
#' @param recovery_time Integer: time step when S recovers to baseline
#' @param varD,varP,varS Numeric: variances of D, P, and S from empirical data
#' @param covDP,covDS,covPS Numeric: empirical covariances among D, P, and S
#' @param lambda_D,lambda_P Numeric: optional scaling of drift terms
#' @param d0,p0 Numeric: initial values for D and P (default zero)
#'
#' @return tibble(time, D, P, S): time-indexed simulated trajectories
simulate_with_recovery <- function(alpha_dp, s0, T, dt,
                                   intervention_time, recovery_time,
                                   varD, varP, varS, covDP, covDS, covPS,
                                   lambda_D = 1, lambda_P = 1,
                                   d0 = 0, p0 = 0) {
  # 1. Calibrate model parameters based on covariances
  p <- get_model_parameters_from_alpha_dp(
    alpha_dp, varD, varP, varS, covDP, covDS, covPS
  )
  alpha_ds <- p$alpha_ds
  alpha_ps <- p$alpha_ps
  alpha_pd <- p$alpha_pd
  sigma_d  <- p$sigma_d
  sigma_p  <- p$sigma_p
  
  # 2. Determine the global minimum S value for intervention period
  global_min_S <- min(clust_data$S.fin, na.rm = TRUE)
  
  # 3. Initialize state vectors
  S <- numeric(T)
  D <- numeric(T)
  P <- numeric(T)
  D[1] <- d0
  P[1] <- p0
  
  # 4. Build stressor (S) time series: baseline → drop → recovery
  S[1:intervention_time] <- s0
  S[(intervention_time + 1):recovery_time] <- global_min_S
  S[(recovery_time + 1):T] <- s0
  
  # 5. Simulate SDE updates for D and P over time
  for (t in 2:T) {
    # small stochastic perturbations 
    dW1 <- rnorm(1, 0, sqrt(dt)) * 0.3 # (0.3 --> scaled for plotting clarity)
    dW2 <- rnorm(1, 0, sqrt(dt)) * 0.3
    
    # drift components based on feedback structure
    drift_D <- lambda_D * (
      alpha_ds * S[t] + alpha_dp * P[t-1] - D[t-1]
    )
    drift_P <- lambda_P * (
      alpha_ps * S[t] + alpha_pd * D[t-1] - P[t-1]
    )
    
    # Euler–Maruyama integration step
    D[t] <- D[t-1] + drift_D * dt + sigma_d * dW1
    P[t] <- P[t-1] + drift_P * dt + sigma_p * dW2
  }
  
  # 6. Return results in long-format tibble
  tibble(time = seq_len(T), D = D, P = P, S = S)
}


# ======================================
# 1. Parameter Trade-Off Plot
# ======================================
# Set up parameters
alpha_dp_grid      <- seq(0, 0.698379, by = 0.05)
intervention_range <- seq(0, 1, by = 0.01)
snapshot_times     <- c(100, 200, 300, 500, 700, 1000)
T_max              <- max(snapshot_times)
N                  <- 1000
dt                 <- 0.01
intervention_time  <- 200


param_grid <- map_dfr(alpha_dp_grid, ~ {
  p <- get_model_parameters_from_alpha_dp(.x, varD, varP, varS, covDP, covDS, covPS)
  tibble(
    alpha_dp = .x,
    alpha_ds = p$alpha_ds,
    alpha_ps = p$alpha_ps,
    alpha_pd = p$alpha_pd,
    sigma_d  = p$sigma_d,
    sigma_p  = p$sigma_p
  )
})

param_labels <- c(
  alpha_ds = "alpha[DS]", alpha_pd = "alpha[PD]",
  alpha_ps = "alpha[PS]", sigma_d = "sigma[D]", sigma_p = "sigma[P]"
)

param_plot <- param_grid %>%
  pivot_longer(-alpha_dp, names_to = "param") %>%
  ggplot(aes(x = alpha_dp, y = value, color = param)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(~param, labeller = as_labeller(param_labels, label_parsed)) +
  scale_color_viridis_d(option = "D") +
  labs(
    title = expression(bold("Parameter trade-offs as a function of ")~alpha[dp]),
    x = expression(alpha[dp]), y = "Parameter value"
  ) +
  theme_bw(base_size = 14, base_family = "Palatino") +
  theme(strip.text = element_text(face = "bold", size = 13),
        axis.line = element_line(color = "black"),
        panel.grid = element_blank())

# ggsave("param_tradeoff.pdf", param_plot, width=9, height=7)




# ======================================
# 2. Response Snapshots at Key Time Points
# ======================================
plan(multisession, workers = 10) # set parallel processing


# Run chunked parallel simulations to save memory and speed up computation
# Split alpha_dp_grid into chunks of 5 for efficient computation
snapshot_results <- alpha_dp_grid %>%
  split(., ceiling(seq_along(.) / 5)) %>%
  map_dfr(function(adp_chunk) {
    
    # 1. Build combinations of alpha_dp, intervention strength, and replicate ID
    df <- expand.grid(
      alpha_dp    = adp_chunk,
      intervention = intervention_range,
      rep          = 1:N
    ) %>%
      mutate(
        s0         = sample(clust_data$S.fin, n(), TRUE),    # baseline S
        min_s      = min(clust_data$S.fin),                 # dataset-wide min S
        s_adjusted = (1 - intervention) * s0 + intervention * min_s  # intervention shift
      )
    
    # 2. Run simulations in parallel for each parameter combination
    snapshot_chunk <- future_pmap_dfr(
      list(df$alpha_dp, df$intervention, df$s_shifted, df$replicate),
      function(adp, intv, s0, rid){
        sim <- simulate_trajectory(adp, s0, T, dt, intervention_time,
                                   varD, varP, varS, covDP, covDS, covPS)
        sim %>% mutate(
          alpha_dp    = adp,
          intervention = intv,
          ID           = paste(adp, intv, rid, sep = "_")
        )
      },
      .options = furrr_options(seed = TRUE),
      .progress = TRUE
    )
    
    # 3. Save chunk to RDS and return the data frame for upstream binding
    saveRDS(snapshot_chunk, file = paste0("snapshot_chunk_adp_", adp_chunk[1], "_to_", tail(adp_chunk, 1), ".rds"))
    message("Saved chunk for alpha_dp ", adp_chunk[1], "–", tail(adp_chunk, 1))
  })


plan(sequential) # stop parallel processing

#  Load all chunked RDS files and aggregate statistics
files <- list.files(pattern = "^snapshot_chunk_adp_.*\\.rds$")
summary_list <- vector("list", length(files))
## (due to memory issue, read file one by one)
for (i in seq_along(files)) {
  df <- readRDS(files[i]) %>%
    pivot_longer(cols = c(D, P), names_to = "Variable", values_to = "value")
  
  summary_list[[i]] <- df %>%
    group_by(alpha_dp, intervention, time, Variable) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      sd   = sd(value,   na.rm = TRUE),
      .groups = "drop"
    )
  rm(df)
  gc()
}

summary_snap <- bind_rows(summary_list)
# summary_snap <- readRDS("Precarity_HELIUS/data/intervention/summarysnap_linearmodel.rds")

# Clean and label data for plotting
summary_snap_clean <- summary_snap %>%
  filter(time %in% snapshot_times) %>%            # focus on key time points
  mutate(
    t_label      = paste0("t = ", time, " steps"), # raw time labels (no dt)
    intervention = as.numeric(intervention)
  ) %>%
  select(alpha_dp, intervention, time, t_label, Variable, mean, sd) %>%
  arrange(Variable, time, alpha_dp, intervention)

# Restrict to specific snapshots: time = 100, 300, 500
summary_snap_filt <- summary_snap_clean %>%
  filter(time %in% c(100, 300, 500))

# Generate snapshot plot
snapshot_plot <- ggplot(summary_snap_filt, aes(
  x     = intervention,
  y     = mean,
  color = alpha_dp,
  group = alpha_dp
)) +
  geom_line(linewidth = 0.5) +
  facet_grid(
    Variable ~ time,
    scales = "free_y",
    labeller = labeller(time = label_both)
  ) +
  scale_color_viridis_c(
    name   = expression(bold(alpha[dp])),
    option = "D"
  ) +
  labs(
    title = "State Means Across Interventions at Key Time Points",
    x     = "Intervention Strength",
    y     = "Mean State Value"
  ) +
  theme_bw(base_size = 20, base_family = "Palatino") +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black", size = 0.5),
    strip.text       = element_text(face = "bold", size = 18),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    legend.key.width = unit(1, "null"),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black", linewidth = 0.3),
    plot.margin      = margin(10, 20, 10, 20)
  )




# ======================================
# 3. Full Trajectories with Intervention & Recovery
# ======================================
# Setup parameters 
selected_adps <- c(min(alpha_dp_grid), max(alpha_dp_grid))
N_traj <- 1000
T <- 3000                
dt <- 0.01
intervention_start <- 300    
intervention_end   <- 1500   
recovery_time      <- intervention_end * dt


# Simulate trajectories with intervention & recovery for selected alphaDP
set.seed(123)
traj_df <- map_dfr(selected_adps, function(adp) {
  s_vals <- sample(clust_data$S.fin, N_traj, replace = TRUE)
  map2_dfr(seq_along(s_vals), s_vals, function(i, s0) {
    sim <- simulate_with_recovery(adp, s0, T, dt, intervention_start, intervention_end,
                                  varD, varP, varS, covDP, covDS, covPS)
    sim$ID <- i
    sim$alpha_dp <- adp  # Use actual alpha_dp value
    sim
  })
}) %>%
  pivot_longer(cols=c(D,P), names_to="Variable", values_to="value") 

# Summarize mean curves
summary_traj <- traj_df %>%
  group_by(alpha_dp, time, Variable) %>%
  summarize(mean = mean(value), sd = sd(value), .groups = "drop")

# Color palette for D and P
vars_colors <- c("D" = "#E69F00", "P" = "steelblue4")

# Plot trajectory 
traj_plot <- ggplot() +
  # Individual trajectories
  geom_line(data = traj_df_filtered,
            aes(x = time, y = value, group = interaction(ID, Variable), color = Variable),
            alpha = 0.01, linewidth = 0.2) +
  
  # Mean trajectories
  geom_line(data = summary_traj,
            aes(x = time, y = mean, color = Variable),
            linewidth = 0.8) +
  
  # Intervention ON/OFF lines
  geom_vline(xintercept = intervention_start, linetype = "dashed") +
  geom_vline(xintercept = intervention_end,   linetype = "dashed") +
  annotate("text", x = intervention_start, y = Inf, label = "Int. ON",
           vjust = 2, size = 5) +
  annotate("text", x = intervention_end, y = Inf, label = "Int. OFF",
           vjust = 2, size = 5) +
  
  # Facet by α_dp extremes
  facet_wrap(~ alpha_dp, labeller = label_bquote(alpha[dp] == .(alpha_dp)), nrow = 2) +
  
  scale_color_manual(name = "Variable", values = vars_colors) +
  
  scale_x_continuous(
    breaks = c(0, intervention_start, intervention_end, T),
    labels = c("0", intervention_start, intervention_end, T)
  ) +
  
  labs(
    title    = "Dynamic Response & Recovery Under Extreme Feedback",
    subtitle = "Dashed lines = Intervention ON / OFF",
    x        = "Time (steps)",
    y        = "State Level"
  ) +
  ylim(-1,1) + # for clarity of plot we limit the y-axis scale
  theme_bw(base_size = 20, base_family = "Palatino") +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black", size = 0.5),
    strip.text = element_text(face = "bold", size = 18),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
    legend.position = "bottom",
    panel.grid = element_blank()
  )


# ======================================
# 4. Combine All Figures
# ======================================
combined_fig <- snapshot_plot + traj_plot +
  plot_layout(nrow = 2, heights = c(1, 1)) +
  plot_annotation(
    tag_levels = "A",
  ) & theme(
    plot.tag          = element_text(face = "bold", size = 20),
    plot.title        = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle     = element_text(size = 14, hjust = 0.5),
    plot.margin       = margin(10, 10, 10, 10),
    legend.title      = element_text(face = "bold")
  )

combined_fig
#ggsave("intervention_linear.pdf", combined_fig, width = 11, height = 17)
