## =============================================================================
## Linear Feedback SDE Model Analysis: Covariance-Based Calibration & Trajectory Dynamics
## =============================================================================
#' This script implements a stochastic linear feedback model of depression (D) and
#' precarity (P), influenced by stressor (S), using empirical covariances for calibration.
#'
#' 0. Load and calculate empirical covariance structure from ‘clust_data’.
#' 1. Estimate α₍dp₎ by minimizing discrepancy between modeled and observed covariances.
#' 2. Analyze parameter trade-offs across a grid of α₍dp₎ values with annotated plots.
#' 3. Run snapshot simulations at key time points and visualize mean responses to interventions.
#' 4. Simulate full trajectories under “intervention ON/OFF” and examine recovery dynamics
#'     with both individual paths and averaged curves.
#' 5. Assemble all visuals into a combined multi-panel figure.
## =============================================================================

# Load data & libraries
source("Precarity_HELIUS/code/utils/preprocess_dat.R") 

set.seed(123) # set seed

# ========== 0. Data & Covariance Setup ==========
init_covars <- function(clust_data) {
  list(
    varD  = 1,
    varP  = var(clust_data$P.soc),
    varS  = var(clust_data$S.fin),
    covDP = cov(clust_data$PHQsum, clust_data$P.soc),
    covDS = cov(clust_data$PHQsum, clust_data$S.fin),
    covPS = cov(clust_data$P.soc,   clust_data$S.fin)
  )
}
covs <- init_covars(clust_data)

# ========== 1. Covariance → α₍dp₎ & Parameter Estimation ==========
get_model_params <- function(alpha_dp, covs) {
  with(covs, {
    αds <- (-alpha_dp * covPS + covDS) / varS
    αps <- (-covDS^2 * covPS + covDS * (alpha_dp * covPS^2 - alpha_dp * varP * varS + 2*covDP*varS) - covPS * varD * varS) /
      (varS*(covDS^2 - varD*varS))
    αpd <- (-alpha_dp * covPS^2 + alpha_dp * varP * varS - 2*covDP*varS + 2*covDS*covPS) /
      (covDS^2 - varD*varS)
    σd2 <- ((alpha_dp*covPS - covDS)^2) / varS
    σp2 <- ((alpha_dp*varS   - covDP)^2) / varS
    list(alpha_ds = αds, alpha_ps = αps, alpha_pd = αpd,
         sigma_d = sqrt(σd2), sigma_p = sqrt(σp2))
  })
}

estimate_alpha_dp <- function(covs) {
  loss <- function(adp) {
    p <- get_model_params(adp, covs)
    ests <- c(p$alpha_ds * covs$varS + adp * covs$covPS,
              p$alpha_ps * covs$varS + p$alpha_pd * covs$covDS,
              adp * covs$varS)
    sum((ests - c(covs$covDS, covs$covPS, covs$covDP))^2)
  }
  optimize(loss, interval = c(0, 1))$minimum
}
αdp_est <- estimate_alpha_dp(covs)

# ========== 2. Parameter Tradeoff Plot ==========
αdp_grid <- seq(0, 0.698379, by = 0.05)

param_grid <- map_dfr(αdp_grid, ~ {
  p <- get_model_params(.x, covs)
  tibble(alpha_dp = .x, alpha_ds = p$alpha_ds, alpha_ps = p$alpha_ps,
         alpha_pd = p$alpha_pd, sigma_d = p$sigma_d, sigma_p = p$sigma_p)
})

# Assuming param_grid is already created
param_labels <- c(
  alpha_ds = "alpha[ds]",
  alpha_pd = "alpha[pd]",
  alpha_ps = "alpha[ps]",
  sigma_d  = "sigma[D]",
  sigma_p  = "sigma[P]"
)

# Create the plot
param_plot <- param_grid %>%
  pivot_longer(-alpha_dp, names_to = "param") %>%
  ggplot(aes(x = alpha_dp, y = value, color = param)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(
    ~ param,
    #scales = "free_y",
    labeller = as_labeller(param_labels, default = label_parsed)
  ) +
  scale_color_viridis_d(option = "D") +
  labs(
    title = expression(bold("Parameter trade-offs as a function of ")~alpha[dp]),
    x = expression(alpha[dp]),
    y = "Parameter value"
  ) +
  theme_bw(base_size = 14, base_family = "Palatino") +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    axis.line  = element_line(color = "black"),
    panel.grid = element_blank()
  )

param_plot

# ggsave("param_tradeoff.pdf", param_plot, width=9, height=7)

# ========== 3. Snapshot Simulations at Key Times ==========
simulate_traj <- function(adp, s0, T, dt, intervention_time, covs) {
  P <- get_model_params(adp, covs)
  D <- numeric(T); P_ <- numeric(T); S <- rep(s0, T)
  for (t in 2:T) {
    dW1 <- rnorm(1, 0, sqrt(dt)); dW2 <- rnorm(1, 0, sqrt(dt))
    d <- P$alpha_ds * S[t] + adp * P_[t-1] - D[t-1]
    p <- P$alpha_ps * S[t] + P$alpha_pd * D[t-1] - P_[t-1]
    D[t] <- D[t-1] + d * dt + P$sigma_d * dW1
    P_[t] <- P_[t-1] + p * dt + P$sigma_p * dW2
  }
  tibble(time = seq(1, by=1, length.out=T), D = D, P = P_, S = S)
}

set.seed(123); plan(multisession, workers = 6)
N <- 500
interventions <- seq(0, 1, by = 0.01)
Tmax <- 1000; dt <- 0.01; int_time <- 200
times_filter <- c(100, 300, 500)  # raw time units

SnapshotChunk <- function(adps) {
  expand.grid(alpha_dp=adps, intervention=interventions, rep=1:N) %>%
    mutate(s0 = sample(clust_data$S.fin, n(), TRUE),
           minS = min(clust_data$S.fin),
           s_adj = (1 - intervention) * s0 + intervention * minS) %>%
    pmap_dfr(function(alpha_dp, intervention, rep, s0, minS, s_adj) {
      simulate_traj(alpha_dp, s_adj, Tmax, dt, int_time, covs) %>%
        filter(time %in% times_filter) %>%
        pivot_longer(D:P, names_to="Variable", values_to="value") %>%
        mutate(intervention, alpha_dp)
    }, .options = furrr_options(seed=TRUE))
}
chunks <- split(αdp_grid, ceiling(seq_along(αdp_grid)/3))
snapshots <- map_dfr(chunks, SnapshotChunk) |> plan(sequential)

snapshot_summary <- snapshots %>%
  group_by(alpha_dp, intervention, time, Variable) %>%
  summarize(mean = mean(value), .groups = "drop")
snapshot_summary$time <- factor(snapshot_summary$time, levels=times_filter,
                                labels = paste("Time =", times_filter))


snapshot_plot <- ggplot(snapshot_summary, aes(intervention, mean, color=alpha_dp, group=alpha_dp)) +
  geom_line(size=0.6) +
  facet_grid(Variable ~ time, scales="free_y") +
  scale_color_viridis_c(name = expression(bold(alpha[dp]))) +
  theme_bw(base_size=13) +
  labs(title="Response Snapshots at Key Times", x="Intervention Strength", y="Mean State")

# ========== 4. Intervention + Recovery Trajectories ==========
simulate_recovery <- function(adp, s0, T, dt, t_on, t_off, covs) {
  Pm <- get_model_params(adp, covs)
  minS <- min(clust_data$S.fin)
  D <- numeric(T); P_ <- numeric(T); S <- numeric(T)
  D[1] <- P_[1] <- 0
  for (t in seq(2, T)) {
    S[t] <- ifelse(t < t_on, s0,
                   ifelse(t <= t_off, minS, s0))
    dW1 <- rnorm(1)*sqrt(dt); dW2 <- rnorm(1)*sqrt(dt)
    driftD <- Pm$alpha_ds*S[t] + adp * P_[t-1] - D[t-1]
    driftP <- Pm$alpha_ps*S[t] + Pm$alpha_pd*D[t-1] - P_[t-1]
    D[t] <- D[t-1] + driftD*dt + Pm$sigma_d*dW1
    P_[t] <- P_[t-1] + driftP*dt + Pm$sigma_p*dW2
  }
  tibble(time = 1:T, D = D, P = P_, S = S, alpha_dp = adp)
}

set.seed(123)
sel_adps <- c(min(αdp_grid), max(αdp_grid))
recs <- map_dfr(sel_adps, ~ {
  s0s <- sample(clust_data$S.fin, 300, TRUE)
  map_dfr(s0s, ~ simulate_recovery(.x, .y, 3000, 0.01, 300, 1500, covs))
})
reclong <- recs |> pivot_longer(D:P, names_to="Variable", values_to="value")
recsum <- reclong %>%
  group_by(alpha_dp, time, Variable) %>%
  summarise(mean = mean(value), .groups = "drop")

traj_plot <- ggplot() +
  geom_line(data=reclong, aes(time, value, color=Variable, group=interaction(alpha_dp, regexp_replace(ID, "-", ""))),
            alpha=0.05, size=0.2) +
  geom_line(data=recsum, aes(time, mean, color=Variable), size=0.8) +
  facet_wrap(~alpha_dp, labeller=label_bquote(alpha[dp] == .(alpha_dp)), nrow=2) +
  geom_vline(xintercept = c(300, 1500), linetype="dashed") +
  scale_color_manual(name="State", values = c(D="#E69F00", P="steelblue4")) +
  labs(title="Trajectories with Intervention & Recovery", x="Time (steps)", y="State Level") +
  theme_bw(base_size=13)

# ========== 5. Combine & Save ==========
combined <- (param_plot + snapshot_plot + traj_plot) +
  plot_layout(nrow=3) +
  plot_annotation(title="Linear Feedback Model Analysis", tag_levels="A") &
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        plot.tag   = element_text(size=14, face="bold"),
        legend.title = element_text(face="bold"))

# ggsave("full_analysis_figure.pdf", combined, width=12, height=18)

