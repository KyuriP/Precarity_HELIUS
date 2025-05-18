## =============================================================================
## Script: 02_simulate_intervention.R
## Simulate D–P System Dynamics Under External Stress Interventions
## =============================================================================
# Description:
# This script runs simulations of the depression (D) and precariousness (P) system
# under varying levels of stress reduction (interventions on S). It uses top 
# parameter sets previously identified via multi-objective optimization, simulates
# post-intervention trajectories, and saves both aggregate trends and detailed 
# outcomes for selected candidates (Sol_1 and Sol_21).
## =============================================================================

# parallel processing
plan(multisession)

# ------------------------------------------------------------------------------
# 1. Load and Annotate Top Parameter Sets
# ------------------------------------------------------------------------------

top_solutions <- readRDS("Precarity_HELIUS/data/intervention/top_solutions_nsga2.rds")

# Add derived features for parameter diagnostics
param_sets_top <- top_solutions %>%
  mutate(
    ParamID           = paste0("Sol_", row_number()),
    feedback_strength = beta2 * alpha2,
    external_strength = beta1 * alpha1,
    param_strength    = beta1 * beta2 * alpha1 * alpha2,
    feedback_ratio    = feedback_strength / external_strength,
    sigmasize         = sigma1 + sigma2,
    bistability_index = feedback_strength / sigmasize
  )

# Identify candidates for case-study plots
bistable_candidate  <- param_sets_top %>% slice_max(bistability_index, n = 2)
unistable_candidate <- param_sets_top %>% slice_min(bistability_index, n = 2)

# Extract full parameter matrix and specific candidates
param_sets_all <- param_sets_top %>% select(ParamID, beta1:sigma2)
param_sets_candidates <- list(
  "Sol_1"  = unlist(param_sets_top %>% filter(ParamID == "Sol_1")  %>% select(beta1:sigma2)),
  "Sol_21" = unlist(param_sets_top %>% filter(ParamID == "Sol_21") %>% select(beta1:sigma2))
)

# ------------------------------------------------------------------------------
# 2. Define Simulation Settings and Function
# ------------------------------------------------------------------------------

intervention_sizes <- seq(0, 1, by = 0.01)
set.seed(42)
S_vals <- sample(depsum$S.fin, 300, replace = TRUE)

# Simulates the final D and P values for each S value and intervention strength
simulate_final_D_P <- function(params, S_vals, shift_frac, T_post = 1000, dt = 0.01) {
  lambda_D <- lambda_P <- 1
  S_min <- min(S_vals)
  S_shifted <- S_vals - shift_frac * (S_vals - S_min)
  
  map2_dfr(S_vals, S_shifted, function(S0, S1) {
    D <- P <- numeric(T_post)
    for (t in 2:T_post) {
      dW1 <- rnorm(1, 0, sqrt(dt))
      dW2 <- rnorm(1, 0, sqrt(dt))
      D[t] <- D[t-1] + lambda_D * (tanh(params["beta1"] * S1 + params["beta2"] * P[t-1]) - D[t-1]) * dt + params["sigma2"] * dW2
      P[t] <- P[t-1] + lambda_P * (tanh(params["alpha1"] * S1 + params["alpha2"] * D[t-1]) - P[t-1]) * dt + params["sigma1"] * dW1
    }
    tibble(D_final = D[T_post], P_final = P[T_post])
  })
}

# ------------------------------------------------------------------------------
# 3. Simulate Aggregate Effects Across Top Parameter Sets
# ------------------------------------------------------------------------------

results_DP <- map_dfr(1:nrow(param_sets_top), function(i) {
  row <- param_sets_top[i, ]
  params <- unlist(row[c("beta1", "beta2", "alpha1", "alpha2", "sigma1", "sigma2")])
  
  map_dfr(intervention_sizes, function(shift) {
    sims <- simulate_final_D_P(params, S_vals, shift)
    tibble(
      ParamID        = row$ParamID,
      Intervention   = shift,
      Mean_D         = mean(sims$D_final),
      Mean_P         = mean(sims$P_final),
      param_strength = row$param_strength
    )
  })
})

# Add feedback metrics for downstream analysis
results_DP1 <- results_DP %>%
  left_join(param_sets_top %>% select(ParamID, feedback_strength, param_strength), by = "ParamID")

# ------------------------------------------------------------------------------
# 4. Simulate Detailed Outcomes for Selected Candidates
# ------------------------------------------------------------------------------

final_data <- map_dfr(names(param_sets_candidates), function(label) {
  params <- param_sets_candidates[[label]]
  
  map_dfr(intervention_sizes, function(shift) {
    sims <- simulate_final_D_P(params, S_vals, shift)
    sims$Intervention <- shift
    sims$Model <- label
    sims$ID <- seq_len(nrow(sims))
    sims
  })
})

# ------------------------------------------------------------------------------
# 5. Save Simulation Outputs
# ------------------------------------------------------------------------------

# saveRDS(results_DP1, "Precarity_HELIUS/data/intervention/simulated_intervention_top30.rds")
# saveRDS(final_data,   "Precarity_HELIUS/data/intervention/final_sol1_sol21.rds")


