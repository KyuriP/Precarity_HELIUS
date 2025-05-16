## =============================================================================
## Simulation-Based Multi-Objective Optimization for SDE Model
## =============================================================================
#' This script performs multi-objective calibration of a SDE model of 
#' depression (D) and precariousness (P), influenced by 
#' a fixed stressor (S). The model parameters are optimized using NSGA-II to 
#' match empirical summary statistics (means, variances, partial correlations).
## =============================================================================


# Function to compute partial correlation manually
partial_correlation <- function(x, y, z) {
  r_xy <- cor(x, y)  # Pearson correlation between x and y
  r_xz <- cor(x, z)  # Pearson correlation between x and z
  r_yz <- cor(y, z)  # Pearson correlation between y and z
  
  # Compute partial correlation using the formula
  r_xy_z <- (r_xy - r_xz * r_yz) / sqrt((1 - r_xz^2) * (1 - r_yz^2))
  
  return(r_xy_z)
}


# Function to simulate time trajectories given fixed S per individual
simulate_sde_fixed_S <- function(params, T = 100, dt = 0.01) {
  lambda_D <- lambda_P <- 1
  
  beta1  <- params[1]   # Effect of S on D
  beta2  <- params[2]   # Effect of P on D
  alpha1 <- params[3]   # Effect of S on P
  alpha2 <- params[4]   # Effect of D on P
  sigma1 <- params[5]   # Noise on P
  sigma2 <- params[6]   # Noise on D
  
  D <- numeric(T)
  P <- numeric(T)
  
  # Sample one S value from empirical dist for the whole trajectory
  S_val <- sample(depsum$S.fin, 1)
  S <- rep(S_val, T)
  
  for (t in 2:T) {
    dW1 <- rnorm(1, 0, sqrt(dt))
    dW2 <- rnorm(1, 0, sqrt(dt))
    
    P[t] <- P[t - 1] + lambda_P * (tanh(alpha1 * S[t] + alpha2 * D[t - 1]) - P[t - 1]) * dt + sigma1 * dW1    
    D[t] <- D[t - 1] + lambda_D * (tanh(beta1 * S[t] + beta2 * P[t - 1]) - D[t - 1]) * dt + sigma2 * dW2
    
  }
  
  return(data.frame(D = D, P = P, S = S))
}



# Multi-objective evaluation function 
multi_obj_fun_parallel <- function(params, N_runs = 1000) {
  # Simulate N_runs "individuals", each with one trajectory
  sims <- future_map(1:N_runs, ~ simulate_sde_fixed_S(params, T = 1000), .options = furrr_options(seed = TRUE))
  
  # Extract final time point (e.g., as if it's one observation per person)
  final_D_values <- map_dbl(sims, ~ tail(.x$D, 1))
  final_P_values <- map_dbl(sims, ~ tail(.x$P, 1))
  final_S_values <- map_dbl(sims, ~ tail(.x$S, 1))
  
  # Summary statistics across the simulated individuals
  mean_D <- mean(final_D_values)
  var_D  <- var(final_D_values)
  mean_P <- mean(final_P_values)
  var_P  <- var(final_P_values)
  
  # Partial correlations across simulated individuals
  pcor_DP <- partial_correlation(final_D_values, final_P_values, final_S_values)
  pcor_DS <- partial_correlation(final_D_values, final_S_values, final_P_values)
  pcor_PS <- partial_correlation(final_P_values, final_S_values, final_D_values)
  
  # Compare to real data (empirical targets)
  return(c(
    abs(mean_D - mean(depsum$PHQsum)),
    abs(var_D  - var(depsum$PHQsum)),
    abs(mean_P - mean(depsum$P.soc)),
    abs(var_P  - var(depsum$P.soc)),
    abs(pcor_DP - partial_correlation(depsum$PHQsum, depsum$P.soc, depsum$S.fin)),
    abs(pcor_DS - partial_correlation(depsum$PHQsum, depsum$S.fin, depsum$P.soc)),
    abs(pcor_PS - partial_correlation(depsum$P.soc, depsum$S.fin, depsum$PHQsum))
  ))
}


# Run NSGA-II
plan(multisession) # parallel processing


plan(multisession)
result7 <- nsga2R::nsga2R(
  fn = function(params) multi_obj_fun_parallel(params),  # Fix N_runs
  varNo = 6,                   # Number of parameters
  objDim = 7,                  # Number of objectives
  lowerBounds = rep(0, 6),
  upperBounds = rep(2, 6),
  popSize = 500,
  generations = 100,
  cprob = 0.9,                 # Crossover probability
  mprob = 1 / 4,               # Mutation probability
  tourSize = 2,               # Tournament size
  XoverDistIdx = 10,          # crossover index
  MuDistIdx = 20
)

# save result
# saveRDS(result7, "multioptim_result_6var7optim_500pop_fixedS.rds")


