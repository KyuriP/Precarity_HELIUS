
# parallel processing
plan(multisession)  

# NSGA2 - multioptimization
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
  
  # Compare to real data (depsum)
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

# ======= 1. Setup =======
library(tidyverse)
library(furrr)
library(reshape2)
library(fmsb)

library(nsga2R)


result2 <- nsga2R::nsga2R(
  fn = multi_obj_fun,
  varNo = 6,                   # Number of parameters
  objDim = 5,                  # Number of objectives
  lowerBounds = rep(0, 6),
  upperBounds = rep(3, 6),
  popSize = 500,
  generations = 1000,
  cprob = 0.9,                 # Crossover probability
  mprob = 1 / 4,               # Mutation probability
  tourSize = 2               # Tournament size
)

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

saveRDS(result7, "multioptim_result_6var7optim_500pop_fixedS.rds")


