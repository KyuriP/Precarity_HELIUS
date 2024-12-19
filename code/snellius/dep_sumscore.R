## =============================================================================
## Script for Causal Discovery and Subsampling Analysis 
## on Depression SUM score
## =============================================================================
#' This script preprocesses a dataset, scales variables, and clusters data 
#' related to precariousness and depression symptoms. It performs causal 
#' discovery using various algorithms (PC, FCI, CCI) and subsampling approaches.
#'
#' @details
#' - The script runs causal discovery algorithms on subsampled data 
#'   using different parameters (e.g., alpha, thresholds, CI tests).
#' - Results are saved in `.rds` format for further analysis.
## =============================================================================


# load helper functions and libraries
source("helius/code/robust_func.R")

# preprocess dataset
source("helius/code/preprocess_dat.R")



## =============================================================================
## Run Causal Discovery on Depression Sumscore Data
## =============================================================================

# Define parameters
alphas = c(0.01, 0.05)
thresholds = c(0.5, 0.6, 0.7, 0.8)
citests = c("gaussCItest", "RCoT")
algorithms = c("PC", "FCI", "CCI")
data <- depsum # sumscore 
subsample_size <- nrow(data)  # Size of each subsample
num_subsamples <- 100     # Number of subsamples


# Create a data frame of all parameter combinations
params <- tidyr::expand_grid(alpha = alphas, threshold = thresholds, citest = citests, algorithm = algorithms)

# Function to run and save results for each combination
run_and_save <- function(alpha, threshold, citest, algorithm) {
  # Run causal_subsampling for the given parameters
  result <- causal_subsampling(
    data,
    algorithm = algorithm,
    subsample_size = subsample_size,
    num_subsamples = num_subsamples,
    alpha = alpha,
    threshold = threshold,
    citest = citest
  )
  
  # Create file name
  file_name <- glue::glue("dat_sumscore/{algorithm}_alpha_{alpha}_threshold_{threshold}_citest_{citest}.rds")
  saveRDS(result, file = file_name)
  
  # Print a message to track progress
  message(glue("Completed: algorithm = {algorithm}, alpha = {alpha}, threshold = {threshold}, citest = {citest}"))
}

# Apply the function to each row in the parameter data frame
pwalk(params, run_and_save, .progress = TRUE)
