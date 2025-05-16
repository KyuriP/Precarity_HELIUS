## =============================================================================
## Script for data preprocessing and running causal discovery
## =============================================================================
#' This script loads the pre-processed dataset, and runs causal discovery 
#' algorithms (PC, FCI, CCI) under multiple parameter combinations and saves the 
#' results for further analysis.
#'
#' @details
#' - Causal discovery algorithms are applied on bootstrapped datasets using 
#'   combinations of alpha levels, thresholds, and CI tests.
#' - Results are saved in `.rds` format for both sum score and individual 
#'   symptom analyses.
## =============================================================================

# load preprocessed data
source("Precarity_HELIUS/code/utils/preprocess_dat.R") 


## =============================================================================
## Causal discovery: run analysis under each setup
## =============================================================================

# Define parameters
alphas = c(0.01, 0.05)
thresholds = c(0.5, 0.6, 0.7, 0.8)
citests = c("gaussCItest", "RCoT")
algorithms = c("PC", "FCI", "CCI")
data <- agg_data #depsum # sumscore 
# data <- depsym # individual symptoms
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
  # sumscore version
  file_name <- glue::glue("data/aggregated_dat/{algorithm}_alpha_{alpha}_threshold_{threshold}_citest_{citest}.rds")
  # individual symptom version
  file_name <- glue::glue("data/aggregated_dat/{algorithm}_alpha_{alpha}_threshold_{threshold}_citest_{citest}.rds")
  saveRDS(result, file = file_name)
  
  # Print a message to track progress
  message(glue("Completed: algorithm = {algorithm}, alpha = {alpha}, threshold = {threshold}, citest = {citest}"))
}

# Dynamically determine available cores and set workers
available_cores <- parallel::detectCores() - 2  # Reserve some for the system
workers <- min(available_cores, 8)  # Use 16 workers, or the number of available cores

# Set the future plan
plan(multisession, workers = workers)
# Confirm the setup
cat("Using", workers, "parallel workers.\n")

# Apply the function to each row in the parameter data frame
pwalk(params, run_and_save, .progress = TRUE)
