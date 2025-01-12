## =============================================================================
## Script for Causal Discovery and Subsampling Analysis
## on Depression *Symptom Data with Gaussian CI Test*
## =============================================================================
#' It runs causal discovery algorithms (PC, FCI, CCI) using multiple parameter combinations 
#' and saves the results for further analysis.
#'
#' @details
#' - Causal discovery algorithms are applied to subsampled data using combinations 
#'   of alpha levels, and thresholds. (CItest = `gaussCItest`)
#' - Constraints on edges and gaps (fixedEdges and fixedGaps) are defined to 
#'   refine the analysis for symptom-level data.
#' - Results are saved in `.rds` format for both sum score and individual 
#'   symptom analyses.
## =============================================================================

# load helper functions and libraries
source("helius/code/robust_func.R")

# preprocess dataset
source("helius/code/preprocess_dat.R")

# ==============================================================================
# Causal Discovery: Define Parameters and Run Analyses
# ==============================================================================
# Set parameters
# args <- commandArgs(trailingOnly = TRUE)
# 
# alpha <- as.numeric(args[1]) # Significance level 
# threshold <- as.numeric(args[2]) # Threshold for stable edges 
alphas = c(0.01, 0.05)
thresholds = c(0.6, 0.7)
citests = "gaussCItest"
algorithms = c("PC", "FCI", "CCI")
data <- dep_sym # sumscore 
subsample_size <- nrow(data)  # Size of each subsample
num_subsamples <- 30     # Number of subsamples


## Set up fixed gaps and edges for individual symptom analysis *speed up*
## using fixed gaps and edges
# Initialize a 14x14 matrix with FALSE for both fixedEdges and fixedGaps
fixedEdges_full <- matrix(FALSE, nrow = 14, ncol = 14)
fixedGaps_full <- matrix(FALSE, nrow = 14, ncol = 14)

# Define the constraints for the first 9 variables (fixedEdges)
fixedEdges_full[1:9, 1:9] <- matrix(c(
  FALSE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, FALSE, # anh
  TRUE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, # dep
  TRUE,  TRUE, FALSE,  TRUE,  TRUE, TRUE,  TRUE,  TRUE, FALSE, # slp
  TRUE,  TRUE,  TRUE, FALSE,  TRUE, FALSE,  TRUE,  TRUE, FALSE, # ene
  TRUE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, FALSE, # app
  TRUE,  TRUE,  TRUE, FALSE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, # glt
  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, # con
  FALSE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, # mot
  FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE,  TRUE,  TRUE, FALSE  # sui
), nrow = 9, byrow = TRUE)


# Define the constraints for the first 9 variables (fixedGaps)
fixedGaps_full[1:9, 1:9] <- matrix(c(
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE,  TRUE, # anh
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, # dep
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, # slp
  FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, FALSE, FALSE,  TRUE, # ene
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, # app
  FALSE, FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, # glt
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE, FALSE, # con
  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE, # mot
  TRUE, FALSE,  TRUE,  TRUE,  TRUE, FALSE, FALSE, FALSE,  FALSE  # sui
), nrow = 9, byrow = TRUE)


# Create a data frame of all parameter combinations
params <- tidyr::expand_grid(alpha = alphas, threshold = thresholds, citest = citests, algorithm = algorithms)


# Setup parallel backend
plan(multisession)
message("Number of parallel workers: ", nbrOfWorkers())

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
    citest = citest,
    fixedEdges = fixedEdges_full,
    fixedGaps = fixedGaps_full
  )
  
  # Create file name
  file_name <- glue::glue("data/symptom_gaussian2/{algorithm}_alpha_{alpha}_threshold_{threshold}_citest_{citest}.rds")
  saveRDS(result, file = file_name)
  
  # Print a message to track progress
  message(glue::glue("Completed: algorithm = {algorithm}, alpha = {alpha}, threshold = {threshold}, citest = {citest}"))
}

# Apply the function to each row in the parameter data frame
pwalk(params, run_and_save, .progress = TRUE)


# # Run with GaussianCI
# pc_result <- causal_subsampling(data, algorithm = "PC", subsample_size = subsample_size, num_subsamples = num_subsamples, alpha = alpha, threshold = threshold, citest = "gaussCItest", fixedEdges = fixedEdges_full, fixedGaps = fixedGaps_full)
# saveRDS(pc_result, file = sprintf("data/sym_gaussian2/PC_alpha_%s_threshold_%s_citest_gaussCI.rds", alpha, threshold))
# 
# fci_result <- causal_subsampling(data, algorithm = "FCI", subsample_size = subsample_size, num_subsamples = num_subsamples, alpha = alpha, threshold = threshold, citest = "gaussCItest", fixedEdges = fixedEdges_full, fixedGaps = fixedGaps_full)
# saveRDS(fci_result, file = sprintf("data/sym_gaussian2/FCI_alpha_%s_threshold_%s_citest_gaussCI.rds", alpha, threshold))
# 
# cci_result <- causal_subsampling(data, algorithm = "CCI", subsample_size = subsample_size, num_subsamples = num_subsamples, alpha = alpha, threshold = threshold, citest = "gaussCItest", fixedEdges = fixedEdges_full, fixedGaps = fixedGaps_full)
# saveRDS(cci_result, file = sprintf("data/sym_gaussian2/CCI_alpha_%s_threshold_%s_citest_gaussCI.rds", alpha, threshold))



