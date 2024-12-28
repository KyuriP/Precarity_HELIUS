## =============================================================================
## Script for Causal Discovery and Subsampling Analysis
## on Depression *Symptom Data with RCoT test*
## =============================================================================
#' It runs causal discovery algorithms (PC, FCI, CCI) using multiple parameter combinations 
#' and saves the results for further analysis.
#'
#' @details
#' - Fixed edges and gaps are defined to speed up individual symptom analysis 
#'   and enforce specific constraints on the causal structure.
#' - The script supports parameterization of alpha levels, thresholds, and 
#'   CI tests (e.g., `RCoT`) to evaluate edge stability and causal graphs.
#' - Results for each algorithm and parameter set are saved in `.rds` format.
## =============================================================================

# ==============================================================================
# Load Helper Functions and Preprocessed Data
# ==============================================================================
source("helius/code/robust_func.R")       # Load helper functions
source("helius/code/preprocess_dat.R")   # Load and preprocess the dataset

# load helper functions and libraries
source("helius/code/robust_func.R")

# preprocess dataset
source("helius/code/preprocess_dat.R")



## =============================================================================
## Run Causal Discovery on Depression Sumscore Data
## =============================================================================

# Set parameters
args <- commandArgs(trailingOnly = TRUE)

alpha <- as.numeric(args[1]) # Significance level 
threshold <- as.numeric(args[2]) # Threshold for stable edges 

data <- depsym
subsample_size <- nrow(data)  # Size of each subsample
num_subsamples <- 30     # Number of subsamples 


## Set up fixed gaps and edges for individual symptom analysis (*speed up*)
# # Initialize a 14x14 matrix with FALSE for both fixedEdges and fixedGaps
# fixedEdges_full <- matrix(FALSE, nrow = 14, ncol = 14)
# fixedGaps_full <- matrix(FALSE, nrow = 14, ncol = 14)
# 
# # Define the constraints for the first 9 variables (fixedEdges)
# fixedEdges_full[1:9, 1:9] <- matrix(
#   c(
#     FALSE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, FALSE, FALSE,
#     TRUE, FALSE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE,
#     TRUE,  TRUE, FALSE,  TRUE,  TRUE, FALSE,  TRUE, FALSE, FALSE,
#     TRUE,  TRUE,  TRUE, FALSE,  TRUE, FALSE,  TRUE, FALSE, FALSE,
#     TRUE, FALSE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, FALSE,
#     FALSE,  TRUE, FALSE, FALSE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE,
#     TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, FALSE,
#     FALSE,  TRUE, FALSE, FALSE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE,
#     FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE, FALSE,  TRUE, FALSE
#   ),
#   nrow = 9, byrow = TRUE
# )
# 
# # Define the constraints for the first 9 variables (fixedGaps)
# fixedGaps_full[1:9, 1:9] <- matrix(
#   c(
#     FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, FALSE,  TRUE,  TRUE,
#     FALSE, FALSE, FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE, FALSE,
#     FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, FALSE,  TRUE,  TRUE,
#     FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, FALSE,  TRUE,  TRUE,
#     FALSE,  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE,
#     TRUE, FALSE,  TRUE,  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
#     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE,
#     TRUE, FALSE,  TRUE,  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
#     TRUE, FALSE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, FALSE, FALSE
#   ),
#   nrow = 9, byrow = TRUE
# )

# Run with RCoT
pc_result <- causal_subsampling(data, algorithm = "PC", subsample_size = subsample_size, num_subsamples = num_subsamples, alpha = alpha, threshold = threshold, citest = "RCoT", fixedEdges = fixedEdges_full, fixedGaps = fixedGaps_full)
saveRDS(pc_result, file = sprintf("helius/data/rcotPC_alpha_%s.rds", alpha))

fci_result <- causal_subsampling(data, algorithm = "FCI", subsample_size = subsample_size, num_subsamples = num_subsamples, alpha = alpha, threshold = threshold, citest = "RCoT", fixedEdges = fixedEdges_full, fixedGaps = fixedGaps_full)
saveRDS(fci_result, file = sprintf("helius/data/rcotFCI_alpha_%s_threshold_%s.rds", alpha, threshold))

cci_result <- causal_subsampling(data, algorithm = "CCI", subsample_size = subsample_size, num_subsamples = num_subsamples, alpha = alpha, threshold = threshold, citest = "RCoT", fixedEdges = fixedEdges_full, fixedGaps = fixedGaps_full)
saveRDS(cci_result, file = sprintf("helius/data/rcotCCI_alpha_%s_threshold_%s.rds", alpha, threshold))


