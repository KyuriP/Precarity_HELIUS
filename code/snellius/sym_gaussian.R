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
args <- commandArgs(trailingOnly = TRUE)

alpha <- as.numeric(args[1]) # Significance level 
threshold <- as.numeric(args[2]) # Threshold for stable edges 

data <- depsym
subsample_size <- nrow(data) #10000  # Size of each subsample
num_subsamples <- 100    # Number of subsamples


## Set up fixed gaps and edges for individual symptom analysis *speed up*
## using fixed gaps and edges
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



# Run with GaussianCI
pc_result <- causal_subsampling(data, algorithm = "PC", subsample_size = subsample_size, num_subsamples = num_subsamples, alpha = alpha, threshold = threshold, citest = "gaussCItest", fixedEdges = fixedEdges_full, fixedGaps = fixedGaps_full)
saveRDS(pc_result, file = sprintf("helius/data/gaussianPC_alpha_%s_threshold_%s.rds", alpha, threshold))

fci_result <- causal_subsampling(data, algorithm = "FCI", subsample_size = subsample_size, num_subsamples = num_subsamples, alpha = alpha, threshold = threshold, citest = "gaussCItest", fixedEdges = fixedEdges_full, fixedGaps = fixedGaps_full)
saveRDS(fci_result, file = sprintf("helius/data/gaussianFCI_alpha_%s_threshold_%s.rds", alpha, threshold))

cci_result <- causal_subsampling(data, algorithm = "CCI", subsample_size = subsample_size, num_subsamples = num_subsamples, alpha = alpha, threshold = threshold, citest = "gaussCItest", fixedEdges = fixedEdges_full, fixedGaps = fixedGaps_full)
saveRDS(cci_result, file = sprintf("helius/data/gaussianCCI_alpha_%s_threshold_%s.rds", alpha, threshold))



