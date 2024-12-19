## =============================================================================
## Script for Processing and Visualizing Causal Discovery Results
## =============================================================================
#' This script processes causal discovery results, generates summaries of stable 
#' edges, compares results across algorithms and parameters, and visualizes the 
#' summarized causal graphs. 
#'
#' @details
#' - Reads causal discovery results stored in `.rds` files.
#' - Summarizes stable edges from multiple matrices using the most frequent 
#'   value for each element.
#' - Identifies shared and differing edges between adjacency matrices.
## =============================================================================


## Generate file names 
# all conditions
algorithm <- c("PC", "FCI", "CCI")
alpha <- c(0.01, 0.05)
threshold <- c(0.5, 0.6, 0.7, 0.8)
citest <- c("gaussCItest", "RCoT")
comb <- expand.grid(algorithm = algorithm, alpha = alpha, threshold = threshold, citest = citest)
# sum score data
filenames <- glue::glue(
    "{comb$algorithm}_alpha_{sprintf('%.2f', comb$alpha)}_threshold_{sprintf('%.1f', comb$threshold)}_citest_{comb$citest}.rds"
  )
# # gaussian sym test data
# filenames <- glue::glue(
#   "gaussian{comb$algorithm}_alpha_{sprintf('%.2f', comb$alpha)}_threshold_{sprintf('%.1f', comb$threshold)}.rds"
# )

# directory containing the files
directory <- "data/dep_sumscore"  

# Full paths to the files
full_paths <- file.path(directory, filenames)

# Read all .rds files into a list
rds_data <- lapply(full_paths, readRDS)

# Name the datasets for easy access
names(rds_data) <- filenames

# Extract 'stable_edges' matrices from each list
stable_edges_data <- lapply(rds_data, function(x) x$stable_edges)

# Group matrices by algorithm
grouped_stable_edges <- split(stable_edges_data, comb$algorithm)


summarized_stable_edges <- lapply(grouped_stable_edges, function(group) {
  # Check dimensions from the first matrix in the group
  nrow <- nrow(group[[1]])
  ncol <- ncol(group[[1]])
  
  # Create a matrix to store the most frequent element for each position
  summarized <- matrix(NA, nrow = nrow, ncol = ncol, 
                       dimnames = dimnames(group[[1]]))  # Preserve row and column names
  
  # Iterate through each cell in the matrices
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      # Collect all values for the same position (i, j) across all matrices in the group
      elements <- sapply(group, function(mat) mat[i, j])
      
      # Find the most frequent value for this position
      summarized[i, j] <- as.numeric(names(sort(table(elements), decreasing = TRUE))[1])
    }
  }
  return(summarized)  # Return the summarized matrix for this group
})



# Compare matrices
fcimat <- summarized_stable_edges$FCI 
ccimat <- summarized_stable_edges$CCI

shared_edges <- which(fcimat == ccimat, arr.ind = TRUE)
diff_edges <- which(fcimat != ccimat, arr.ind = TRUE)

cat("Shared Edges:\n")
print(shared_edges)

cat("\nDiffering Edges:\n")
print(diff_edges)



