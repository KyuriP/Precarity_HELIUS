
## =============================================================================
## Causal Discovery and Stable Edge Aggregation Script
## =============================================================================
#' Perform causal discovery on subsampled data and aggregate results
#'
#' @description
#' This script is designed to run causal discovery algorithms (e.g., FCI, PC, CCI) 
#' on subsampled data, leveraging parallel processing to improve efficiency. 
#' It identifies stable edges by aggregating adjacency matrices across multiple 
#' subsamples and outputs a final stable edge matrix along with edge type counts.
#'
#' @details
#' Supported algorithms:
#' - "PC": Uses partial correlation tests for causal discovery.
#' - "FCI": Handles latent confounders and outputs a PAG.
#' - "CCI": A variant that incorporates specific improvements over FCI.
#'
#' Supported CI tests:
#' - "gaussCItest": Gaussian conditional independence test.
#' - "RCoT": Kernel-based test for non-linear dependencies.
#'
#' Adjacency matrix edge types:
#' - "0": No edge
#' - "1": Circle (uncertain direction)
#' - "2": Arrowhead (directed edge)
#' - "3": Tail
#'
#' Parallelization:
#' The script utilizes the `future` and `furrr` packages for parallel processing.
#'
#' @return
#' - `stable_edges`: A matrix representing stable edges across subsamples.
#' - `edge_type_count`: A list of matrices counting occurrences of each edge type.
## =============================================================================



# Load necessary packages
source("Precarity_HELIUS/code/utils/libraries.R")

# Setup parallel backend
plan(multicore); 
message("Number of parallel workers: ", nbrOfWorkers())



# General function to run a causal discovery algorithm on a subsample
run_causal_subsample <- function(data_subsample, algorithm, citest, alpha, num_f = 100, num_f2 = 10, fixedEdges = NULL, fixedGaps = NULL) {
  message(sprintf("Running causal discovery for algorithm: %s, CI test: %s, alpha: %f", algorithm, citest, alpha))
  
  # Specify suffStat based on the CImethod
  suffStat <- if (citest  == "RCoT") {
    list(data = data_subsample, num_f = num_f, num_f2 = num_f2)
  } else if (citest == "gaussCItest") {
    list(C = cor(data_subsample), n = nrow(data_subsample))
  } else {
    stop("Unsupported CI test specified.")
  }
  
  # Select the CI test function 
  indepTest <- switch(citest,
                      "gaussCItest" = pcalg::gaussCItest,
                      "RCoT" = RCIT::RCoT,
                      stop("Unsupported CI test specified.")
  )
  
  # Run the selected causal discovery algorithm
  message("Estimating causal graph...")
  result <- switch(algorithm,
                   "FCI" = pcalg::fci(suffStat = suffStat, indepTest = indepTest,
                                      labels = colnames(data_subsample), alpha = alpha, selectionBias = FALSE, fixedGaps = fixedGaps, fixedEdges = fixedEdges, skel.method = "stable.fast", numCores = nbrOfWorkers()),
                   "PC" = pcalg::pc(suffStat = suffStat, indepTest = indepTest,
                                    labels = colnames(data_subsample), alpha = alpha, fixedGaps = fixedGaps, fixedEdges = fixedEdges, skel.method = "stable.fast", numCores = nbrOfWorkers()),
                   "CCI" = CCI.KP::cci(suffStat = suffStat, indepTest = indepTest,
                                       labels = colnames(data_subsample), alpha = alpha, p = ncol(data_subsample), fixedGaps = fixedGaps, fixedEdges = fixedEdges, skel.method = "stable.fast", numCores = nbrOfWorkers()),
                   stop("Unsupported algorithm specified.")
  )
  message("Causal graph estimated successfully.")
  
  
  # Extract adjacency matrix depending on algorithm type
  if (algorithm == "FCI") {
    adjacency_matrix <- result@amat
  } else if (algorithm == "CCI") {
    adjacency_matrix <- result$maag
  } else if (algorithm == "PC"){
    adjacency_matrix <- result@graph |> as("matrix")
  }
  
  return(adjacency_matrix)
}

# Function to perform subsampling, run causal discovery, and aggregate results
causal_subsampling <- function(data, algorithm, subsample_size, num_subsamples, threshold, citest, alpha, num_f = 100, num_f2 = 10, fixedGaps = NULL, fixedEdges = NULL) {
  
  num_variables <- ncol(data)
  
  # Subsampling and applying causal discovery with furrr::future_map for parallel processing
  adjacency_matrices <- furrr::future_map(
    1:num_subsamples, 
    ~ {
      library(RCIT)
      library(pcalg)
      
      data_subsample <- dplyr::sample_n(data, subsample_size, replace = TRUE)
      message(sprintf("Processing subsample: %s", .x))
      
      run_causal_subsample(data_subsample, algorithm = algorithm, alpha = alpha, citest = citest, num_f = num_f, num_f2 = num_f2, fixedGaps = fixedGaps, fixedEdges = fixedEdges)
    },
    .progress = TRUE, #snellius too many bars...
    .options = furrr_options(seed = TRUE)
  )
  
  # Initialize edge-type count matrices
  edge_type_count <- list(
    "none" = matrix(0, num_variables, num_variables),
    "circle" = matrix(0, num_variables, num_variables),
    "arrowhead" = matrix(0, num_variables, num_variables),
    "arrowtail" = matrix(0, num_variables, num_variables)
  )
  
  # Count occurrences of each edge type across adjacency matrices
  if(algorithm %in% c("FCI", "CCI")){
    for (adjacency_matrix in adjacency_matrices) {
      edge_type_count$none <- edge_type_count$none + (adjacency_matrix == 0)
      edge_type_count$circle <- edge_type_count$circle + (adjacency_matrix == 1)
      edge_type_count$arrowhead <- edge_type_count$arrowhead + (adjacency_matrix == 2)
      edge_type_count$arrowtail <- edge_type_count$arrowtail + (adjacency_matrix == 3)
    }
  } else if (algorithm == "PC") {
    for (adjacency_matrix in adjacency_matrices) {
      edge_type_count$none <- edge_type_count$none + (adjacency_matrix == 0)
      edge_type_count$arrowhead <- edge_type_count$arrowhead + (adjacency_matrix == 1)
      edge_type_count$circle <- NULL
      edge_type_count$arrowtail <- NULL
      
    }
  }
  
  # Initialize the final stable edge matrix
  stable_edges <- matrix(0, num_variables, num_variables)
  
  # Set stable edges based on the threshold
  for (i in 1:num_variables) {
    for (j in 1:num_variables) {
      if (edge_type_count$none[i, j] / num_subsamples <= (1 - threshold)) {
        
        # For FCI/CCI: Identify the most frequent edge type
        if (algorithm %in% c("FCI", "CCI")) {
          edge_frequencies <- c(
            edge_type_count$none[i, j],
            edge_type_count$circle[i, j],
            edge_type_count$arrowhead[i, j],
            edge_type_count$arrowtail[i, j]
          )
          most_frequent_edge_type <- which.max(edge_frequencies)
          
          # Apply threshold to determine if it's a stable edge
          if (edge_frequencies[most_frequent_edge_type] / num_subsamples >= threshold) {
            stable_edges[i, j] <- most_frequent_edge_type - 1  # Adjust for 0-indexing
          } else {
            stable_edges[i, j] <- 1  # Use circle (1) for uncertain edges
          }
          
          # For PC: check threshold for 0 and 1 only
          # For PC: check threshold for 0, 1, and handle bidirected edges
        } else if (algorithm == "PC") {
          # Calculate the proportion of "arrowhead" and "none" edge types
          proportion_arrowhead <- edge_type_count$arrowhead[i, j] / num_subsamples
          proportion_none <- edge_type_count$none[i, j] / num_subsamples
          
          # Determine the edge type based on proportions
          if (proportion_arrowhead >= threshold) {
            stable_edges[i, j] <- 1  # Keep as 1 for majority arrowhead
          } else if (proportion_none >= threshold) {
            stable_edges[i, j] <- 0  # Keep as 0 for majority absence
          } else {
            # Mark as bidirected (In the CPDAG representation used by PC, bidirected edges indicate uncertainty in edge direction).
            stable_edges[i, j] <- 1  
            stable_edges[j, i] <- 1  # Ensure symmetric representation for bidirected edge
          }
        }
      }
    }
  }
  
  
  colnames(stable_edges) <- rownames(stable_edges) <- colnames(data)
  # Return stable edges and edge type counts
  return(list(stable_edges = stable_edges, edge_type_count = edge_type_count))
}

