## =============================================================================
## Script for Visualizing Proportions of Edge Endpoint Types in Depression Graphs
## =============================================================================
#' This script computes and visualizes the proportions of edge endpoint types 
#' (null, circle, arrowhead, arrowtail) for causal discovery results (FCI and CCI)
#' derived from sum score and individual symptom-level data.
#'
#' @details
#' - Stable edges are extracted from `.rds` files containing causal discovery results.
#' - Proportions of edge types are computed for each edge across multiple runs.
#' - Heatmaps are generated to visualize the dominant edge types and their proportions.
#' 
#' @output
#' - Heatmaps visualizing the proportions of edge endpoint types for sum score graphs.
#' - Heatmaps visualizing the proportions of edge endpoint types for individual symptom graphs.
#' - Figures exported as `.pdf` files.
## =============================================================================


# BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)
library(grid)
library(ggpubr)
library(ggplotify)


# Define the directory containing the .rds files
directory <- "Precarity_HELIUS/data/dep_sumscore"  # Replace with the path you need
# directory <- "Precarity_HELIUS/data/dep_symscore"  # individual symptom
# directory <- "Precarity_HELIUS/data/presum_symscore"  # individual symptom

# List all the file names from the directory
file_names <- list.files(directory, full.names = TRUE, pattern = "\\.rds$")

# Read all .rds files into a list
rds_data <- lapply(file_names, readRDS)

# Assign meaningful names to the datasets based on the filenames
names(rds_data) <- gsub("\\.rds$", "", basename(file_names))

# Check the loaded data
print(names(rds_data))  # View the names of the datasets
# Group files by algorithm
grouped_rds_data <- list(
  FCI = rds_data[grep("FCI", names(rds_data))],
  CCI = rds_data[grep("CCI", names(rds_data))]
)


# Extract stable_edges for each algorithm group
stable_edges_grouped <- lapply(grouped_rds_data, function(group) {
  lapply(group, function(data) data$stable_edges)
})


# Function to summarize stable edges as proportions
summarize_stable_edges_proportion <- function(stable_edges_list) {
  # Get the dimensions of the matrices (assuming all matrices are the same size)
  nrow <- nrow(stable_edges_list[[1]])
  ncol <- ncol(stable_edges_list[[1]])
  
  # Create a list to store proportions for each value (0, 1, 2, 3)
  summary <- array(0, dim = c(nrow, ncol, 4), 
                   dimnames = list(
                     rownames(stable_edges_list[[1]]),
                     colnames(stable_edges_list[[1]]),
                     c("0", "1", "2", "3")
                   ))
  
  # Total number of matrices
  num_matrices <- length(stable_edges_list)
  
  # Loop through each matrix and count occurrences of each value
  for (mat in stable_edges_list) {
    for (i in 1:nrow) {
      for (j in 1:ncol) {
        value <- as.character(mat[i, j])  # Convert value to a string for indexing
        summary[i, j, value] <- summary[i, j, value] + 1
      }
    }
  }
  
  # Convert counts to proportions
  summary <- summary / num_matrices
  
  return(summary)  # Return the summarized array as proportions
}

# Apply the summarization to each algorithm group
summarized_stable_edges_proportion <- lapply(stable_edges_grouped, function(group) {
  summarize_stable_edges_proportion(group)
})

# Example: View proportions for FCI group
cat("Proportional Stable Edges for FCI Group:\n")
print(summarized_stable_edges_proportion$FCI)
print(summarized_stable_edges_proportion$CCI)


# Use summarized stable edges for a specific algorithm (e.g., CCI and FCI)
proportion_array_FCI <- summarized_stable_edges_proportion$FCI
proportion_array_CCI <- summarized_stable_edges_proportion$CCI

# Generate dominant_symbol
dominant_symbol_FCI <- apply(proportion_array_FCI, c(1, 2), function(x) which.max(x) - 1)
dominant_symbol_CCI <- apply(proportion_array_CCI, c(1, 2), function(x) which.max(x) - 1)

# Define blended gradient colors for all symbols
blend_colors <- function(proportions) {
  # Normalize proportions to sum to 1
  proportions <- proportions / sum(proportions)
  
  # Define base colors for each symbol
  base_colors <- c("darkgray", "mediumpurple4", "aquamarine4", "salmon")
  
  # Convert base colors to RGB
  base_rgb <- t(col2rgb(base_colors) / 255)
  
  # Compute the weighted average of the colors
  blended_rgb <- colSums(base_rgb * proportions)
  
  # Convert back to HEX color
  rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3])
}

# Generate blended colors for all cells in the matrix
blended_cell_colors_FCI <- matrix(
  apply(proportion_array_FCI, c(1, 2), blend_colors),
  nrow = nrow(dominant_symbol_FCI),
  ncol = ncol(dominant_symbol_FCI),
  dimnames = dimnames(dominant_symbol_FCI)
)

blended_cell_colors_CCI <- matrix(
  apply(proportion_array_CCI, c(1, 2), blend_colors),
  nrow = nrow(dominant_symbol_CCI),
  ncol = ncol(dominant_symbol_CCI),
  dimnames = dimnames(dominant_symbol_CCI)
)

# Custom legend with bold labels
custom_labels <- c(
  "0" = expression(bold("N")~ "null"),
  "1" = expression(bold("o")~ "circle"),
  "2" = expression(bold(">")~ "arrowhead"),
  "3" = expression(bold("–")~ "arrowtail"))


custom_legend <- Legend(
  labels = custom_labels,
  legend_gp = gpar(
    fill = c("darkgray", "mediumpurple4", "aquamarine4", "salmon")),
  labels_gp = gpar( fontsize = 12,                  # Ensure label font size increases
                    fontfamily = "Palatino"         # Set font to Palatino for labels
  ),
  title = "Edge Type",
  title_gp = gpar(
    fontsize = 15,                  # Set a larger font size for the title
    fontfamily = "Palatino",        # Use Palatino for the title
    fontface = "bold"               # Make the title bold
  )
)


# Define colors for each category (used in heatmap only for dominant values)
category_colors <- c(
  "0" = "darkgray",
  "1" = "mediumpurple4",
  "2" = "aquamarine4",
  "3" = "salmon"
)

# Generate the heatmaps with proper bolding and multi-line text for non-zero proportions
heatmap_fci <- Heatmap(
  dominant_symbol_FCI,
  name = "Edge Type",
  col = category_colors,  # Colors for valid edge types
  rect_gp = gpar(col = "black"),  # Add borders to each cell
  cluster_rows = FALSE,  # Keep original row order
  cluster_columns = FALSE,  # Keep original column order
  cell_fun = function(j, i, x, y, width, height, fill) {
    # Draw the cell background with blended colors
    grid.rect(x, y, width, height, gp = gpar(fill = blended_cell_colors_FCI[i, j], col = NA))
    
    # Generate text lines for non-zero proportions
    text_lines <- c()
    if (proportion_array_FCI[i, j, "0"] > 0) {
      text_lines <- c(text_lines, sprintf("N: %.2f", proportion_array_FCI[i, j, "0"]))
    }
    if (proportion_array_FCI[i, j, "1"] > 0) {
      text_lines <- c(text_lines, sprintf("o: %.2f", proportion_array_FCI[i, j, "1"]))
    }
    if (proportion_array_FCI[i, j, "2"] > 0) {
      text_lines <- c(text_lines, sprintf(">: %.2f", proportion_array_FCI[i, j, "2"]))
    }
    if (proportion_array_FCI[i, j, "3"] > 0) {
      text_lines <- c(text_lines, sprintf("–: %.2f", proportion_array_FCI[i, j, "3"]))
    }
    
    # Combine all text lines and display them in the cell
    if (length(text_lines) > 0) {
      grid.text(
        paste(text_lines, collapse = "\n"), 
        x, y, gp = gpar(fontsize = 10, fontfamily = "Palatino", col = "white", fontface = "bold"))
    }
  },
  show_heatmap_legend = FALSE,
  row_names_side = "left",  # Show row names on the left
  column_names_rot = 45,  # Rotate column names for better readability
  show_column_names = TRUE,
  show_row_names = TRUE,
  row_names_gp = gpar(fontsize = 12, fontfamily = "Palatino"),  # Palatino font for row labels
  column_names_gp = gpar(fontsize = 12, fontfamily = "Palatino"),  # Palatino font for column labels
  heatmap_legend_param = list(
    title = NULL,  # Suppress default Heatmap legend
    at = NULL,     # No entries
    labels = NULL  # No labels
  )
)

heatmap_cci <- Heatmap(
  dominant_symbol_CCI,
  name = "Edge Type",
  col = category_colors,  # Colors for valid edge types
  rect_gp = gpar(col = "black"),  # Add borders to each cell
  cluster_rows = FALSE,  # Keep original row order
  cluster_columns = FALSE,  # Keep original column order
  cell_fun = function(j, i, x, y, width, height, fill) {
    # Draw the cell background with blended colors
    grid.rect(x, y, width, height, gp = gpar(fill = blended_cell_colors_CCI[i, j], col = NA))
    
    # Generate text lines for non-zero proportions
    text_lines <- c()
    if (proportion_array_CCI[i, j, "0"] > 0) {
      text_lines <- c(text_lines, sprintf("N: %.2f", proportion_array_CCI[i, j, "0"]))
    }
    if (proportion_array_CCI[i, j, "1"] > 0) {
      text_lines <- c(text_lines, sprintf("o: %.2f", proportion_array_CCI[i, j, "1"]))
    }
    if (proportion_array_CCI[i, j, "2"] > 0) {
      text_lines <- c(text_lines, sprintf(">: %.2f", proportion_array_CCI[i, j, "2"]))
    }
    if (proportion_array_CCI[i, j, "3"] > 0) {
      text_lines <- c(text_lines, sprintf("–: %.2f", proportion_array_CCI[i, j, "3"]))
    }
    
    # Combine all text lines and display them in the cell
    if (length(text_lines) > 0) {
      grid.text(
        paste(text_lines, collapse = "\n"), 
        x, y, gp = gpar(fontsize = 10, fontfamily = "Palatino", col = "white", fontface = "bold"))
    }
  },
  show_heatmap_legend = FALSE,
  row_names_side = "left",  # Show row names on the left
  column_names_rot = 45,  # Rotate column names for better readability
  show_column_names = TRUE,
  show_row_names = TRUE,
  row_names_gp = gpar(fontsize = 12, fontfamily = "Palatino"),  # Palatino font for row labels
  column_names_gp = gpar(fontsize = 12, fontfamily = "Palatino"),  # Palatino font for column labels
  heatmap_legend_param = list(
    title = NULL,  # Suppress default Heatmap legend
    at = NULL,     # No entries
    labels = NULL  # No labels
  )
)

# Convert heatmaps to ggplot objects
heatmap_FCI_plot <- ggplotify::as.ggplot(grid.grabExpr(draw(heatmap_fci))) + ggtitle("(a) FCI") +   
  theme(
  plot.title = element_text(
    hjust = 0.55,          # Center the title
    family = "Palatino",  # Set font to Palatino
    face = "bold",        # Make the title bold
    size = 18             # Slightly larger font size
  )
)

heatmap_CCI_plot <- ggplotify::as.ggplot(grid.grabExpr(draw(heatmap_cci))) + ggtitle("(b) CCI") +   
  theme(
  plot.title = element_text(
    hjust = 0.55,          # Center the title
    family = "Palatino",  # Set font to Palatino
    face = "bold",        # Make the title bold
    size = 18             # Slightly larger font size
  )
)

legend <- ggplotify::as.ggplot(grid.grabExpr(draw(custom_legend)))
legend_h <- ggplotify::as.ggplot(grid.grabExpr(draw(custom_legend_h))) # horizontal version 


## Combine heatmaps and legend using ggarrange
# vertical version
sumgraph_mat_v <- ggpubr::ggarrange(
  heatmap_FCI_plot,
  heatmap_CCI_plot,
  legend_v,
  ncol = 3,  # Heatmaps side by side with legend
  widths = c(3, 3, 0.9)  # Adjust the relative width of each panel
)
print(sumgraph_mat_v)

# horizontal version
sumgraph_mat_h <- ggpubr::ggarrange(
  heatmap_FCI_plot,
  heatmap_CCI_plot,
  legend_h,
  nrow = 3,  # Heatmaps side by side with legend
  heights = c(3, 3, 0.1)  # Adjust the relative width of each panel
)
print(sumgraph_mat_h)
# save plot
# ggpubr::ggexport(filename = "sumgraph_mat.pdf", plot = sumgraph_mat,  width = 11, height = 5, units = "cm")
# ggpubr::ggexport(filename = "symgraph_mat.pdf", plot = sumgraph_mat,  width = 11, height = 5, units = "cm")
# ggpubr::ggexport(filename = "presumgraph_mat.pdf", plot = sumgraph_mat_h,  width = 9, height = 20, units = "cm")


# legend horizontal version (for symptom graph)
symgraph_mat_h <- ggpubr::ggarrange(
  heatmap_FCI_plot,
  heatmap_CCI_plot,
  legend_h,
  nrow = 3,  # Heatmaps side by side with legend
  heights = c(3, 3, 0.1)  # Adjust the relative width of each panel
)
print(symgraph_mat_h)
# save plot
# ggpubr::ggexport(filename = "symgraph_mat.pdf", plot = symgraph_mat_h,  width = 9, height = 20, units = "cm")



