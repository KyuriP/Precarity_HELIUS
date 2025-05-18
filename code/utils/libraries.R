## =============================================================================
## Load Required Libraries
## =============================================================================

# Data manipulation and processing
library(dplyr)       # General data manipulation
library(tidyr)       # Creating parameter grids and reshaping data
library(purrr)       # Functional programming tools
library(magrittr)    # Pipe operators and functional utilities
library(stringr)     # String manipulation
library(glue)        # Dynamic string interpolation
library(tidyverse)
library(MASS)

# Data input/output
library(haven)       # Reading SPSS .sav files

# Visualization
library(ggplot2)     # Data visualization
library(qgraph)      # Graph-based visualizations
library(plotly)
library(GGally)
library(viridis)
library(patchwork)

# Parallel processing
library(furrr)       # Parallelized mapping functions
library(future)      # Setting up parallel execution plans

# Causal discovery
library(pcalg)       # Algorithms for causal discovery
#devtools::install_github("kyurip/RCIT", force = TRUE)
library(RCIT)        # Kernel-based conditional independence tests
#devtools::install_github("kyurip/CCI_KP", force = TRUE)
library(CCI.KP)      # CCI for causal discovery

# Graph handling
library(graph)       # Graph structure utilities

# SDE handling
library(sde)
library(deSolve)

# Optimization
library(nsga2R)

