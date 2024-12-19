## =============================================================================
## Script for data preprocessing and running causal discovery
## =============================================================================
#' This script preprocesses a dataset, scales variables, and clusters data 
#' related to precariousness and depression symptoms. It runs causal discovery 
#' algorithms (PC, FCI, CCI) under multiple parameter combinations and saves the 
#' results for further analysis.
#'
#' @details
#' - The dataset is scaled and processed to derive both depression sum scores 
#'   and symptom-level data for analysis.
#' - Causal discovery algorithms are applied on subsampled data using 
#'   combinations of alpha levels, thresholds, and CI tests.
#' - Results are saved in `.rds` format for both sum score and individual 
#'   symptom analyses.
## =============================================================================

# ==============================================================================
# Load required libraries and functions
# ==============================================================================
source("Precarity_HELIUS/code/utils/libraries.R")   # Load libraries
source("Precarity_HELIUS/code/utils/robust_func.R") # Load helper functions

# Load the dataset 
# dat <- read_sav("../../../data/HELIUS_LEONIE.sav") 

## =============================================================================
## Preprocess and scale the precariousness data
## =============================================================================
scaled_data <- dat %>%
  # Select precariousness-related variables
  # Employment Precariousness
  dplyr::select(H1_Arbeidsparticipatie, H1_WerkSit, H1_RecentErv8, 
                # Financial Precariousness
                H1_InkHhMoeite, H1_RecentErv9,                      
                # Housing Precariousness
                veilig_2012, vrz_2012, P_HUURWON,                   
                # Cultural Precariousness
                H1_Discr_sumscore, H1_SBSQ_meanscore, A_BED_RU,         
                # Social Precariousness
                H1_RecentErv5, H1_RecentErv6, H1_RecentErv7,       
                H1_SSQT, H1_SSQSa,                                  
                # Depression symptoms / sum scores
                H1_WlbvRecent1, H1_WlbvRecent2, H1_WlbvRecent3, H1_WlbvRecent4, H1_WlbvRecent5, H1_WlbvRecent6, H1_WlbvRecent7, H1_WlbvRecent_89, H1_WlbvRecent10, H1_PHQ9_sumscore,   
                # Ethnicity
                H1_etniciteit,
                # Ags
                H1_lft) %>%                                
  # Replace missing codes with NA 
  mutate(across(everything(), ~ na_if(.x, -9)), 
         across(everything(), ~ na_if(.x, -1))) %>%  
  # Remove NAs 
  na.omit() %>%
  # Convert all columns to numeric
  mutate(across(everything(), as.numeric),
         # Combine categories 5 to 10 into a single category (5-10 as one unemployed)
         H1_WerkSit = case_when(
           H1_WerkSit %in% c(5, 6, 7, 8, 9, 10) ~ 5,  
           .default = H1_WerkSit  # Keep other values as they are 
         ))  %>%
  # Reverse the values of specific variables (reverse transformation)
  mutate(across(c(H1_RecentErv8, H1_RecentErv9, veilig_2012, vrz_2012, 
                  H1_SBSQ_meanscore, A_BED_RU, H1_RecentErv5, H1_RecentErv6, 
                  H1_RecentErv7, H1_SSQT, H1_SSQSa), 
                ~ max(.x) - .x)) %>%
  # scale the data
  mutate(across(!c(H1_etniciteit, H1_lft), scale))
# Apply Min-Max scaling to all columns
# mutate(across(everything(), min_max_scaling))

# redefine colnames
colnames(scaled_data) <- c(
  "emp_stat",       # H1_Arbeidsparticipatie
  "work_sit",   # H1_WerkSit
  "unemp12",     # H1_RecentErv8
  "inc_dif",      # H1_InkHhMoeite
  "fincri12",# H1_RecentErv9
  "nb_safe",     # veilig_2012
  "nb_res",   # vrz_2012
  "nb_rent",         # P_HUURWON
  "discrim",      # H1_Discr_sumscore
  "hea_lit",       # H1_SBSQ_meanscore
  "cul_rec",      # A_BED_RU
  "rel_end12",   # H1_RecentErv5
  "frd_brk12", # H1_RecentErv6
  "conf12",         # H1_RecentErv7
  "soc_freq",      # H1_SSQT
  "soc_adq",      # H1_SSQSa
  "anh",       # H1_WlbvRecent1
  "dep",       # H1_WlbvRecent2
  "slp",       # H1_WlbvRecent3
  "ene",       # H1_WlbvRecent4
  "app",       # H1_WlbvRecent5
  "glt",       # H1_WlbvRecent6
  "con",       # H1_WlbvRecent7
  "mot",     # H1_WlbvRecent_89
  "sui",      # H1_WlbvRecent10
  "PHQsum",         # H1_PHQ9_sumscore
  "ethn",         # H1_etniciteit
  "age"          #"H1_lft" 
)


# clustered data 
clust_data <- scaled_data |> 
  mutate(
    P.emp = rowMeans(cbind(emp_stat, work_sit)),
    P.soc = rowMeans(cbind(soc_freq, soc_adq)),
    P.hou = rowMeans(cbind(nb_safe, nb_res, nb_rent, cul_rec)),
    S.rel = rowMeans(cbind(frd_brk12, conf12)),
    S.fin = rowMeans(cbind(fincri12, inc_dif))
  ) |>
  dplyr::select(17:33) |>
  as.data.frame()

# sum score data
depsum <- clust_data |> dplyr::select(PHQsum, P.emp, P.soc, P.hou, S.rel, S.fin) 

# symptom score data
depsym <- clust_data |> dplyr::select(anh, dep, slp, ene, app, glt, con, mot, sui, P.emp, P.soc, P.hou, S.rel, S.fin) 



## =============================================================================
## Causal discovery: run analysis under each setup
## =============================================================================

# Define parameters
alphas = c(0.01, 0.05)
thresholds = c(0.5, 0.6, 0.7, 0.8)
citests = c("gaussCItest", "RCoT")
algorithms = c("PC", "FCI", "CCI")
data <- depsum # sumscore 
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
  file_name <- glue::glue("dat_sumscore/{algorithm}_alpha_{alpha}_threshold_{threshold}_citest_{citest}.rds")
  # individual symptom version
  file_name <- glue::glue("dat_symscore/{algorithm}_alpha_{alpha}_threshold_{threshold}_citest_{citest}.rds")
  saveRDS(result, file = file_name)
  
  # Print a message to track progress
  message(glue("Completed: algorithm = {algorithm}, alpha = {alpha}, threshold = {threshold}, citest = {citest}"))
}

# Apply the function to each row in the parameter data frame
pwalk(params, run_and_save, .progress = TRUE)
