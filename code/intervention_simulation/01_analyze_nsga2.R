## =============================================================================
## Script: 01_analyze_nsga2.R
## Analyze NSGA-II Results and Visualize Pareto Frontier
## =============================================================================

# Load libraries
source("Precarity_HELIUS/code/utils/libraries.R")

# Load NSGA-II results
result <- readRDS("Precarity_HELIUS/data/intervention/multioptim_result_6var7optim_500pop_fixedS.rds") 

# Prepare parameter and objective data
df <- bind_cols(
  as.data.frame(result$parameters) %>% setNames(c("beta1", "beta2", "alpha1", "alpha2", "sigma1", "sigma2")),
  as.data.frame(result$objectives) %>% setNames(c("mean_D_err", "var_D_err", "mean_P_err", "var_P_err", "pcor_DP_err", "pcor_DS_err", "pcor_PS_err"))
) %>%
  mutate(total_error = rowSums(across(ends_with("_err"))))

# Visualize parameter distributions
param_long <- df %>% select(beta1:sigma2) %>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value")

ggplot(param_long, aes(x = Value, fill = Parameter)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "black") +
  facet_wrap(~Parameter, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Parameter Values (Final Population)")

# Filter and score Pareto-efficient solutions
df_filtered <- df %>%
  filter(
    var_D_err + var_P_err < 2,
    mean_D_err + mean_P_err < 2,
    pcor_DP_err + pcor_DS_err + pcor_PS_err < 2
  ) %>%
  mutate(
    total_variance_error = var_D_err + var_P_err,
    total_partial_error = pcor_DP_err + pcor_DS_err + pcor_PS_err,
    total_mean_error = mean_D_err + mean_P_err,
    total_error_sum = total_variance_error + total_partial_error + total_mean_error,
    Solution = paste0("S", row_number())
  )

# Select top solutions
top_solutions <- df_filtered %>% slice_min(total_error_sum, n = 30)

# Plot Pareto front
paretoplot <- ggplot(df_filtered, aes(x = total_variance_error, y = total_partial_error)) +
  geom_point(aes(color = total_mean_error), size = 1, alpha = 0.6) +
  geom_point(
    data = df_filtered %>% filter(Solution %in% top_solutions$Solution),
    aes(shape = "Top 30 Solutions"), color = "red3", size = 1.5
  ) +
  scale_shape_manual(values = c("Top 30 Solutions" = 4), name = "") +
  scale_color_viridis_c(option = "D", name = "Total Mean Error") +
  labs(title = "Pareto Frontier with Top Solutions Highlighted",
       x = "Total Variance Error", y = "Total Partial Correlation Error") +
  theme_light(base_size = 15) +
  theme(text = element_text(family = "Palatino"))

# Save pareto plot
# ggsave("Precarity_HELIUS/figures/pareto_frontier.pdf", paretoplot, width = 8, height = 6)

# Save top solutions
# saveRDS(top_solutions, "Precarity_HELIUS/data/intervention/top_solutions_nsga2.rds")
