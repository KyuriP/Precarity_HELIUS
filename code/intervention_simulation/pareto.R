## =============================================================================
## Analyze NSGA-II Results and Plot Pareto Frontier
## =============================================================================
# Load libraries
source("Precarity_HELIUS/code/utils/libraries.R")   

# ---- 1. Load NSGA-II Results ----
result <- readRDS("Precarity_HELIUS/data/intervention/multioptim_result_6var7optim_500pop_fixedS.rds") 

param_df <- as.data.frame(result$parameters) %>%
  setNames(c("beta1", "beta2", "alpha1", "alpha2", "sigma1", "sigma2"))

obj_df <- as.data.frame(result$objectives) %>%
  setNames(c("mean_D_err", "var_D_err", "mean_P_err", "var_P_err", 
             "pcor_DP_err", "pcor_DS_err", "pcor_PS_err"))

df <- bind_cols(param_df, obj_df) %>%
  mutate(total_error = rowSums(across(ends_with("_err"))))

# ---- 2. Visualize Parameter Distributions ----
param_long <- param_df %>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value")

ggplot(param_long, aes(x = Value, fill = Parameter)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "black") +
  facet_wrap(~Parameter, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Parameter Values (Final Population)")

# ---- 3. Identify Top 5% Solutions ----
threshold <- quantile(df$total_error, 0.05)
df <- df %>% mutate(Top = total_error <= threshold)

top_df <- df %>% filter(Top) %>% arrange(total_error)

# Visualize Top Parameter Distributions
top_df %>%
  pivot_longer(cols = starts_with(c("beta", "alpha", "sigma")), 
               names_to = "Parameter", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Parameter)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "black") +
  facet_wrap(~Parameter, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Parameters (Top Solutions)")

# ---- 4. Pareto Frontier Plot ----
# Filter reasonable range for clarity
df_filtered <- df %>% filter(
  var_D_err + var_P_err < 2,
  mean_D_err + mean_P_err < 2,
  pcor_DP_err + pcor_DS_err + pcor_PS_err < 2
)

# Compute aggregated error categories
df_filtered <- df_filtered %>%
  mutate(
    total_variance_error = var_D_err + var_P_err,
    total_partial_error  = pcor_DP_err + pcor_DS_err + pcor_PS_err,
    total_mean_error     = mean_D_err + mean_P_err,
    total_error_sum      = total_variance_error + total_partial_error + total_mean_error,
    Solution             = paste0("S", row_number())
  )

# Select top N for highlighting
top_solutions <- df_filtered %>% 
  slice_min(total_error_sum, n = 30)

df_filtered <- df_filtered %>%
  mutate(label = ifelse(Solution %in% top_solutions$Solution, "Top 30 Solutions", NA))

# ---- 5. Generate Final Pareto Plot ----
paretoplot <- ggplot(df_filtered, aes(
  x = total_variance_error,
  y = total_partial_error
)) +
  geom_point(aes(color = total_mean_error), size = 1, alpha = 0.6, show.legend = FALSE) +
  geom_point(
    data = df_filtered %>% filter(label == "Top 30 Solutions"),
    aes(color = total_mean_error, shape = label),
    size = 1.5, stroke = 0.5, color = "red3", show.legend = TRUE
  ) +
  scale_shape_manual(values = c("Top 30 Solutions" = 4), name = "") +
  scale_color_viridis_c(option = "D", name = "Total Mean Error") +
  labs(
    title = "Pareto Frontier with Top Solutions Highlighted",
    x = "Total Variance Error",
    y = "Total Partial Correlation Error"
  ) +
  theme_light(base_size = 15) +
  theme(
    text = element_text(family = "Palatino"),
    legend.spacing.y = unit(0.5, 'cm'),
    legend.key.height = unit(1, "null"))

# ggsave("paretoplot.png", paretoplot) # save the plot


