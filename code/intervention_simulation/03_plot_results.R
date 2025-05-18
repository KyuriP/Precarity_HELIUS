## =============================================================================
## Script: 03_plot_results.R
## Plot Intervention Effects: Line Summaries and Density Heatmaps
## =============================================================================

# -----------------------------------------------------------------------------
# 1. Load Simulation Outputs
# -----------------------------------------------------------------------------

# Load line summary results for top 30 parameter sets
results_DP <- readRDS("Precarity_HELIUS/data/intervention/simulated_intervention_top30.rds")

# Load final-state simulations for Sol_1 and Sol_21
final_sol1_sol21 <- readRDS("Precarity_HELIUS/data/intervention/final_sol1_sol21.rds")

# -----------------------------------------------------------------------------
# 2. Plot Line Summary: Mean Final D and P Across Intervention Levels
# -----------------------------------------------------------------------------

# Prepare data in long format
results_long <- results_DP %>%
  pivot_longer(cols = c("Mean_D", "Mean_P"),
               names_to = "Variable", values_to = "Value")

# Line plot of intervention curves
plot_lines <- ggplot(results_long, aes(x = Intervention, y = Value,
                                       group = interaction(ParamID, Variable),
                                       color = param_strength)) +
  geom_line(linewidth = 0.5) +
  facet_grid(. ~ Variable, scales = "free_y") +
  scale_color_viridis_c(option = "D", name = "Feedback and\nInput Strength") +
  labs(
    title = "Final Mean D and P Across Intervention Sizes",
    subtitle = expression(paste("Top 30 Parameter Sets | Color = ", alpha[1], "×", beta[1], "×", alpha[2], "×", beta[2])),
    x = "Intervention Size", y = "Final Value"
  ) +
  theme_minimal(base_size = 14, base_family = "Palatino") +
  theme(
    strip.text       = element_text(face = "bold", size = 14),
    axis.line        = element_line(color = "black", linewidth = 0.3),
    legend.position  = "bottom",
    legend.key.width = unit(1, "null"),
    panel.grid       = element_blank()
  )

# -----------------------------------------------------------------------------
# 3. Create Smoothed Density Heatmaps for Sol_1 and Sol_21
# -----------------------------------------------------------------------------

# Reshape final-state simulation data to long format
final_long <- final_sol1_sol21 %>%
  pivot_longer(cols = c(D_final, P_final),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Value = map_dbl(Value, ~ .x[[1]]))  # Unwrap from list

# Function: Compute 2D density for one Model–Variable facet
compute_density_facet <- function(sub_df) {
  dens <- MASS::kde2d(sub_df$Intervention, sub_df$Value, n = 100)
  expand.grid(
    Intervention = dens$x,
    Value        = dens$y
  ) %>%
    mutate(
      Density  = as.vector(dens$z),
      Model    = unique(sub_df$Model),
      Variable = unique(sub_df$Variable)
    )
}

# Compute density for Sol_1 (Unistable)
density_data_uni <- final_long %>%
  filter(Model == "Sol_1") %>%
  group_split(Model, Variable) %>%
  map_dfr(compute_density_facet) %>%
  mutate(Model = "Unistable_Candidate")

# Compute density for Sol_21 (Bistable)
density_data_bi <- final_long %>%
  filter(Model == "Sol_21") %>%
  group_split(Model, Variable) %>%
  map_dfr(compute_density_facet) %>%
  mutate(Model = "Bistable_Candidate")

# -----------------------------------------------------------------------------
# 4. Plot Density Heatmaps
# -----------------------------------------------------------------------------

# Unistable Candidate plot
plot_unistable <- ggplot(density_data_uni, aes(x = Intervention, y = Value, fill = Density)) +
  geom_raster(interpolate = TRUE) +
  facet_grid(Model ~ Variable, scales = "fixed") +
  ylim(-2.5, 2.5) +
  scale_fill_viridis_c(option = "plasma", name = "Density") +
  labs(x = "", y = "Final Value") +
  theme_minimal(base_size = 13, base_family = "Palatino") +
  theme(
    strip.text      = element_text(face = "bold", size = 14),
    axis.text.x     = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "none"
  )

# Bistable Candidate plot
plot_bistable <- ggplot(density_data_bi, aes(x = Intervention, y = Value, fill = Density)) +
  geom_raster(interpolate = TRUE) +
  facet_grid(Model ~ Variable, scales = "fixed") +
  ylim(-1.5, 1.5) +
  scale_fill_viridis_c(option = "plasma", name = "Density") +
  labs(x = "Intervention Size", y = "Final Value") +
  theme_minimal(base_size = 13, base_family = "Palatino") +
  theme(
    strip.text      = element_text(face = "bold", size = 14),
    strip.text.x    = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, "null")
  )

# -----------------------------------------------------------------------------
# 5. Combine and Save Final Plot
# -----------------------------------------------------------------------------

# Stack unistable and bistable heatmaps with spacer
bottom_plots <- plot_unistable / plot_spacer() / plot_bistable +
  plot_layout(heights = c(1, -0.3, 1), axis_titles = "collect")

# Combine line plot with heatmaps
combined_plot <- plot_lines / bottom_plots +
  plot_layout(heights = c(1.1, 1.1), axis_titles = "collect")

# Save if desired
# ggsave("intervention_plot2.pdf", combined_plot, width = 11, height = 13)