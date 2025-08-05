# =================================================================
# Script to Generate an Improved Figure for Country Fixed-Effects
#
# This script uses a cleaner, publication-ready theme to visualize
# the country coefficients from Model 2 only.
# =================================================================

library(tidyverse)
library(here)

print("--- Generating Improved Figure for Country Fixed-Effects ---")

# --- 1. Create a Data Frame with Your Model 2 Results ---
# Chile (CHL) is the reference country, so its coefficient is 0.
country_results_df <- tibble(
  term = c("CNTFIN", "CNTGBR", "CNTGRC", "CNTHKG", "CNTHRV", "CNTISL", "CNTKOR",
           "CNTLTU", "CNTMLT", "CNTNZL", "CNTPRT", "CNTROU", "CNTTAP", "CNTTUR", "CNTCHL"),
  country_name = c("Finland", "United Kingdom", "Greece", "Hong Kong", "Croatia", "Iceland", "Republic of Korea",
                   "Lithuania", "Malta", "New Zealand", "Portugal", "Romania", "Chinese Taipei", "Turkey", "Chile (Reference)"),
  estimate = c(-0.701, -0.878, -0.335, -0.529, -0.691, -0.936, -1.411,
               -0.693, -0.885, -1.130, -0.512, -0.494, -1.060, -0.151, 0),
  std_error = c(0.420, 0.236, 0.240, 0.254, 0.219, 0.414, 0.272,
                0.407, 0.250, 0.258, 0.209, 0.248, 0.255, 0.245, 0)
)

# --- 2. Calculate 95% Confidence Intervals ---
country_plot_data <- country_results_df %>%
  mutate(
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  )

# --- 3. Create the Improved ggplot ---
# This plot uses fct_reorder() to sort countries and theme_minimal() for a clean look.
improved_country_plot <- ggplot(country_plot_data,
                                # Reorder countries by the size of their coefficient for easy interpretation
                                aes(x = estimate, y = fct_reorder(country_name, estimate))) +

  # Add the vertical reference line at 0 (Chile's position)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +

  # Add the confidence interval whiskers (using height = 0 for a clean line)
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0, color = "gray40", linewidth = 0.8) +

  # Add the point estimates
  geom_point(color = "#0072B2", size = 3.5, alpha = 0.8) + # Using a professional blue color

  # Add clear, publication-style labels
  labs(
    title = "CNT-Level Differences in Academic Resilience",
    subtitle = "Results from the full model, with Chile as the reference category.",
    x = "Coefficient (Log-Odds Difference from Chile)",
    y = NULL
  ) +


  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.line.x = element_line(color = "gray30"), # Add a solid x-axis line
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot" # Align title to the full plot width
  )

# --- 4. Save and Print the Plot ---
output_figure2_path <- here("dataset", "analysis", "Figure2_CNT_Effects_Plot_Improved.png")
ggsave(output_figure2_path, plot = improved_country_plot, width = 10, height = 8, dpi = 300, bg = "white")

print(paste("Figure CNT saved to:", output_figure2_path))
print(improved_country_plot)