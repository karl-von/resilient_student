# =================================================================
# CORRECTED Script to Generate Figure 3: Bar Chart of Resilience Rates
#
# This script creates a bar chart comparing the percentage of
# academically resilient students across different key subgroups.
# =================================================================

# --- 1. Load Libraries ---
library(tidyverse)
library(here)

print("--- Generating Figure 3: Bar Chart of Resilience Rates ---")

# --- 2. Load and Prepare Data ---
# This loads the final list of 5 datasets which already include PCA scores.
print("Loading the list of 5 engineered datasets...")
INPUT_FINAL_LIST <- here("dataset", "", "Step4_ListOfEngineeredDatasets.rds")
list_of_engineered_datasets <- readRDS(INPUT_FINAL_LIST)

# Stack all 5 imputed datasets into one long data frame.
# bind_rows() is the correct function for this.
stacked_data <- bind_rows(list_of_engineered_datasets, .id = "imputation_num")

# --- 3. Create Categorical Variables ---
# Find the median of the Math Disposition (Self-Efficacy) component
math_disposition_median <- median(stacked_data$Math_Disposition_RC1, na.rm = TRUE)

# Create the new categorical variables for grouping
data_for_plot <- stacked_data %>%
  mutate(
    # Create a clean factor for Grade Repetition
    Grade_Repetition_Status = factor(if_else(REPEAT == 1, "Repeated Grade", "Did Not Repeat"),
                                     levels = c("Did Not Repeat", "Repeated Grade")),

    # Create a factor for Math Disposition based on the median
    Math_Disposition_Group = factor(if_else(Math_Disposition_RC1 >= math_disposition_median,
                                            "High Disposition", "Low Disposition"),
                                    levels = c("Low Disposition", "High Disposition"))
  )

# --- 4. Calculate Average Resilience Rates ---
# calculate the rate for each PV, then average them.

# Define the 10 resilience outcome variables
resilience_pvs <- paste0("ACADEMIC_RESILIENCE_PV", 1:10)

# Calculate the overall resilience rate
overall_resilience <- data_for_plot %>%
  summarise(across(all_of(resilience_pvs), ~ mean(.x, na.rm = TRUE))) %>%
  rowMeans()

# Calculate rates by Grade Repetition
by_repetition <- data_for_plot %>%
  group_by(Grade_Repetition_Status) %>%
  summarise(across(all_of(resilience_pvs), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
  mutate(avg_resilience_rate = rowMeans(select(., all_of(resilience_pvs))))

# Calculate rates by Math Disposition
by_disposition <- data_for_plot %>%
  group_by(Math_Disposition_Group) %>%
  summarise(across(all_of(resilience_pvs), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
  mutate(avg_resilience_rate = rowMeans(select(., all_of(resilience_pvs))))

# --- 5. Combine Data for Plotting ---
plot_data <- bind_rows(
  tibble(Group = "Overall", Category = "All Disadvantaged Students", avg_resilience_rate = overall_resilience),
  tibble(Group = "Grade Repetition", Category = as.character(by_repetition$Grade_Repetition_Status), avg_resilience_rate = by_repetition$avg_resilience_rate),
  tibble(Group = "Math Disposition", Category = as.character(by_disposition$Math_Disposition_Group), avg_resilience_rate = by_disposition$avg_resilience_rate)
) %>%
  # Order the categories for a logical plot layout
  mutate(Category = fct_reorder(Category, avg_resilience_rate))


# --- 6. Create and Save the Bar Chart ---
resilience_bar_chart <- ggplot(plot_data, aes(x = avg_resilience_rate, y = Category, fill = Group)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = scales::percent(avg_resilience_rate, accuracy = 0.1)),
            hjust = -0.2, size = 4.5, fontface = "bold", color = "gray20") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.35), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Percentage of Academically Resilient Students by Subgroup",
    subtitle = "Average resilience rate calculated across all 10 plausible values and 5 imputed datasets.",
    x = "Resilience Rate (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

# Save the plot
output_figure3_path <- here("dataset", "analysis", "Figure3_Resilience_Rates_Bar_Chart.png")
ggsave(output_figure3_path, plot = resilience_bar_chart, width = 10, height = 6, dpi = 300, bg = "white")

print(paste("Figure 3 (Bar Chart) saved to:", output_figure3_path))
print(resilience_bar_chart)