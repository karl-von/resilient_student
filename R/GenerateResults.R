# --- 1. Load Libraries ---
library(tidyverse)
library(here)
library(mice)
library(psych)
library(ggplot2)

print("\n--- Generating PCA Diagnostics: Scree Plots & Loading Tables (Step 4) ---")

# --- A. Define Paths and Load Data ---
INPUT_PCA_MODELS <- here("dataset", "analysis", "Step4_PCAModelObjects_for_Diagnostics.rds")
OUTPUT_DIAGNOSTICS_DIR_STEP4 <- here("dataset", "analysis", "diagnostics_pca")
dir.create(OUTPUT_DIAGNOSTICS_DIR_STEP4, showWarnings = FALSE, recursive = TRUE)

pca_models <- readRDS(INPUT_PCA_MODELS)

# --- B. Create Scree Plots and Save Loading Tables for each PCA group ---
for (group_name in names(pca_models)) {
  print(paste("... Processing PCA diagnostics for group:", group_name))

  # --- Scree Plot ---
  unrotated_model <- pca_models[[group_name]]$unrotated_model
  eigenvalues <- unrotated_model$values

  scree_data <- tibble(
    component = 1:length(eigenvalues),
    eigenvalue = eigenvalues
  )

  scree_plot <- ggplot(scree_data, aes(x = component, y = eigenvalue)) +
    geom_line(color = "skyblue") +
    geom_point(color = "red", size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("Scree Plot for:", group_name),
      subtitle = "Eigenvalues of Unrotated Components",
      x = "Component Number",
      y = "Eigenvalue"
    ) +
    scale_x_continuous(breaks = 1:length(eigenvalues)) +
    theme_minimal()

  # Save the scree plot
  scree_plot_path <- file.path(OUTPUT_DIAGNOSTICS_DIR_STEP4, paste0(group_name, "_scree_plot.png"))
  ggsave(scree_plot_path, plot = scree_plot, width = 7, height = 5)

  # --- Factor Loadings Table ---
  rotated_model <- pca_models[[group_name]]$rotated_model
  loadings_matrix <- rotated_model$loadings

  # Convert to a dataframe and save as CSV
  loadings_df <- as.data.frame(unclass(loadings_matrix)) %>%
    rownames_to_column("Variable")

  loadings_path <- file.path(OUTPUT_DIAGNOSTICS_DIR_STEP4, paste0(group_name, "_factor_loadings.csv"))
  write_csv(loadings_df, loadings_path)
}

print(paste("PCA diagnostic plots and tables saved to:", OUTPUT_DIAGNOSTICS_DIR_STEP4))


# --- 1. Load Libraries ---
library(tidyverse)
library(here)
library(survey)
library(gtsummary)
library(gt)
library(broom)
library(haven)


# --- 2. Generate Table 1: Weighted Descriptive Statistics ---
# Describes the final analytical sample (before imputation)
# --- 1. 加载库 ---
# --- 1. Load libraries ---
library(tidyverse)
library(here)
library(survey)

print("--- Manually computing summary statistics and saving as CSV file) ---")

# --- 2. 加载和准备数据 ---
# --- 2. Load and prepare data ---
data_pre_imputation <- readRDS(here("dataset", "", "Step2_Merged_Cleaned_for_Imputation.rds"))

# 定义关键变量，用于缺失数据筛选
# Define key predictor variables used for filtering missingness
predictor_vars <- c(
  "ST004D01T", "AGE", "ISCEDP", "IMMIG", "EXPECEDU", "BSMJ", "SISCO", "GROSAGR",
  "ANXMAT", "MATHEFF", "MATHEF21", "MATHPERS", "ASSERAGR", "COOPAGR", "CURIOAGR",
  "EMOCOAGR", "EMPATAGR", "PERSEVAGR", "STRESAGR", "CREATEFF", "CREATOP",
  "IMAGINE", "OPENART", "SDLEFF", "ST268Q04JA", "ST268Q07JA", "ST268Q01JA",
  "REPEAT", "MISSSC", "SKIPPING", "TARDYSD", "EXERPRAC", "STUDYHMW", "WORKPAY",
  "WORKHOME", "INFOSEEK", "EXPOFA", "EXPO21ST", "CREATAS", "CREATOOS",
  "TEACHSUP", "RELATST", "COGACRCO", "COGACMCO", "DISCLIM", "CREATSCH", "FAMSUP",
  "CREATFAM", "FAMSUPSL", "FEELLAH", "PROBSELF", "LEARRES", "BULLIED",
  "FEELSAFE", "SCHRISK", "BELONG", "SCHSUST", "OCOD3_major_group"
)

data_for_table1 <- data_pre_imputation %>%
  group_by(CNT) %>%
  # 过滤掉在任一关键预测变量上存在100%缺失的国家
  # Filter out countries where any key predictor variable has 100% missing values
  filter(max(across(any_of(predictor_vars), ~mean(is.na(.)))) < 1.0) %>%
  ungroup() %>%
  mutate(
    # 创建性别变量
    # Create a FEMALE variable based on ST004D01T
    FEMALE = as.factor(if_else(ST004D01T == 1, "Female", "Male")),
    # 确保所有分类型变量都被正确识别为因子
    # Ensure all categorical variables are properly treated as factors
    across(c(IMMIG, ISCEDP, REPEAT, EXPECEDU), .fns = ~as.factor(.))
  )

# --- 3. 创建抽样设计对象 ---
# --- 3. Create replicate-weight survey design object ---
design_for_table1 <- svrepdesign(
  weights = ~W_FSTUWT,
  repweights = "W_FSTURWT[0-9]+",
  data = data_for_table1,
  type = "Fay",
  rho = 0.5,
  combined.weights = TRUE
)

# --- 4. 手动计算所有统计量 ---
# --- 4. Manually compute all summary statistics ---
vars_for_table1 <- c(
  "ACADEMIC_RESILIENCE_PV1", "ESCS", "AGE", "IMMIG",
  "ISCEDP", "REPEAT", "EXPECEDU", "STUDYHMW"
)

results_list <- list()

for (var in vars_for_table1) {
  print(paste("Processing variable:", var))

  formula <- as.formula(paste0("~", var))
  is_continuous <- is.numeric(data_for_table1[[var]])

  if (is_continuous) {
    # --- 处理连续变量 ---
    # --- Handle continuous variables ---
    by_gender <- svyby(formula, ~FEMALE, design_for_table1, svymean, na.rm = TRUE)
    by_gender_sd <- sqrt(svyby(formula, ~FEMALE, design_for_table1, svyvar, na.rm = TRUE)[, 2])

    overall <- svymean(formula, design_for_table1, na.rm = TRUE)
    overall_sd <- sqrt(svyvar(formula, design_for_table1, na.rm = TRUE)[1,1])

    p_value <- svyttest(as.formula(paste0(var, " ~ FEMALE")), design_for_table1)$p.value

    results_list[[var]] <- tibble(
      Variable = var,
      Characteristic = "Mean (SD)",
      Overall = sprintf("%.2f (%.2f)", overall[1], overall_sd),
      Female = sprintf("%.2f (%.2f)", by_gender[1, 2], by_gender_sd[1]),
      Male = sprintf("%.2f (%.2f)", by_gender[2, 2], by_gender_sd[2]),
      p_value = p_value
    )

  } else {
    # --- 处理分类型变量 ---
    # --- Handle categorical variables ---
    formula_by_gender <- as.formula(paste0("~", var, " + FEMALE"))

    # 使用列联表计算分组频数
    # Use contingency tables to compute grouped counts
    counts_gender_table <- svytable(formula_by_gender, design_for_table1)

    # 按列（性别）计算百分比
    # Compute column percentages (by gender)
    props_gender_matrix <- proportions(counts_gender_table, margin = 2) * 100

    # 计算总体频数和百分比
    # Compute overall counts and percentages
    overall_counts <- svytable(formula, design_for_table1)
    overall_props <- proportions(overall_counts) * 100

    # 使用Rao-Scott卡方检验计算p值
    # Use Rao-Scott chi-square test to compute p-value
    p_value <- svychisq(formula_by_gender, design_for_table1)$p.value

    # 整理结果到tibble中
    # Format the results into a tibble
    results_list[[var]] <- tibble(
      Variable = var,
      Characteristic = names(overall_counts),
      Overall = sprintf("%.0f (%.1f%%)", overall_counts, overall_props),
      Female = sprintf("%.0f (%.1f%%)", counts_gender_table[, "Female"], props_gender_matrix[, "Female"]),
      Male = sprintf("%.0f (%.1f%%)", counts_gender_table[, "Male"], props_gender_matrix[, "Male"]),
      p_value = p_value
    )
  }
}

# --- 5. 合并结果并保存 ---
# --- 5. Combine results and save as CSV ---
final_table <- bind_rows(results_list)

# 清理p值列，只保留每组变量的第一行
# Clean p-value column: show only for the first row per variable
final_table <- final_table %>%
  group_by(Variable) %>%
  mutate(p_value = if_else(row_number() == 1, p_value, NA_real_)) %>%
  ungroup()

print("Final manually computed summary table:")
print(final_table)

output_csv_path <- here("dataset", "analysis", "Table1_Manual_Descriptive_Statistics.csv")
write_csv(final_table, output_csv_path)

print(paste("Table 1 successfully saved as CSV:", output_csv_path))



# --- 3. Generate Table 2: Formatted Regression Results ---

print("\n--- Generating Table 2: Combined Regression Results ---")

# --- A. Load and combine all model results ---
model_files <- list(
  "Model 1: Controls" = here("dataset", "analysis", "Results_Model_Controls.csv"),
  "Model 2: Full" = here("dataset", "analysis", "Results_Model_Full.csv"),
  "Model 3: Sensitivity" = here("dataset", "analysis", "Results_Model_Sensitivity.csv")
)

all_models_df <- map_dfr(model_files, read_csv, .id = "model")

# --- B. Calculate p-values and format the table ---
formatted_results <- all_models_df %>%
  filter(!str_detect(term, "CNT|Intercept")) %>% # Remove country fixed effects and intercept
  mutate(
    p.value = 2 * pnorm(-abs(t_statistic)),
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    # Combine estimate and significance stars
    estimate_str = paste0(format(round(final_estimate, 3), nsmall = 3), signif),
    # Format standard error for presentation
    stderr_str = paste0("(", format(round(final_std_error, 3), nsmall = 3), ")")
  ) %>%
  # Create a combined column for estimate and SE
  mutate(combined_stat = paste(estimate_str, stderr_str, sep = "\n")) %>%
  select(term, model, combined_stat) %>%
  # Pivot to the wide format required for the table
  pivot_wider(names_from = model, values_from = combined_stat)

# --- C. Create and save the GT table ---
reg_table <- formatted_results %>%
  gt() %>%
  tab_header(
    title = "Table 2: Logistic Regression Models Predicting Academic Resilience",
    subtitle = "Coefficients are log-odds. Standard errors are in parentheses."
  ) %>%
  cols_label(
    term = "Predictor Variable"
  ) %>%
  tab_footnote(
    footnote = "*** p < 0.001; ** p < 0.01; * p < 0.05",
    locations = cells_column_labels(columns = contains("Model"))
  )

output_table2_path <- here("dataset", "analysis", "Table2_Regression_Results.html")
gtsave(reg_table, filename = output_table2_path)

print(paste("Table 2 saved to:", output_table2_path))
print(reg_table)



# --- 4. Generate Figure 1: Coefficient Plot ---

print("\n--- Generating Figure 1: Coefficient Plot ---")

# --- A. Prepare data for plotting (from the full and sensitivity models) ---
plot_data <- all_models_df %>%
  filter(
    model != "Model 1: Controls", # Exclude the controls-only model from the plot
    !str_detect(term, "CNT|Intercept|ESCS|AGE|FEMALE|IMMIG|ISCEDP") # Focus on key predictors
  ) %>%
  mutate(
    conf.low = final_estimate - 1.96 * final_std_error,
    conf.high = final_estimate + 1.96 * final_std_error
  )

# --- B. Create and save the ggplot ---
coefficient_plot <- ggplot(plot_data, aes(x = final_estimate, y = fct_reorder(term, final_estimate), color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 0.8, position = position_dodge(width = 0.5)) +
  labs(
    title = "Predictors of Academic Resilience",
    subtitle = "Points represent log-odds coefficients; lines are 95% confidence intervals.",
    x = "Coefficient (Log-Odds)",
    y = "Predictor Variable",
    color = "Model Specification"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Save the plot
output_figure1_path <- here("dataset", "analysis", "Figure1_Coefficient_Plot.png")
ggsave(output_figure1_path, plot = coefficient_plot, width = 10, height = 12, dpi = 300)

print(paste("Figure 1 saved to:", output_figure1_path))
print(coefficient_plot)