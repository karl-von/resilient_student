# =================================================================
# Step 1: Foundational Data Creation & Resilience Definition (Corrected)
# 步骤 1: 创建基础数据集并定义学业韧性 (修正版)
# =================================================================

# --- 1. Load Libraries ---
# --- 1. 加载程序包 ---

# // English: Load necessary R packages.
# // 中文: 加载所需的R程序包。
library(haven)
library(dplyr)
library(survey)
library(here)
library(progress)

# --- 2. Configuration ---
# --- 2. 配置部分 ---

# // English: Define paths using the 'here' package for portability.
# // 中文: 使用 'here' 包定义路径以增强代码的可移植性。
INPUT_SAV_FILE <- here("dataset", "meta", "CY08MSP_STU_QQQ.SAV")
OUTPUT_RDS_FILE <- here("dataset", "clean_R", "Step1_Resilience_Flags_Corrected.rds")

# --- 3. Load Required Data Columns ---
# --- 3. 加载所需的数据列 ---

# // English: Load IDs, weights, ESCS, all 10 Plausible Values, and all 80 Replicate Weights.
# // 中文: 加载ID、权重、ESCS、全部10个合理值以及全部80个重复权重。
required_cols <- c(
  "CNT", "CNTSCHID", "CNTSTUID",
  "W_FSTUWT",
  "ESCS",
  paste0("PV", 1:10, "MATH"),
  paste0("W_FSTURWT", 1:80)
)

print("Loading specified columns from PISA data...")
pisa_data <- read_sav(INPUT_SAV_FILE, col_select = all_of(required_cols))
print("Data loaded successfully.")

# --- 4. Identify and Filter Countries with Missing ESCS ---
# --- 4. 识别并筛选缺失ESCS数据的国家 ---

print("Checking for countries with all-NA ESCS data...")
problem_countries <- pisa_data %>%
  group_by(CNT) %>%
  summarise(valid_escs_count = sum(!is.na(ESCS)), .groups = 'drop') %>%
  filter(valid_escs_count == 0)

if (nrow(problem_countries) > 0) {
  countries_to_remove <- problem_countries$CNT
  print(paste("Removing", length(countries_to_remove), "countries with no valid ESCS data:", paste(countries_to_remove, collapse=", ")))
  pisa_data_filtered <- pisa_data %>%
    filter(!CNT %in% countries_to_remove)
} else {
  print("No countries with all-NA ESCS found. Proceeding with the full dataset.")
  pisa_data_filtered <- pisa_data
}

# --- 5. Create the FINAL Survey Design Object ---
# --- 5. 创建最终的调查设计对象 ---

print("Creating final survey design object using replicate weights...")
pisa_design_final <- svrepdesign(
  weights = ~W_FSTUWT,
  repweights = "^W_FSTURWT", # Matches all columns that START WITH "W_FSTURWT"
  data = pisa_data_filtered,
  type = "Fay",
  rho = 0.5,
  combined.weights = TRUE
)
print("Final survey design object created successfully.")

# --- 6. Define Disadvantaged Student Sample ---
# --- 6. 定义处境不利学生样本 ---

print("Calculating weighted ESCS thresholds to define disadvantaged students...")
escs_thresholds <- svyby(~ESCS, by = ~CNT, design = pisa_design_final, svyquantile, quantiles = 0.25, ci = FALSE, na.rm = TRUE, keep.var = FALSE)

escs_thresholds <- escs_thresholds %>%
  rename(ESCS_p25_threshold = statistic)

# // English: Create the final data frame, starting with only the disadvantaged students.
# // 中文: 创建最终的数据框，仅保留处境不利的学生作为开始。
disadvantaged_data <- pisa_data_filtered %>%
  left_join(escs_thresholds, by = "CNT") %>%
  filter(!is.na(ESCS_p25_threshold) & ESCS < ESCS_p25_threshold)
print(paste("Created disadvantaged student sample with", nrow(disadvantaged_data), "students."))

# --- 7. Define Resilience Across All 10 Plausible Values ---
# --- 7. 基于全部10个合理值定义学业韧性 ---

# // English: This loop is the correct method. It defines resilience relative to each country's context for each plausible value separately.
# // 中文: 这个循环是正确的方法。它分别针对每一个合理值，并相对于每个国家的具体情况来定义学业韧性。
print("Defining resilience by creating 10 separate flags...")
final_data <- disadvantaged_data

# Initialize Progress Bar
pb <- progress_bar$new(
  format = "  Processing PV [:bar] :percent (ETA: :eta)",
  total = 10, clear = FALSE, width = 60)

for (i in 1:10) {
  pv_name <- paste0("PV", i, "MATH")
  formula <- as.formula(paste0("~", pv_name))

  # // English: Calculate the weighted 75th percentile for the current Plausible Value.
  # // 中文: 计算当前合理值的加权第75个百分位数。
  pv_thresholds <- svyby(formula, by = ~CNT, design = pisa_design_final, svyquantile, quantiles = 0.75, ci = FALSE, na.rm = TRUE, keep.var = FALSE)

  threshold_col_name <- paste0(pv_name, "_p75_threshold")
  pv_thresholds <- pv_thresholds %>%
    rename(!!threshold_col_name := statistic)

  # // English: Merge this specific threshold back into our data.
  # // 中文: 将这个特定的阈值合并回我们的数据中。
  final_data <- final_data %>%
    left_join(pv_thresholds, by = "CNT")

  # // English: Create the final resilience flag for this specific plausible value.
  # // 中文: 为这个特定的合理值创建最终的学业韧性标签。
  resilience_flag_name <- paste0("ACADEMIC_RESILIENCE_PV", i)
  final_data <- final_data %>%
    mutate(
      !!resilience_flag_name := if_else(
        !is.na(!!sym(threshold_col_name)) & !!sym(pv_name) > !!sym(threshold_col_name), 1, 0, missing = 0
      )
    )
  pb$tick() # Update progress bar
}
print("All 10 resilience flags created successfully.")

# --- 8. Final Cleanup and Save ---
# --- 8. 清理并保存最终结果 ---

# // English: Select only the columns needed for the next steps to keep the file clean.
# // 中文: 只选择后续步骤需要的列，以保持文件整洁。
final_data_cleaned <- final_data %>%
  select(
    CNT, CNTSCHID, CNTSTUID, W_FSTUWT, ESCS,
    starts_with("PV"),
    starts_with("ACADEMIC_RESILIENCE_PV"),
    starts_with("W_FSTURWT") # <-- ADD THIS LINE
  )

print(paste("Final dataset has", ncol(final_data_cleaned), "columns and", nrow(final_data_cleaned), "rows."))
saveRDS(final_data_cleaned, file = OUTPUT_RDS_FILE)
print(paste("Success! Correctly defined foundational dataset saved to:", OUTPUT_RDS_FILE))