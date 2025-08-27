# =================================================================
# Diagnostic Script: Preview Final Sample and Missing Data
# 诊断脚本: 预览最终样本和缺失数据
# =================================================================

# --- 1. Load Libraries ---
# --- 1. 加载程序包 ---
library(dplyr)
library(here)
library(tidyr)

# --- 2. Configuration ---
# --- 2. 配置部分 ---

# // Define paths for the outputs of Step 1 and Step 2.
# // 定义步骤1和步骤2的输出文件路径。
INPUT_RDS_STEP1 <- here("dataset", "", "Step1_Resilience.rds")
INPUT_RDS_STEP2 <- here("dataset", "", "Step2_Merged_Cleaned_bayesian.rds")

# --- 3. Load and Merge Data ---

print("Loading data from Step 1 (for resilience scores)...")
resilience_info <- readRDS(INPUT_RDS_STEP1) %>%
  select(CNTSTUID, starts_with("RESILIENCE_SCORE_PV"))

print("Loading data from Step 2 (for predictors)...")
data_cleaned <- readRDS(INPUT_RDS_STEP2)

print("Merging the two datasets...")
merged_data <- inner_join(data_cleaned, resilience_info, by = "CNTSTUID") %>%
  mutate(CNT = as.character(haven::as_factor(CNT)))

#
# print("Merging the two datasets...")
# merged_data <- inner_join(data_cleaned, resilience_info, by = "CNTSTUID")

# The rest of your script, including the haven_labelled fix...
merged_data <- merged_data %>%
  mutate(CNT = as.character(haven::as_factor(CNT)))

# --- 4. Replicate the Filtering Logic from Step 3 ---
# --- 4. 重现步骤3中的筛选逻辑 ---

print("Replicating Step 3's logic to find countries for the final sample...")

# Dynamically identify predictor variables
all_vars <- names(merged_data)
id_vars <- c("CNT", "CNTSCHID", "CNTSTUID")
weight_vars <- all_vars[startsWith(all_vars, "W_FSTURWT") | all_vars == "W_FSTUWT"]
outcome_vars <- all_vars[startsWith(all_vars, "RESILIENCE_SCORE_PV") | startsWith(all_vars, "PV")]
predictor_vars <- setdiff(all_vars, c(id_vars, weight_vars, outcome_vars))

# Identify countries with 100% missing data on any predictor
problematic_country_list <- merged_data %>%
  group_by(CNT) %>%
  summarise(max_missing_pct = max(across(all_of(predictor_vars), ~mean(is.na(.)))), .groups = 'drop') %>%
  filter(max_missing_pct >= 1.0) %>%
  pull(CNT) %>%
  unique()

# Determine the final list of countries to be included
all_countries <- unique(merged_data$CNT)
countries_to_keep <- sort(setdiff(all_countries, problematic_country_list))

print(paste("The analysis will include", length(countries_to_keep), "countries."))

# Filter the merged data to the final analytical sample
final_sample_data <- merged_data %>% filter(CNT %in% countries_to_keep)


# --- 5. Define Resilience for the Final Sample ---
# --- 5. 为最终样本定义学业韧性 ---

final_sample_with_resilience <- final_sample_data %>%
  mutate(
    AVG_RESILIENCE_SCORE = rowMeans(select(., starts_with("RESILIENCE_SCORE_PV")), na.rm = TRUE),
    IS_RESILIENT = if_else(AVG_RESILIENCE_SCORE > 0, 1, 0)
  )

# --- 6. Generate Per-Country Reports (Revised with a more robust method) ---

cat("\n\n=================================================================\n")
cat("          DETAILED REPORT FOR EACH INCLUDED COUNTRY          \n")
cat("=================================================================\n\n")

# Use group_split to create a list of dataframes, one for each country.
# This avoids the filtering problem inside the loop.
list_of_country_data <- final_sample_with_resilience %>%
  group_split(CNT)

# Now, loop through this list of dataframes.
for (country_data in list_of_country_data) {

  # Get the country code directly from the current dataframe subset.
  country_code <- country_data$CNT[1]

  # --- The rest of your original loop code is now safe to use ---

  # Calculate student and resilience stats
  num_students <- nrow(country_data)
  num_resilient <- sum(country_data$IS_RESILIENT, na.rm = TRUE)
  pct_resilient <- (num_resilient / num_students) * 100

  # Calculate missing value statistics
  missing_stats <- country_data %>%
    summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
    filter(missing_pct > 0) %>%
    arrange(desc(missing_pct))

  # Print the report for the country
  cat(paste0("--- Report for ", country_code, " ---\n"))
  cat(sprintf("Number of Students: %d\n", num_students))
  cat(sprintf("Number of Resilient Students: %d\n", num_resilient))
  cat(sprintf("Percentage of Resilient Students: %.2f%%\n", pct_resilient))

  if (nrow(missing_stats) > 0) {
    cat("\nVariables with Missing Values:\n")
    print(as.data.frame(missing_stats))
  } else {
    cat("\nNo missing values found in any variable for this country.\n")
  }
  cat("\n-------------------------------------------------\n\n")
}

# --- 7. Generate Overall Summary Report ---
# --- 7. 生成总体摘要报告 ---
cat("\n\n=================================================================\n")
cat("                    OVERALL SUMMARY REPORT                   \n")
cat("=================================================================\n\n")

total_students <- nrow(final_sample_with_resilience)
total_resilient <- sum(final_sample_with_resilience$IS_RESILIENT, na.rm = TRUE)
overall_pct_resilient <- (total_resilient / total_students) * 100

cat(sprintf("Total Number of Countries Included: %d\n", length(countries_to_keep)))
cat("--------------------------------------\n")
cat(sprintf("Total Number of Students: %d\n", total_students))
cat(sprintf("Total Number of Resilient Students: %d\n", total_resilient))
cat(sprintf("Overall Percentage of Resilient Students: %.2f%%\n", overall_pct_resilient))
cat("\n=================================================================\n")