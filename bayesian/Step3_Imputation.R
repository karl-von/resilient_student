# =================================================================
# Step 3: Final Sample Selection & MULTILEVEL Multiple Imputation
# 步骤 3: 最终样本选择与多层次多重插补
# =================================================================


# --- 1. Load Libraries and Data ---
# --- 1. 加载程序包和数据 ---
library(dplyr)
library(here)
library(mice)
library(miceadds)
library(haven) # Added for the 'is.labelled' check

# --- Define File Paths ---
INPUT_RDS_STEP2 <- here("dataset", "Step2_Merged_Cleaned_final.rds")
OUTPUT_RDS_MICE <- here("dataset", "", "Step3_Imputed_Mice_Object_bayesian.rds")
OUTPUT_RDS_LIST <- here("dataset", "", "Step3_Imputed_Data_List_bayesian.rds")

print("Loading cleaned data from Step 2...")
data_cleaned <- readRDS(INPUT_RDS_STEP2)


# --- 2. Identify Final Analytical Sample ---
# --- 2. 确定最终分析样本 ---

# // This section is modified to dynamically identify predictor variables
# // instead of using a fixed, hardcoded list.
# // 本节经过修改，以动态识别预测变量，而不是使用固定的硬编码列表。

print("Dynamically identifying predictor variables for sample selection...")

# <<< MODIFIED SECTION START >>>

# --- Step A: Define variables that are NOT predictors ---
# --- 步骤 A: 定义不属于预测变量的变量 ---
all_vars <- names(data_cleaned)
id_vars <- c("CNT", "CNTSCHID", "CNTSTUID")
weight_vars <- all_vars[startsWith(all_vars, "W_FSTURWT") | all_vars == "W_FSTUWT"]
outcome_vars <- all_vars[startsWith(all_vars, "RESILIENCE_SCORE_PV") | startsWith(all_vars, "PV")]

# --- Step B: Predictors are all other variables ---
# --- 步骤 B: 预测变量是所有其余的变量 ---
predictor_vars <- setdiff(all_vars, c(id_vars, weight_vars, outcome_vars))

print(paste("Identified", length(predictor_vars), "predictor variables to check for missingness."))

# --- Step C: Identify countries with 100% missing data on any predictor ---
# --- 步骤 C: 识别在任何预测变量上存在100%缺失数据的国家 ---
problematic_country_list <- data_cleaned %>%
  group_by(CNT) %>%
  # Check the max missing % across our dynamically identified predictors
  # 在我们动态识别的预测变量中检查最大缺失百分比
  summarise(max_missing_pct = max(across(all_of(predictor_vars), ~mean(is.na(.)))), .groups = 'drop') %>%
  filter(max_missing_pct >= 1.0) %>%
  pull(CNT) %>%
  unique()

# <<< MODIFIED SECTION END >>>

all_countries <- unique(data_cleaned$CNT)
countries_to_keep <- setdiff(all_countries, problematic_country_list)

print(paste("Identified", length(countries_to_keep), "countries for the final analytical sample."))

# Filter to the bayesian analytical sample
data_for_imputation <- data_cleaned %>%
  filter(CNT %in% countries_to_keep) %>%
  mutate(
    CNTSCHID = as.integer(as.factor(CNTSCHID)),
    CNT = as.factor(CNT),
    # Convert other potential categorical variables to factors. Add any others here.
    # Note: Many variables like IMMIG might already be numeric after Step 2.
    across(any_of(c("ST004D01T", "IMMIG")), as.factor)
  ) %>%
  # This line handles any remaining SPSS labelled columns robustly.
  mutate(across(where(haven::is.labelled), haven::zap_labels))
  # mutate(across(where(haven::is.labelled), as.numeric))

# --- ADD THIS DEBUGGING SNIPPET ---
print("--- Full list of variables with missing data before exclusions ---")
missing_counts <- colSums(is.na(data_for_imputation))
variables_with_missing_data <- missing_counts[missing_counts > 0]
print(variables_with_missing_data)
print("----------------------------------------------------------------")
# --- END OF DEBUGGING SNIPPET ---

# --- 3. Run MULTILEVEL Multiple Imputation using mice ---
# --- 3. 使用 mice 运行多层次多重插补 ---
# This section is well-structured and does not need changes. It already
# uses dynamic logic (e.g., startsWith) to build the imputation model.
# 这部分结构良好，无需更改。它已经使用动态逻辑 (例如, startsWith) 来构建插补模型。

print("Setting up for MULTILEVEL imputation...")

# --- A. Get all variable names from the bayesian dataset ---
all_column_names <- names(data_for_imputation)

# --- B. Initialize mice to get the configuration ---
ini <- mice(data_for_imputation, maxit = 0)
pred_matrix <- ini$predictorMatrix
meth_vector <- ini$method

# --- C. Define variables that should NOT BE IMPUTED ---
vars_to_not_impute <- c(
  "CNTSTUID", "CNT", "CNTSCHID",
  "W_FSTUWT",
  all_column_names[startsWith(all_column_names, "W_FSTURWT")],
  all_column_names[startsWith(all_column_names, "RESILIENCE_SCORE_PV")],
  all_column_names[startsWith(all_column_names, "PV")]
)
meth_vector[intersect(vars_to_not_impute, names(meth_vector))] <- ""

# --- D. Define variables that should NOT BE USED AS PREDICTORS ---
vars_to_not_use_as_predictors <- c(
  "CNTSTUID", "CNT",
  all_column_names[startsWith(all_column_names, "W_FSTURWT")],
  all_column_names[startsWith(all_column_names, "RESILIENCE_SCORE_PV")]
)
pred_matrix[, intersect(vars_to_not_use_as_predictors, colnames(pred_matrix))] <- 0

# --- E. Specify the multilevel structure ---
pred_matrix[, "CNTSCHID"] <- -2

# --- F. Set the two-level imputation method ---
vars_to_impute <- names(meth_vector[meth_vector != ""])
meth_vector[vars_to_impute] <- "2l.pmm"

# --- G. Run the bayesian imputation ---
print("Starting the imputation process. This may take a long time...")
print("Summary of missing data before imputation:")
print(colSums(is.na(data_for_imputation))[colSums(is.na(data_for_imputation)) > 0])

imputed_object <- mice(
  data_for_imputation,
  m = 5,
  maxit = 30, # Consider a lower number like 10-15 for faster runs
  predictorMatrix = pred_matrix,
  method = meth_vector,
  seed = 12345
)

print("Multilevel multiple imputation complete.")

# --- 4. Bundle IDs and Imputed Data and Save ---
# --- 4. 捆绑ID和插补数据并保存 ---
# This section is correct and does not need changes.
# 这部分是正确的，无需更改。

# --- PART A: Save the original MICE object and IDs ---
print("Bundling IDs and imputed data into a single object...")
ids_df <- data_for_imputation %>%
  select(all_of(c("CNTSTUID", "CNTSCHID", "CNT")))

output_to_save <- list(
  mids_object = imputed_object,
  ids = ids_df
)
saveRDS(output_to_save, file = OUTPUT_RDS_MICE)
print(paste("Success! Bundled multilevel MICE object saved to:", OUTPUT_RDS_MICE))


# --- PART B: Convert MICE object to a simple list and save ---
print("Converting MICE object to a simple list of 5 dataframes for easy analysis...")
list_of_imputed_datasets <- list()
for (i in 1:imputed_object$m) {
  list_of_imputed_datasets[[i]] <- mice::complete(imputed_object, i)
}
saveRDS(list_of_imputed_datasets, file = OUTPUT_RDS_LIST)
print(paste("Success! List of 5 imputed dataframes saved to:", OUTPUT_RDS_LIST))