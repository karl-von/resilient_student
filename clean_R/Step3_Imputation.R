# =================================================================

# Step 3: Final Sample Selection & Multiple Imputation (MICE)

# 步骤 3: 最终样本选择与多重插补 (MICE)

# =================================================================


# --- 1. Load Libraries and Data ---

# --- 1. 加载程序包和数据 ---
library(dplyr)
library(here)
library(mice)
library(haven)
library(writexl)
# Define file paths

# 定义文件路径

INPUT_RDS_STEP2 <- here("dataset", "clean_R", "Step2_Merged_Cleaned_for_Imputation.rds")

OUTPUT_RDS_MICE <- here("dataset", "clean_R", "Step3_Imputed_Mice_Object.rds")

OUTPUT_PLOT_DIR <- here("dataset", "analysis", "imputation_diagnostics")


print("Loading cleaned data from Step 2...")

data_cleaned <- readRDS(INPUT_RDS_STEP2)


# --- 2. Automatically Identify Countries with Complete Data ---

# --- 2. 自动识别拥有完整数据的国家 ---


# Define the list of all potential predictors to check for structural missingness

# 定义需要检查是否存在结构性缺失的所有潜在预测变量

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

predictor_vars_exist <- predictor_vars[predictor_vars %in% names(data_cleaned)]


# Find countries that have NO variables that are 100% missing

# 查找没有任何变量是100%缺失的国家

print("Identifying countries with no structural missingness...")

problematic_country_list <- data_cleaned %>%

  group_by(CNT) %>%

  summarise(max_missing_pct = max(across(all_of(predictor_vars_exist), ~mean(is.na(.)))), .groups = 'drop') %>%

  filter(max_missing_pct >= 1.0) %>%

  pull(CNT) %>%

  unique()


all_countries <- unique(data_cleaned$CNT)

countries_to_keep <- setdiff(all_countries, problematic_country_list)


print(paste("Identified", length(countries_to_keep), "countries for the final analytical sample."))


# Filter to the final analytical sample

# 筛选出最终的分析样本

data_for_imputation <- data_cleaned %>%

  filter(CNT %in% countries_to_keep)


# --- 3. Run the MICE Imputation ---
# --- 3. 运行MICE插补 ---

# --- Run the MICE Imputation ---

# The imputation subset correctly excludes IDs

imputation_subset <- data_for_imputation %>%

  select(

    -CNT, -CNTSCHID, -CNTSTUID,

    -W_FSTUWT, -starts_with("W_FSTURWT"),

    -starts_with("ACADEMIC_RESILIENCE_PV"),

    -ends_with("_p75_threshold")

  ) %>%

  mutate(across(where(haven::is.labelled), as.numeric)) %>%

  mutate(across(where(is.character), as.factor))

print("Starting multiple imputation...")

imputed_object <- mice(imputation_subset, m = 5, maxit = 10, method = 'pmm', seed = 12345)

print("Multiple imputation complete.")

saveRDS(imputed_object, file = OUTPUT_RDS_MICE)

print(paste("Success! MICE object saved to:", OUTPUT_RDS_MICE))


# --- 6. Export Imputed Datasets to Excel (with IDs) ---
print("Exporting completed datasets to Excel...")
### --- MODIFICATION IS HERE --- ###
# 1. First, create a clean dataframe of just the ID columns.
# This ensures the rows are perfectly aligned with the imputed data.
ids_to_add <- data_for_imputation %>% select(CNT, CNTSCHID, CNTSTUID)

# 2. Create a list to hold each of the 5 imputed dataframes
list_of_completed_datasets <- list()

for (i in 1:imputed_object$m) {
  # Get the i-th completed dataset (this one has no IDs)
  dataset <- complete(imputed_object, i)
  # Add the IDs back to the front of the dataset
  dataset_with_ids <- bind_cols(ids_to_add, dataset)
  # Add the version WITH IDs to the list
  list_of_completed_datasets[[paste0("Imputation_", i)]] <- dataset_with_ids

}

# 3. Define the output file path and write to Excel
OUTPUT_XLSX <- file.path(OUTPUT_PLOT_DIR, "Step3_Imputation_Results_with_IDs.xlsx")
write_xlsx(list_of_completed_datasets, path = OUTPUT_XLSX)
print(paste("Success! Imputed datasets WITH IDs saved to:", OUTPUT_XLSX))

print("Bundling IDs and imputed data into a single object...")

# A. Create a clean dataframe of just the ID columns.
# This ensures the rows are perfectly aligned with the imputed data.
ids_df <- data_for_imputation %>% select(CNTSTUID, CNTSCHID)

# B. Create a list to hold both the mids object and the IDs
output_to_save <- list(
  mids_object = imputed_object,
  ids = ids_df
)

# --- 5. Save the Bundled Object ---
# We now save the single list object.
saveRDS(output_to_save, file = OUTPUT_RDS_MICE)
print(paste("Success! Bundled MICE object and IDs saved to:", OUTPUT_RDS_MICE))