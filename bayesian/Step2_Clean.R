# =================================================================
# Step 2: Merge and Clean Predictor Variables
# 步骤 2: 合并并清洗预测变量
# =================================================================

# --- 1. Load Libraries ---
# --- 1. 加载程序包 ---
library(haven)
library(dplyr)
library(here)

# --- 2. Configuration ---
# --- 2. 配置部分 ---
INPUT_RDS_STEP1 <- here("dataset", "", "Step1_Resilience.rds")
INPUT_SAV_FILE <- here("dataset", "meta", "CY08MSP_STU_QQQ.SAV")
OUTPUT_RDS_FILE <- here("dataset", "", "Step2_Merged_Cleaned_bayesian.rds")

# --- 3. Define All Predictor Variables ---
# --- 3. 定义所有预测变量 ---

Math_Dispo <- c('ANXMAT', 'MATHEFF', 'MATHEF21', 'MATHPERS', 'ST268Q04JA', 'ST268Q07JA', 'ST268Q01JA')
Social_Emo_Ski <- c('ASSERAGR', 'COOPAGR', 'EMOCOAGR', 'EMPATAGR', 'PERSEVAGR')
Open_Creat <- c('CURIOAGR', 'CREATEFF', 'CREATOP', 'IMAGINE', 'OPENART')
Tea_Class_Exp <- c('TEACHSUP', 'RELATST', 'COGACRCO', 'COGACMCO', 'DISCLIM')
Sch_Exp <- c('FEELSAFE', 'SCHRISK', 'BELONG', 'SCHSUST', 'CREATSCH', 'BULLIED')

# Modified to use ST004D01T instead of FEMALE
# 修改为使用 ST004D01T 而不是 FEMALE
original_model_2_predictors <- c("ESCS", "ST004D01T", "IMMIG", "REPEAT", "MISSSC", "SKIPPING",
                                 "TARDYSD", "STUDYHMW", "WORKPAY", "WORKHOME", "EXPOFA", "EXPO21ST", "STRESAGR", 'SDLEFF', 'GROSAGR')

predictor_cols <- unique(c("CNTSTUID", Math_Dispo, Social_Emo_Ski, Open_Creat, Tea_Class_Exp, Sch_Exp, original_model_2_predictors))

# --- 4. Load and Merge Data ---
# --- 4. 加载并合并数据 ---

print("Loading foundational data from Step 1...")
data_step1 <- readRDS(INPUT_RDS_STEP1)

print("Loading predictor variables from raw PISA data...")
# ESCS is already in the Step 1 data, so we don't load it again. ST004D01T must be loaded.
# ESCS 已在步骤1的数据中，因此我们不再加载它。ST004D01T 必须被加载。
cols_to_load <- setdiff(predictor_cols, "ESCS")

# Removed 'user_na = TRUE' to allow for manual cleaning.
# 移除了 'user_na = TRUE' 以便进行手动清洗。
predictors_data <- read_sav(
  INPUT_SAV_FILE,
  col_select = all_of(cols_to_load)
)

print("Merging foundational data with predictor variables...")
merged_data <- left_join(data_step1, predictors_data, by = "CNTSTUID")
print(paste("Merged dataset created with", nrow(merged_data), "rows and", ncol(merged_data), "columns."))


# --- 5. Manually Clean Invalid Codes ---
# --- 5. 手动清洗无效代码 ---

# // This list defines the specific "invalid" codes for each variable, based on the PISA codebook.
# // 这个列表根据PISA代码手册为每个变量定义了特定的“无效”代码。
invalid_codes_list <- list(
  ST004D01T = c(5, 7, 8, 9), # Gender
  IMMIG     = c(5, 7, 8, 9),
  REPEAT    = c(5, 7, 8, 9),
  MISSSC    = c(5, 7, 8, 9),
  SKIPPING  = c(5, 7, 8, 9),
  TARDYSD   = c(5, 7, 8, 9),
  # // For most other questionnaire scales, 95-99 are the standard invalid codes
  # // (e.g., Not Applicable, Invalid, No Response).
  # // 对于大多数其他问卷量表, 95-99是标准的无效代码 (例如，不适用、无效、无回答)。
  DEFAULT   = c(95, 97, 98, 99)
)

# // Identify variables that need special cleaning vs. default cleaning.
# // 识别需要特殊清洗规则与默认清洗规则的变量。
vars_special_cleaning <- names(invalid_codes_list)[names(invalid_codes_list) != "DEFAULT"]
all_predictors_in_data <- setdiff(predictor_cols, c("CNTSTUID", "ESCS"))
vars_default_cleaning <- setdiff(all_predictors_in_data, vars_special_cleaning)

print("Cleaning invalid codes for all predictor variables using the manual list...")
data_cleaned <- merged_data %>%
  mutate(
    # Clean variables with specific invalid codes defined in the list
    across(all_of(intersect(vars_special_cleaning, names(.))),
           ~ if_else(.x %in% invalid_codes_list[[cur_column()]], NA_real_, .x)),

    # Clean all other questionnaire variables using the default invalid codes
    across(all_of(intersect(vars_default_cleaning, names(.))),
           ~ if_else(.x %in% invalid_codes_list$DEFAULT, NA_real_, .x))
  )
print("Invalid codes replaced with NA.")


# --- 6. Diagnose Structural Missingness---
# --- 6. 诊断结构性缺失 ---
print("Diagnosing structural missingness...")
missing_report <- data_cleaned %>%
  group_by(CNT) %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  tidyr::pivot_longer(cols = -CNT, names_to = "variable", values_to = "missing_pct") %>%
  filter(missing_pct > 99)

if(nrow(missing_report) > 0) {
  print("WARNING: Structural missingness detected...")
  print(missing_report, n = 20)
  print("ACTION REQUIRED: You must now decide on your final analytical sample...")
} else {
  print("No major structural missingness detected across variables.")
}


# --- 7. Save the Pre-Imputation Dataset ---
# --- 7. 保存插补前的数据集 ---
print("Removing temporary threshold columns...")
data_cleaned <- data_cleaned %>%
  select(-ends_with("_p75_threshold"))

print("Saving clean, merged dataset ready for imputation...")
saveRDS(data_cleaned, file = OUTPUT_RDS_FILE)
print(paste("Success! File ready for Step 3 saved to:", OUTPUT_RDS_FILE))