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
OUTPUT_RDS_FILE <- here("dataset", "", "Step2_Merged_Cleaned_final.rds")

# --- 3. Define All Predictor Variables ---
# --- 3. 定义所有预测变量 ---

Math_Dispo <- c('ANXMAT', 'MATHEFF', 'MATHEF21', 'MATHPERS', 'ST268Q04JA', 'ST268Q07JA', 'ST268Q01JA')
Social_Emo_Ski <- c('ASSERAGR', 'COOPAGR', 'EMOCOAGR', 'EMPATAGR', 'PERSEVAGR')
Open_Creat <- c('CURIOAGR', 'CREATEFF', 'CREATOP', 'IMAGINE', 'OPENART')
Tea_Class_Exp <- c('TEACHSUP', 'RELATST', 'COGACRCO', 'COGACMCO', 'DISCLIM')
Sch_Exp <- c('FEELSAFE', 'SCHRISK', 'BELONG', 'SCHSUST', 'CREATSCH', 'BULLIED')
original_model_2_predictors <- c("ESCS", "FEMALE", "IMMIG", "REPEAT", "MISSSC", "SKIPPING", "TARDYSD", "STUDYHMW", "WORKPAY", "EXPOFA", "EXPO21ST", "STRESAGR",'SDLEFF', 'GROSAGR')

predictor_cols <- unique(c("CNTSTUID", Math_Dispo, Social_Emo_Ski, Open_Creat, Self_Dir_Lear, Tea_Class_Exp, Sch_Exp, original_model_2_predictors))

# --- 4. Load, Merge, and Automatically Clean Data ---
# --- 4. 加载、合并并自动清洗数据 ---

print("Loading foundational data from Step 1...")
data_step1 <- readRDS(INPUT_RDS_STEP1)

print("Loading predictor variables and automatically converting user-defined missing to NA...")
cols_to_load <- setdiff(predictor_cols, c("ESCS", "FEMALE"))

# The key change is adding user_na = TRUE
# 关键的改动是加上 user_na = TRUE
predictors_data <- read_sav(
  INPUT_SAV_FILE,
  col_select = all_of(cols_to_load),
  user_na = TRUE
)

print("Merging foundational data with predictor variables...")
merged_data <- left_join(data_step1, predictors_data, by = "CNTSTUID")
print(paste("Merged dataset created with", nrow(merged_data), "rows and", ncol(merged_data), "columns."))


# --- 5. Clean Invalid Codes (SECTION REMOVED) ---
# --- 5. 清洗无效代码 (此部分已移除) ---

# // This entire section is no longer needed because `user_na = TRUE` in the
# // read_sav() function handles the conversion of invalid codes automatically.
# // This is a more reliable and efficient method.
# // 整个部分都不再需要, 因为 read_sav() 函数中的 `user_na = TRUE` 参数
# // 会自动处理无效代码的转换。这是一个更可靠、更高效的方法。
data_cleaned <- merged_data


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