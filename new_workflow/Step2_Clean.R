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

# // Define input files (from Step 1 and the raw data) and the output file for this step.
# // 定义输入文件 (来自步骤1和原始数据) 以及此步骤的输出文件。
INPUT_RDS_STEP1 <- here("dataset", "", "Step1_Resilience.rds")
INPUT_SAV_FILE <- here("dataset", "meta", "CY08MSP_STU_QQQ.SAV")
OUTPUT_RDS_FILE <- here("dataset", "", "Step2_Merged_Cleaned.rds")

# --- 3. Define All Predictor Variables ---
# --- 3. 定义所有预测变量 ---

# // This list contains all the student, teacher, and school variables from research plan.
# // 这个列表包含了研究计划中所有的学生、教师和学校层面的变量。
predictor_cols <- c(
  "CNTSTUID",
  # Background Control Variables
  "ST004D01T", "AGE", "ISCEDP", "IMMIG",
  # Student Psychological Variables
  "EXPECEDU", "OCOD3", "BSMJ", "SISCO", "GROSAGR", "ANXMAT", "MATHEFF",
  "MATHEF21", "MATHPERS", "ASSERAGR", "COOPAGR", "CURIOAGR", "EMOCOAGR",
  "EMPATAGR", "PERSEVAGR", "STRESAGR", "CREATEFF", "CREATOP", "IMAGINE",
  "OPENART", "SDLEFF", "ST268Q04JA", "ST268Q07JA", "ST268Q01JA",
  # Student Practice Variables
  "REPEAT", "MISSSC", "SKIPPING", "TARDYSD", "EXERPRAC", "STUDYHMW",
  "WORKPAY", "WORKHOME", "INFOSEEK", "EXPOFA", "EXPO21ST", "CREATAS", "CREATOOS",
  # Teacher & Classroom & Home Experience
  "TEACHSUP", "RELATST", "COGACRCO", "COGACMCO", "DISCLIM", "CREATSCH",
  "FAMSUP", "CREATFAM", "FAMSUPSL", "FEELLAH", "PROBSELF", "LEARRES",
  # School Experience Variables
  "BULLIED", "FEELSAFE", "SCHRISK", "BELONG", "SCHSUST"
)

# --- 4. Load and Merge Data ---
# --- 4. 加载并合并数据 ---

# // Load the foundational dataset created in Step 1.
# // 加载步骤1中创建的基础数据集。
print("Loading foundational data from Step 1...")
data_step1 <- readRDS(INPUT_RDS_STEP1)

# // Load the predictor variables from the raw PISA file.
# // 从原始PISA文件中加载预测变量。
print("Loading predictor variables from raw PISA data...")
predictors_data <- read_sav(INPUT_SAV_FILE, col_select = all_of(predictor_cols))

# // Merge the two datasets using the unique student ID.
# // 使用唯一的学生ID合并两个数据集。
print("Merging foundational data with predictor variables...")
merged_data <- left_join(data_step1, predictors_data, by = "CNTSTUID")
print(paste("Merged dataset created with", nrow(merged_data), "rows and", ncol(merged_data), "columns."))

# --- 5. Clean Invalid Codes and Feature Engineering ---
# --- 5. 清洗无效代码并进行特征工程  ---

# // This list defines the specific "invalid" codes for each variable, based on the PISA codebook.
# // 这个列表根据PISA代码手册为每个变量定义了特定的“无效”代码。
invalid_codes_list <- list(
  ST004D01T = c(5, 7, 8, 9), AGE = c(9995, 9997, 9998, 9999), ISCEDP = 999,
  IMMIG = c(5, 7, 8, 9), EXPECEDU = c(95, 97, 98, 99), BSMJ = c(95, 97, 98, 99),
  SISCO = c(5, 7, 8, 9), REPEAT = c(5, 7, 8, 9), MISSSC = c(5, 7, 8, 9),
  SKIPPING = c(5, 7, 8, 9), TARDYSD = c(5, 7, 8, 9),
  # // For most other scales, 95-99 are the standard invalid codes
  # // 对于大多数其他量表, 95-99是标准的无效代码
  DEFAULT = c(95, 97, 98, 99)
)

print("Cleaning invalid codes for all predictor variables...")
data_cleaned <- merged_data %>%
  mutate(
    # --- Part A: Clean all standard variables (your original code is good) ---
    across(all_of(names(invalid_codes_list)[-which(names(invalid_codes_list) == "DEFAULT")]),
           ~ if_else(.x %in% invalid_codes_list[[cur_column()]], NA_real_, .x)),

    across(c(GROSAGR, ANXMAT, MATHEFF, MATHEF21, MATHPERS, ASSERAGR, COOPAGR, CURIOAGR,
             EMOCOAGR, EMPATAGR, PERSEVAGR, STRESAGR, CREATEFF, CREATOP, IMAGINE,
             OPENART, SDLEFF, ST268Q04JA, ST268Q07JA, ST268Q01JA, EXERPRAC, STUDYHMW,
             WORKPAY, WORKHOME, INFOSEEK, EXPOFA, EXPO21ST, CREATAS, CREATOOS, TEACHSUP,
             RELATST, COGACRCO, COGACMCO, DISCLIM, CREATSCH, FAMSUP, CREATFAM, FAMSUPSL,
             FEELLAH, PROBSELF, LEARRES, BULLIED, FEELSAFE, SCHRISK, BELONG, SCHSUST),
           ~ if_else(.x %in% invalid_codes_list$DEFAULT, NA_real_, .x)),

    # --- Part B: Clean OCOD3 and Engineer the New Feature ---

    OCOD3_cleaned = case_when(
      OCOD3 %in% c("9997", "9998", "9999", 9997, 9998, 9999) ~ NA_character_,
      TRUE ~ as.character(OCOD3) # Ensure it's a character for the next step
    ),

    OCOD3_numeric = as.numeric(OCOD3_cleaned),

    OCOD3_major_group = floor(OCOD3_numeric / 1000)
  ) %>%
  # --- Part C: Drop the original and intermediate OCOD3 variables ---
  select(-OCOD3, -OCOD3_cleaned, -OCOD3_numeric)

print("Invalid codes replaced with NA and features engineered.")

# --- 6. Diagnose Structural Missingness---
# --- 6. 诊断结构性缺失 ---

# // This calculates the missing percentage for each variable by country.
# // It helps to identify which countries may not have administered certain modules.
# // 这段代码计算每个变量在各个国家的缺失百分比。
# // 帮助识别哪些国家可能没有实施某些问卷模块。
print("Diagnosing structural missingness...")
missing_report <- data_cleaned %>%
  group_by(CNT) %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  tidyr::pivot_longer(cols = -CNT, names_to = "variable", values_to = "missing_pct") %>%
  filter(missing_pct > 99) # // Show variables that are essentially 100% missing / 显示基本100%缺失的变量

if(nrow(missing_report) > 0) {
  print("WARNING: Structural missingness detected. The following variables are entirely missing in some countries:")
  print(missing_report, n = 20) # // Print the first 20 cases / 打印前20个案例
  print("ACTION REQUIRED: You must now decide on your final analytical sample. Remove countries that did not administer variables essential to your research question before proceeding to Step 3 (Imputation).")
} else {
  print("No major structural missingness detected across variables. You may proceed with all countries in your sample.")
}



# --- 7. Save the Pre-Imputation Dataset ---
# --- 7. 保存插补前的数据集 ---
print("Removing temporary threshold columns...")
# This selects all columns EXCEPT those ending in "_p75_threshold"
data_cleaned <- data_cleaned %>%
  select(-ends_with("_p75_threshold"))

print("Saving clean, merged dataset ready for imputation...")
saveRDS(data_cleaned, file = OUTPUT_RDS_FILE)
print(paste("Success! File ready for Step 3 saved to:", OUTPUT_RDS_FILE))