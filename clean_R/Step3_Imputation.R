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

# Select all columns except the IDs for the imputation model.
# 为插补模型选择除ID之外的所有列。
imputation_subset <- data_for_imputation %>%
  select(-CNT, -CNTSCHID, -CNTSTUID) %>%

  # This removes the special class that causes the error in mice.
  # 将所有 'haven_labelled' 列转换为标准的数值向量。
  # 这样做可以移除导致 mice 出错的特殊类。
  mutate(across(where(haven::is.labelled), as.numeric)) %>%

  mutate(across(where(is.character), as.factor)) # Convert characters to factors

print("Starting multiple imputation... This may take a long time.")
# m=5 creates 5 complete datasets. maxit=10 means 10 iterations per dataset.
# Setting a seed is crucial for reproducibility.
# m=5表示创建5个完整数据集。maxit=10表示每个数据集迭代10次。
# 设置随机种子(seed)对保证结果可重复至关重要。
imputed_object <- mice(imputation_subset, m = 5, maxit = 10, method = 'pmm', seed = 12345)
print("Multiple imputation complete.")


# --- 4. NEW: Check Imputation Performance ---
# --- 4. 新增: 检查插补性能 ---

print("Generating diagnostic plots to check imputation quality...")

# Create the directory for plots if it doesn't exist
# 如果绘图目录不存在，则创建它
if (!dir.exists(OUTPUT_PLOT_DIR)) {
  dir.create(OUTPUT_PLOT_DIR, recursive = TRUE)
}

# 4a. Convergence Plot
# 4a. 收敛图
# Look for the lines to be mixed randomly (like a "fuzzy caterpillar") with no clear trend.
# 观察这些线是否随机混合（像一条“毛茸茸的毛毛虫”），没有明显的上升或下降趋势。
png(file.path(OUTPUT_PLOT_DIR, "mice_convergence_plot.png"), width=1200, height=800)
plot(imputed_object)
dev.off()
print("Convergence plot saved.")

# 4b. Distribution Plots
# 4b. 分布图
# The distribution of the imputed data (red) should be similar to the observed data (blue).
# 插补数据（红色）的分布应与观测数据（蓝色）的分布相似。
png(file.path(OUTPUT_PLOT_DIR, "mice_density_plot.png"), width=1200, height=800)
densityplot(imputed_object)
dev.off()
print("Density plot saved.")

# You can also check individual variables
# 你也可以检查单个变量
# Example for ANXMAT
# ANXMAT的例子
# png(file.path(OUTPUT_PLOT_DIR, "mice_density_ANXMAT.png"), width=800, height=600)
# densityplot(imputed_object, ~ANXMAT)
# dev.off()


# --- 5. Save the Imputed Object ---
# --- 5. 保存插补后的对象 ---

print("Saving the MICE object...")
# We save the entire 'mids' object, which contains all 5 imputed datasets.
# 我们保存整个'mids'对象，它包含所有5个插补数据集。
saveRDS(imputed_object, file = OUTPUT_RDS_MICE)
print(paste("Success! MICE object saved to:", OUTPUT_RDS_MICE))