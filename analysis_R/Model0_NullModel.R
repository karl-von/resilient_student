# =================================================================
# Analysis Step A: Null Model & ICC (Robust Linear Approach)
# 分析步骤 A: 零模型与ICC (稳健的线性模型方法)
# =================================================================

# --- 1. Load Libraries and Data ---
# --- 1. 加载程序包和数据 ---
library(lme4)
library(mitml)
library(dplyr)
library(here)

INPUT_MODEL_DATA <- here("dataset", "clean_R", "Step5_Final_Model_Ready_List.rds")

print("Loading the list of 10 model-ready datasets...")
model_ready_list <- readRDS(INPUT_MODEL_DATA)
print("Data loaded successfully.")

# --- 2. Correct Data Types for Modeling ---
# --- 2. 修正用于建模的数据类型 ---
print("Converting grouping variables to standard R factors...")
model_ready_list <- lapply(model_ready_list, function(df) {
  df <- df %>%
    mutate(
      CNT = as.factor(CNT),
      CNTSCHID = as.factor(CNTSCHID)
    )
  return(df)
})
print("Data types corrected.")


# --- 3. Fit the Null Model using a Linear Model (lmer) for Stability ---
# --- 3. 使用线性模型(lmer)拟合零模型以保证稳定性 ---

# We use a linear model (lmer) because it's more stable for estimating variance components
# when the outcome is sparse, which is common in a logistic model. This will give a reliable ICC.
# 我们使用线性模型(lmer)因为它在估算方差成分时更稳定，
# 尤其是在逻辑模型中当结果变量很稀疏时。这将为我们提供一个可靠的ICC。
print("Fitting the three-level null model using lmer for stability...")

null_models <- with.mitml.list(
  model_ready_list,
  lmer(RESILIENCE ~ 1 + (1 | CNT / CNTSCHID), # The nested formula is correct
       weights = W_FSTUWT)
)
print("All 10 null models fitted successfully.")


# --- 4. Pool Variance Components and Calculate ICC ---
# --- 4. 合并方差成分并计算ICC ---
print("Pooling variance components by averaging...")
model_variances <- lapply(null_models, function(model) {
  variances <- as.data.frame(VarCorr(model))
  var_school <- variances$vcov[variances$grp == "CNTSCHID:CNT"] # School nested in country
  var_country <- variances$vcov[variances$grp == "CNT"]
  var_residual <- variances$vcov[variances$grp == "Residual"] # lmer estimates the residual variance
  return(c(var_school = var_school, var_country = var_country, var_residual = var_residual))
})

pooled_variances <- colMeans(do.call(rbind, model_variances))
var_school_pooled <- pooled_variances["var_school"]
var_country_pooled <- pooled_variances["var_country"]
var_residual_pooled <- pooled_variances["var_residual"]

# Calculate the ICCs using the new variance components
# 使用新的方差成分计算ICC
total_variance <- var_country_pooled + var_school_pooled + var_residual_pooled
icc_school <- (var_country_pooled + var_school_pooled) / total_variance
icc_country <- var_country_pooled / total_variance

# --- 5. Report the Results ---
# --- 5. 报告结果 ---
print("--------------------------------------------------")
print("--- Intraclass Correlation Coefficient (ICC) ---")
print(paste("Country-Level Variance:", round(var_country_pooled, 3)))
print(paste("School-Level Variance:", round(var_school_pooled, 3)))
print(paste("Student-Level Variance (Residual):", round(var_residual_pooled, 3))) # This is now estimated
print("---")
print(paste("Country-Level ICC:", round(icc_country, 3)))
print(paste("School-Level ICC (including country):", round(icc_school, 3)))
print("--------------------------------------------------")

if (!is.na(icc_school) && icc_school > 0.05) {
  print("Since the school-level ICC is > 0.05, a multilevel model is strongly justified.")
}