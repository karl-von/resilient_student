# =================================================================
# Analysis Step A: Null Model & ICC (Corrected glmer Approach)
# 分析步骤 A: 零模型与ICC (修正版 glmer 方法)
# =================================================================

# --- 1. Load Libraries and Data ---
library(lme4)      # For glmer
library(mitml)     # For with.mitml.list and testModels
library(dplyr)
library(here)

# Define input path
INPUT_MODEL_DATA <- here("dataset", "clean_R", "Step5_Final_Model_Ready_List.rds")

print("Loading the list of 10 model-ready datasets...")
model_ready_list <- readRDS(INPUT_MODEL_DATA)

# --- 2. Correct Data Types for Modeling (Definitive Fix) ---
print("Correcting data types for grouping variables...")
model_ready_list <- lapply(model_ready_list, function(df) {
  df <- df %>%
    mutate(
      # THE FIX: First convert from haven_labelled to a simple vector, then to a factor.
      # 修正: 首先将 haven_labelled 类型转换为简单的向量，然后再转换为因子。
      CNT = as.factor(as.vector(CNT)),
      CNTSCHID = as.factor(as.vector(CNTSCHID))
    )
  return(df)
})
print("Data types corrected successfully.")


# --- 3. Fit the Null Model using glmer ---
# This is the theoretically correct approach for a binary outcome.
# We will fit a three-level logistic model with random intercepts for school and country.
print("Fitting the three-level null model using glmer...")

# The formula specifies random intercepts for schools (CNTSCHID) nested within countries (CNT)
null_models_glmer <- with.mitml.list(
  model_ready_list,
  glmer(RESILIENCE ~ 1 + (1 | CNT / CNTSCHID), family = binomial, weights = W_FSTUWT)
)
print("All 10 null models fitted successfully.")


# --- 4. Pool Variance Components and Calculate ICC ---
print("Pooling variance components from glmer models...")

# Extract variance components from each of the 10 models
model_variances_glmer <- lapply(null_models_glmer, function(model) {
  variances <- as.data.frame(VarCorr(model))
  var_school <- variances$vcov[variances$grp == "CNTSCHID:CNT"]
  var_country <- variances$vcov[variances$grp == "CNT"]
  return(c(var_school = var_school, var_country = var_country))
})

# Average the variance components across the 10 models
pooled_variances_glmer <- colMeans(do.call(rbind, model_variances_glmer))

var_school_pooled  <- pooled_variances_glmer["var_school"]
var_country_pooled <- pooled_variances_glmer["var_country"]

# For a logistic model, the student-level (residual) variance is a fixed constant: π²/3
student_variance_logistic <- (pi^2) / 3

# Calculate total variance
total_variance <- var_country_pooled + var_school_pooled + student_variance_logistic

# Calculate the ICCs
icc_school_total <- (var_country_pooled + var_school_pooled) / total_variance
icc_country      <- var_country_pooled / total_variance


# --- 5. Report the Results ---
print("--------------------------------------------------")
print("--- Intraclass Correlation Coefficient (ICC) ---")
print("--- (from Logistic Multilevel Model) ---")
print(paste("Country-Level Variance:", round(var_country_pooled, 4)))
print(paste("School-Level Variance:", round(var_school_pooled, 4)))
print(paste("Student-Level Variance (fixed):", round(student_variance_logistic, 4)))
print("---")
print(paste("Country-Level ICC:", round(icc_country, 4)))
print(paste("Total School-Level ICC (Country + School):", round(icc_school_total, 4)))
print("--------------------------------------------------")

if (!is.na(icc_school_total) && icc_school_total > 0.05) {
  print("RESULT: The ICC is greater than 0.05, which provides strong justification for using a multilevel model.")
} else {
  print("RESULT: The ICC is less than 0.05. While a multilevel model may still be appropriate based on theory, the between-group variance is low.")
}