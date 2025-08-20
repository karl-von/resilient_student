# ==============================================================================
# STANDALONE SCRIPT FOR DIAGNOSTIC CHECKS
# Purpose: To check model assumptions and data characteristics before running
#          the full, time-consuming BIFIEsurvey analysis.
# ==============================================================================

# --- 1. SETUP | Load Required Libraries ---
print("Loading libraries...")
library(dplyr)
library(here)
library(skimr)    # For descriptive statistics
library(car)      # For VIF calculation
library(corrplot) # For visualizing correlations


# --- 2. LOAD AND PREPARE DATA ---
# NOTE: These steps MUST be identical to your main analysis script to ensure
#       the diagnostics are valid for the model you intend to run.
print("Loading and preparing data...")

# Load datasets with the continuous outcome
# main_data_path <- here("dataset", "", "Step6_TestData_With_New_Outcome.rds")
list_of_engineered_datasets <- readRDS(main_data_path)

# Load latent variable scores
lv_scores_path <- here("dataset", "Step5_Combined_Factor_Scores_Long_FullNames.rds")
lv_scores_long <- readRDS(lv_scores_path)

# Merge latent variables into each imputed dataset
for (i in 1:length(list_of_engineered_datasets)) {
  main_df <- list_of_engineered_datasets[[i]]
  lv_scores_for_imp <- lv_scores_long %>% filter(.imp == i)

  main_df$CNTSTUID <- as.character(main_df$CNTSTUID)
  lv_scores_for_imp$.id <- as.character(lv_scores_for_imp$.id)

  merged_df <- left_join(main_df, lv_scores_for_imp, by = c("CNTSTUID" = ".id"))
  list_of_engineered_datasets[[i]] <- merged_df
}

# Pre-process variables (e.g., creating factors)
list_of_engineered_datasets <- lapply(list_of_engineered_datasets, function(df) {
  df %>%
    mutate(
      FEMALE = as.factor(if_else(ST004D01T == 1, 1, 0)),
      IMMIG = as.factor(IMMIG)
    )
})
print("Data loading and preparation complete.")


# --- 3. RUN DIAGNOSTIC CHECKS ---
print("==================================================================")
print(" RUNNING DIAGNOSTIC CHECKS ON THE FIRST IMPUTED DATASET ")
print("==================================================================")

# Use the first fully prepared dataset as a representative sample
diagnostic_df <- list_of_engineered_datasets[[1]]

# --- Diagnostic A: Check Predictor Distributions ---
print("--- Checking predictor distributions with skimr ---")
key_predictors <- diagnostic_df %>%
  select(
    ESCS, REPEAT, MISSSC, SKIPPING, TARDYSD,
    Math_Disposition, Social_Emotional_Skills, Openness_Creativity,
    Self_Directed_Learning, Teacher_Classroom_Exp, Home_Learning_Env,
    School_Experience
  )
skim(key_predictors)


# --- Diagnostic B: Check for Multicollinearity (VIF & Correlations) ---

# Define the full model formula using the continuous outcome variable
# We use just the first PV (`_PV1`) for this diagnostic check.
diagnostic_formula <- as.formula(
  RESILIENCE_SCORE_PV1 ~ ESCS + AGE + FEMALE + IMMIG + ISCEDP +
    EXPECEDU + OCOD3_major_group + BSMJ + SISCO +
    REPEAT + MISSSC + SKIPPING + TARDYSD + EXERPRAC +
    STUDYHMW + WORKPAY + WORKHOME + INFOSEEK + EXPOFA +
    EXPO21ST + CREATAS + CREATOOS + STRESAGR + BULLIED + FEELLAH + PROBSELF + LEARRES +
    Math_Disposition + Social_Emotional_Skills + Openness_Creativity +
    Self_Directed_Learning + Teacher_Classroom_Exp + Home_Learning_Env + School_Experience
)

# Fit a simple linear model (ignoring nesting/weights) just for VIF calculation
simple_lm <- lm(diagnostic_formula, data = diagnostic_df)

# Calculate and print VIF scores
print("--- Calculating Variance Inflation Factor (VIF) Scores ---")
vif_scores <- vif(simple_lm)
print(vif_scores)

# Calculate and plot the correlation matrix for key numeric predictors
print("--- Generating correlation matrix plot (see Plots pane) ---")
cor_matrix <- cor(key_predictors, use = "pairwise.complete.obs")
corrplot(cor_matrix,
         method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.6, diag = FALSE)


# --- Diagnostic C: Check Model Error (RMSE) ---

# We already fit 'simple_lm' above, so we can reuse it
model_residuals <- residuals(simple_lm)
rmse <- sqrt(mean(model_residuals^2, na.rm = TRUE))

print("--- Calculating Root Mean Squared Error (RMSE) ---")
print(paste("The Root Mean Squared Error (RMSE) is:", round(rmse, 4)))

# CRUCIAL: Check the standard deviation of the outcome to contextualize the RMSE
print("--- Descriptive statistics for the outcome variable (for context) ---")
print("Summary of RESILIENCE_SCORE_PV1:")
summary(diagnostic_df$RESILIENCE_SCORE_PV1)
print("Standard Deviation of RESILIENCE_SCORE_PV1:")
sd(diagnostic_df$RESILIENCE_SCORE_PV1, na.rm = TRUE)

print("==================================================================")
print(" DIAGNOSTIC SCRIPT COMPLETE ")
print("==================================================================")