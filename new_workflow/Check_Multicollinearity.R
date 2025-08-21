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
main_data_path <- here("dataset", "", "Step3_Imputed_Data_List.rds")
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
print("creat correlation matrix.png")
cor_matrix <- cor(key_predictors, use = "pairwise.complete.obs") #

png("correlation_matrix.png", width = 1200, height = 1000, res = 150) # 打开PNG设备

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.6,
         diag = FALSE)

dev.off()

# --- MODIFIED: Diagnostic C: Check Model Error (RMSE) across all PVs ---

print("--- Calculating RMSE across all 10 Plausible Values ---")

# Define the list of Plausible Value outcome names
pv_names <- paste0("RESILIENCE_SCORE_PV", 1:10)

# Define the base formula (without the outcome variable)
base_formula <- as.formula(
  ~ ESCS + AGE + FEMALE + IMMIG + ISCEDP +
    EXPECEDU + OCOD3_major_group + BSMJ + SISCO +
    REPEAT + MISSSC + SKIPPING + TARDYSD + EXERPRAC +
    STUDYHMW + WORKPAY + WORKHOME + INFOSEEK + EXPOFA +
    EXPO21ST + CREATAS + CREATOOS + STRESAGR + BULLIED + FEELLAH + PROBSELF + LEARRES +
    Math_Disposition + Social_Emotional_Skills + Openness_Creativity +
    Self_Directed_Learning + Teacher_Classroom_Exp + Home_Learning_Env + School_Experience
)

# Create a list to store results from each PV
pv_results_list <- list()

# Loop through each Plausible Value
for (pv_name in pv_names) {

  # Create the full formula for the current PV
  current_formula <- update.formula(base_formula, as.formula(paste(pv_name, "~ .")))

  # Fit a simple linear model for this PV
  simple_lm <- lm(current_formula, data = diagnostic_df)

  # Get residuals and calculate RMSE
  model_residuals <- residuals(simple_lm)
  rmse <- sqrt(mean(model_residuals^2, na.rm = TRUE))

  # Get the standard deviation of the current PV
  outcome_sd <- sd(diagnostic_df[[pv_name]], na.rm = TRUE)

  # Store the results
  pv_results_list[[pv_name]] <- data.frame(
    PV = pv_name,
    RMSE = rmse,
    Outcome_SD = outcome_sd
  )

}

# Combine the results into a single data frame
pv_results_df <- do.call(rbind, pv_results_list)

# Print the detailed results for each PV
print("--- RMSE and SD for each Plausible Value ---")
print(pv_results_df)

# Calculate and print the final, averaged results
avg_rmse <- mean(pv_results_df$RMSE)
avg_sd <- mean(pv_results_df$Outcome_SD)
improvement_pct <- (avg_sd - avg_rmse) / avg_sd * 100

print("--- Averaged Diagnostic Results Across 10 PVs ---")
print(paste("Average Root Mean Squared Error (RMSE):", round(avg_rmse, 4)))
print(paste("Average Outcome Standard Deviation (SD):", round(avg_sd, 4)))
print(paste("Average Reduction in Error (vs. SD):", round(improvement_pct, 2), "%"))

summary(diagnostic_df$RESILIENCE_SCORE_PV1)

print("==================================================================")
print(" DIAGNOSTIC SCRIPT COMPLETE ")
print("==================================================================")