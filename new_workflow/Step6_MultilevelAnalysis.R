# ==============================================================================
# PISA Two-Level Multilevel Analysis with mitools pooling
# (Manual pooling across PVs and imputations)
# ==============================================================================

# --- 1. SETUP | Load Required Libraries ---
library(BIFIEsurvey)
library(dplyr)
library(here)
library(mitools)

# --- 2. DATA INPUT | Load Imputed Datasets and Latent Variable Scores ---
print("Loading the list of 5 imputed datasets from Step 3...")
main_data_path <- here("dataset", "", "Step3_Imputed_Data_List.rds")
# main_data_path <- here("dataset", "", "Step6_TestData_With_New_Outcome.rds")
list_of_engineered_datasets <- readRDS(main_data_path)
print("List of datasets loaded successfully.")

# --- NEW ---
# Load the latent variable scores you created in the previous step
print("Loading the latent variable factor scores...")
lv_scores_path <- here("dataset", "Step5_Combined_Factor_Scores_Long_FullNames.rds")
lv_scores_long <- readRDS(lv_scores_path)
print("Latent variable scores loaded.")


# --- 3. MERGE DATA | Combine Latent Variables with Main Datasets ---
# --- NEW SECTION ---
print("Merging latent variable scores into each imputed dataset...")

# We loop through each of the 5 main datasets and merge the corresponding
# set of latent variable scores into it.
for (i in 1:length(list_of_engineered_datasets)) {

  # Get the main data for the current imputation
  main_df <- list_of_engineered_datasets[[i]]

  # Get the latent variable scores for the current imputation
  lv_scores_for_imp <- lv_scores_long %>%
    filter(.imp == i)

  # Ensure the student ID columns are the same character type for a safe join
  main_df$CNTSTUID <- as.character(main_df$CNTSTUID)
  lv_scores_for_imp$.id <- as.character(lv_scores_for_imp$.id)

  # Perform the merge using a left join
  # We are joining by the student ID: `CNTSTUID` from the main data
  # and `.id` from the latent variable scores data.
  merged_df <- left_join(main_df, lv_scores_for_imp, by = c("CNTSTUID" = ".id"))

  # Replace the original dataset in the list with the new merged one
  list_of_engineered_datasets[[i]] <- merged_df
}
print("Merging complete. Latent variables are now part of the main datasets.")

# Quick check to confirm new columns were added
print("Columns in the first imputed dataset after merging:")
print(names(list_of_engineered_datasets[[1]]))


# --- 4. PRE-PROCESSING ---
# This step remains the same as before.
print("Pre-processing each of the merged & imputed datasets...")
list_of_engineered_datasets <- lapply(list_of_engineered_datasets, function(df) {
  df %>%
    mutate(
      FEMALE = as.factor(if_else(ST004D01T == 1, 1, 0)),
      IMMIG = as.factor(IMMIG),
      DUMMY_SCH_WT = 1
    ) %>%
    arrange(CNTSCHID)
})
print("Pre-processing complete.")

# --- 5. SURVEY DESIGN | Create BIFIE.data Objects ---
# This step remains the same.
print("Defining the complex survey design...")
bifie_data_list <- lapply(list_of_engineered_datasets, function(df) {
  BIFIE.data.jack(
    jktype="RW_PISA",
    data = df,
    wgt = "W_FSTUWT",
    wgtrep = "W_FSTURWT",
    fayfac = 0.5
  )
})
print("BIFIEsurvey data objects created successfully.")


# --- 6. MODEL SPECIFICATION ---
# --- MODIFIED ---

# Model 1 (Controls) remains the same
model_1_formula <- as.formula("~ ESCS + AGE + FEMALE + IMMIG + ISCEDP")

# Define the names of the latent variables to add
# As requested, 'Remote_Learning_Exp' is excluded.
latent_variable_predictors <- c(
  "Math_Disposition",
  "Social_Emotional_Skills",
  "Openness_Creativity",
  "Self_Directed_Learning",
  "Teacher_Classroom_Exp",
  "Home_Learning_Env",
  "School_Experience"
)

# Combine original predictors with the new latent variable predictors
original_model_2_predictors <- "ESCS + AGE + FEMALE + IMMIG + ISCEDP +
   EXPECEDU + OCOD3_major_group + BSMJ + SISCO +
   REPEAT + MISSSC + SKIPPING + TARDYSD + EXERPRAC +
   STUDYHMW + WORKPAY + WORKHOME + INFOSEEK + EXPOFA +
   EXPO21ST + CREATAS + CREATOOS + STRESAGR + BULLIED + FEELLAH + PROBSELF + LEARRES"

# Create the full model formula string
full_model_string <- paste(
  "~",
  original_model_2_predictors,
  "+",
  paste(latent_variable_predictors, collapse = " + ")
)

# Convert the string to a formula object
model_2_formula <- as.formula(full_model_string)

print("Full model formula updated with latent variables:")
print(model_2_formula)

# Plausible values (dependent variable) names remain the same
pv_names <- paste0("RESILIENCE_SCORE_PV", 1:10)


# --- 7. ANALYSIS & POOLING ---
# This entire function and its execution remain the same.
# It will now use the updated 'model_2_formula'.
run_and_pool_with_mitools <- function(bifie_data_list, model_formula, model_name, pvs) {
  print(paste("Running & Pooling Two-Level", model_name, "- This may take a while..."))

  # 1. Loop over imputations
  pooled_by_imp <- lapply(seq_along(bifie_data_list), function(i) {
    bifie_obj <- bifie_data_list[[i]]
    print(paste("... Imputation", i))

    # 2. Loop over PVs for this imputation
    pv_models <- lapply(pvs, function(pv_variable) {
      print(paste("...... analyzing", pv_variable))
      BIFIE.twolevelreg(
        BIFIEobj = bifie_obj,
        dep = pv_variable,
        formula.fixed = model_formula,
        formula.random = ~1,
        idcluster = "CNTSCHID",
        wgtlevel2 = "DUMMY_SCH_WT",
        wgtlevel1 = NULL,
        se = TRUE
      )
    })

    # 3. Pool PVs within this imputation
    print("pool PVs by MIcombine")
    pooled_pv <- MIcombine(
      results   = lapply(pv_models, coef),
      variances = lapply(pv_models, vcov)
    )
    return(pooled_pv)
  })

  # 4. Pool across imputations
  print("final_pool by MIcombine")
  final_pooled <- MIcombine(
    results   = lapply(pooled_by_imp, coef),
    variances = lapply(pooled_by_imp, vcov)
  )

  # 5. Save summary to CSV
  final_summary <- summary(final_pooled)
  output_filename <- here("dataset", "analysis", paste0("NEW_Summary_", model_name, ".csv"))
  write.csv(as.data.frame(final_summary), output_filename, row.names = TRUE) # Set row.names=TRUE for coefficients
  print(paste("Final pooled results saved to:", output_filename))

  output_filename_rds <- here("dataset", "analysis", paste0("NEW_Results_", model_name, ".rds"))
  saveRDS(final_pooled, file = output_filename_rds)
  print(paste("Complete model object saved to:", output_filename_rds))
  return(final_pooled)
}

# --- 8. RUN MODELS ---
results_model_1 <- run_and_pool_with_mitools(bifie_data_list, model_1_formula, "Model_Controls", pv_names)
results_model_2 <- run_and_pool_with_mitools(bifie_data_list, model_2_formula, "Model_Full_with_LVs", pv_names)

# --- 9. OUTPUT ---
print("==================================================================")
print(" FINAL POOLED RESULTS FOR TWO-LEVEL MODEL 1 (CONTROLS ONLY) ")
print("==================================================================")
print(summary(results_model_1))

print("==================================================================")
print(" FINAL POOLED RESULTS FOR TWO-LEVEL MODEL 2 (FULL MODEL + LVs) ")
print("==================================================================")
print(summary(results_model_2))
print("==================================================================")