# =================================================================
# Standalone Test Script: Inject_New_Outcome_for_Regression_Test
#
# PURPOSE:
# To create a temporary dataset for testing the regression model (Step 6).
# This script takes the final data from the OLD PCA workflow and
# replaces the old binary outcome variable with the NEW continuous
# outcome variable from the corrected Step 1 script.
# =================================================================

# --- 1. Load Libraries ---
library(dplyr)
library(here)
library(purrr)

# --- 2. Configuration: Define File Paths ---

# INPUT 1: The final dataset list from your OLD Step 4 (PCA) workflow.
# This file contains your 5 imputed datasets with PCA-based predictors.
INPUT_OLD_DATA_LIST <- here("dataset", "Step4_ListOfEngineeredDatasets.rds")

# INPUT 2: The output from your NEWLY CORRECTED Step 1 script.
# This file contains the correct student IDs and the new continuous outcome.
INPUT_NEW_OUTCOME_DATA <- here("dataset", "", "Step1_Resilience.rds")

# OUTPUT: The path to save the new, combined dataset for testing.
OUTPUT_TEST_DATA_LIST <- here("dataset", "", "Step6_TestData_With_New_Outcome.rds")


# --- 3. Load Data ---
print("Loading the old dataset list (with PCA variables)...")
list_of_old_datasets <- readRDS(INPUT_OLD_DATA_LIST)

print("Loading the new foundational data (with the continuous outcome)...")
new_outcome_data <- readRDS(INPUT_NEW_OUTCOME_DATA)


# --- 4. Prepare the New Outcome Data for Merging ---
# We only need the student ID and the new RESILIENCE_SCORE_PV columns.
# Selecting only these makes the merge more efficient.
print("Isolating the new outcome variables for the merge...")
outcomes_to_merge <- new_outcome_data %>%
  select(CNTSTUID, starts_with("RESILIENCE_SCORE_PV")) %>%
  # Ensure the join key is a character type for a safe merge
  mutate(CNTSTUID = as.character(CNTSTUID))


# --- 5. Modify the Old Datasets ---
# We will now loop through each of the 5 old datasets. In each one, we will
# remove the old outcome and join in the new one.
print("Modifying the 5 old datasets...")

list_of_test_datasets <- map(list_of_old_datasets, function(df) {

  # Ensure the join key in the main dataframe is also a character
  df_prepared <- df %>%
    mutate(CNTSTUID = as.character(CNTSTUID))

  # 1. Remove the old binary outcome variable, if it exists, to avoid confusion.
  #    The `any_of()` function prevents an error if the columns aren't found.
  df_no_old_outcome <- df_prepared %>%
    select(-any_of(paste0("ACADEMIC_RESILIENCE_PV", 1:10)))

  # 2. Join the new continuous outcome variables using the student ID.
  df_with_new_outcome <- left_join(
    df_no_old_outcome,
    outcomes_to_merge,
    by = "CNTSTUID"
  )

  return(df_with_new_outcome)
})

print("Successfully replaced the outcome variable in all 5 datasets.")


# --- 6. Save the New Test Dataset ---
print("Saving the new list of datasets for regression testing...")
saveRDS(list_of_test_datasets, file = OUTPUT_TEST_DATA_LIST)

print("=================================================================")
print(paste("Success! Your test dataset is ready and has been saved to:", OUTPUT_TEST_DATA_LIST))
print("You can now use this file as the input for Step6_MultilevelAnalysis.R")
print("=================================================================")