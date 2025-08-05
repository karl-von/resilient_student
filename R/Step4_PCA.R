# =================================================================
# Step 4: Post-Imputation Feature Engineering
#
# METHODOLOGY:
# This script uses the correct "Stack-Analyze-Split" method to ensure
# stable PCA components across all imputed datasets.
# =================================================================


# --- 1. Load Libraries and Data ---
library(mice)
library(here)
library(psych)
library(tidyverse)

# --- Define File Paths ---
INPUT_MICE_BUNDLE <- here("dataset", "", "Step3_Imputed_Mice_Object_Multilevel.rds")
OUTPUT_FINAL_LIST <- here("dataset", "", "Step4_ListOfEngineeredDatasets.rds")
OUTPUT_PCA_MODELS <- here("dataset", "analysis", "Step4_PCAModelObjects_for_Diagnostics.rds") # New output file

# --- Load the Bundled Object from Step 3 ---
print("Loading the bundled mice object from Step 3...")
saved_bundle <- readRDS(INPUT_MICE_BUNDLE)
imputed_object <- saved_bundle$mids_object
print("Data loaded successfully.")


# --- 2. Configuration: Define Variable Groups ---
# This section remains the same.
pca_groups <- list(
  Math_Disposition = c('ANXMAT', 'MATHEFF', 'MATHEF21', 'MATHPERS', 'ST268Q04JA', 'ST268Q07JA', 'ST268Q01JA'),
  Math_Disposition_Sensitivity = c('ANXMAT','MATHPERS', 'ST268Q07JA', 'ST268Q01JA'),
  Social_Emotional_Skills = c('ASSERAGR', 'COOPAGR', 'EMOCOAGR', 'EMPATAGR', 'PERSEVAGR', 'STRESAGR'),
  Openness_Creativity = c('CURIOAGR', 'CREATEFF', 'CREATOP', 'IMAGINE', 'OPENART'),
  Self_Directed_Learning = c('SDLEFF', 'GROSAGR'),
  Teacher_Classroom_Exp = c('TEACHSUP', 'RELATST', 'COGACRCO', 'COGACMCO', 'DISCLIM', 'CREATSCH'),
  Home_Learning_Env = c('FAMSUP', 'CREATFAM', 'FAMSUPSL'),
  Remote_Learning_Exp = c('FEELLAH', 'PROBSELF', 'LEARRES'),
  School_Experience = c('BULLIED', 'FEELSAFE', 'SCHRISK', 'BELONG', 'SCHSUST')
)


# --- 3. Stack Imputed Datasets and Run a Single, Stable PCA ---

# STACK: Convert the mice object into a single long-format dataframe.
print("Stacking all 5 imputed datasets for stable PCA...")
stacked_completed_data <- mice::complete(imputed_object, "long")

# ANALYZE: Loop through groups, run PCA, and generate scores.
print("Running PCA on stacked data to ensure stable components...")
list_of_pca_scores <- list()
list_of_pca_models <- list() # New list to store model objects for saving

for (group_name in names(pca_groups)) {
  var_list <- pca_groups[[group_name]]

  # Select and scale data
  pca_data <- stacked_completed_data %>%
    select(all_of(var_list)) %>%
    mutate(across(everything(), as.numeric))
  scaled_data <- scale(pca_data)

  # Run PCA to get eigenvalues (for scree plot)
  pca_full_results <- principal(scaled_data, nfactors = ncol(scaled_data), rotate = "none")

  # Determine number of components
  n_components <- sum(pca_full_results$values > 1)
  if (n_components == 0) n_components <- 1

  # Run final PCA with rotation (for loadings table)
  final_pca <- principal(scaled_data, nfactors = n_components, rotate = "varimax")

  # Store the PCA model objects for later diagnostic use
  list_of_pca_models[[group_name]] <- list(
    unrotated_model = pca_full_results,
    rotated_model = final_pca
  )

  # Generate the component scores to add to the main dataset
  component_scores <- as.data.frame(final_pca$scores)
  names(component_scores) <- paste0(group_name, "_", names(component_scores))
  list_of_pca_scores[[group_name]] <- component_scores
}

# Combine all generated score columns into a single dataframe
all_pca_scores_df <- bind_cols(list_of_pca_scores)


# --- 4. Reconstruct the List of 5 Datasets ---
print("Merging scores and splitting data back into a list of 5 datasets...")
data_with_scores <- bind_cols(stacked_completed_data, all_pca_scores_df)
list_of_engineered_datasets <- split(data_with_scores, data_with_scores$.imp)

# Final cleanup: Remove helper columns
list_of_engineered_datasets <- lapply(list_of_engineered_datasets, function(df) {
  df %>% select(-.imp, -.id)
})
print("Successfully created a list of 5 datasets with stable PCA scores.")


# --- 5. Save the FINAL Datasets and the Diagnostic Objects ---

# Save the primary output: the list of 5 engineered datasets for the next step.
print("Saving the list of engineered datasets...")
saveRDS(list_of_engineered_datasets, file = OUTPUT_FINAL_LIST)
print(paste("Success! Final list of datasets saved to:", OUTPUT_FINAL_LIST))

# Save the secondary output: the list of PCA models for the diagnostic script.
print("Saving the PCA model objects for diagnostic generation...")
saveRDS(list_of_pca_models, file = OUTPUT_PCA_MODELS)
print(paste("Success! PCA model objects saved to:", OUTPUT_PCA_MODELS))