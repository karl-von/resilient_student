# =================================================================
# Step 5 (Revised) - Combine Mplus Factor Scores with Correct Order
# =================================================================

# --- 1. Load Libraries ---
library(tidyverse)
library(here)
library(stringr)

print("Starting Step 5 (Revised): Combining Mplus factor scores...")

# --- 2. Setup and Definitions ---

# Directory where the .dat and .inp files were saved
mplus_data_dir <- here("dataset", "mplus_bayesian")

# Number of imputations (must match the number you ran)
num_imputations <- 5 # Assuming 5, as in your previous script

# --- NEW: Define the mapping from short Mplus names to your desired full names ---
# The names on the LEFT must EXACTLY match the latent variable names from your Step 4 script.
# The names on the RIGHT are the new, longer names you want in the bayesian data frame.
lv_name_map <- list(
  Math_Dispo = "Math_Disposition",
  Soc_Emo_Ski = "Social_Emotional_Skills",
  Open_Creat = "Openness_Creativity",
  Tea_Class_Exp = "Teacher_Classroom_Exp",
  Sch_Exp = "School_Experience"
)

# --- 3. Main Processing Loop ---

# This list will hold the 5 complete data frames, one for each imputation
imputation_dfs_list <- list()

print("Looping through each imputation to combine factor scores...")
for (i in 1:num_imputations) {

  print(paste("--- Processing Imputation #", i, "---"))

  # This list will hold the score data frames for a single imputation
  list_of_lv_scores <- list()

  # Inner loop: iterate through each latent variable's SHORT name
  for (short_lv_name in names(lv_name_map)) {

    # Get the desired long name for the final column
    long_lv_name <- lv_name_map[[short_lv_name]]

    # Construct the file name Mplus created using the short name
    # e.g., "sch_exp_scores_imp1.dat"
    fscores_file_name <- paste0(str_to_lower(short_lv_name), "_scores_imp", i, ".dat")
    file_path <- file.path(mplus_data_dir, fscores_file_name)

    if (file.exists(file_path)) {
      # Read the space-delimited .dat file
      temp_data <- read.table(file_path, header = FALSE, fill = TRUE)

      # Determine the number of columns
      num_cols <- ncol(temp_data)

      # --- MODIFIED: Select columns based on the corrected order from the .out file ---
      # The last three columns are: Factor Score, CNTSTUID, CNT.
      temp_data_processed <- temp_data %>%
        select(
          # Factor Score is the third-to-last column. Rename it to the long name.
          !!long_lv_name := all_of(paste0("V", num_cols - 2)),

          # CNTSTUID is the second-to-last column
          CNTSTUID = all_of(paste0("V", num_cols - 1)),

          # CNT is the very last column
          CNT = all_of(paste0("V", num_cols))
        ) %>%
        # Ensure ID is a character for safe joining and CNT is numeric/integer
        mutate(
          CNTSTUID = as.character(CNTSTUID),
          CNT = as.integer(CNT)
        )

      # Add the processed data frame to our list for this imputation
      list_of_lv_scores[[long_lv_name]] <- temp_data_processed

    } else {
      warning(paste("File not found, skipping:", file_path))
    }
  } # End of latent variable loop

  # Check if we have any scores to combine for this imputation
  if (length(list_of_lv_scores) > 0) {
    # Combine all latent variable scores for the current imputation `i`
    # We join them together using the common student ID and country ID columns
    complete_imp_df <- list_of_lv_scores %>%
      reduce(left_join, by = c("CNTSTUID", "CNT"))

    # Add an imputation identifier column, which is crucial for pooling later
    complete_imp_df <- complete_imp_df %>%
      mutate(.imp = i) %>%
      rename(.id = CNTSTUID) %>% # Rename to `.id` for compatibility with `mice`
      select(.imp, .id, CNT, everything()) # Reorder for clarity

    # Add the final data frame for this imputation to our main list
    imputation_dfs_list[[i]] <- complete_imp_df

    print(paste("--- Finished combining", length(list_of_lv_scores), "latent variables for Imputation #", i, "---"))
  }

} # End of imputation loop

# --- 4. Final Combination and Inspection ---

print("Combining all imputation data frames into one long-format dataset...")

# Stack all the completed imputation data frames on top of each other
final_long_data <- bind_rows(imputation_dfs_list)

# Inspect the bayesian result
print("Final combined dataset structure:")
glimpse(final_long_data)

print("First few rows of the final dataset:")
print(head(final_long_data))

# --- 5. (Optional) Save the Final Dataset ---
# This is highly recommended so you don't have to re-run this script.
output_path <- here("dataset", "Step5_lantent_variables.rds")
saveRDS(final_long_data, file = output_path)

print("=================================================================")
print(paste("Success! All factor scores combined and saved to:", output_path))
print("This 'final_long_data' object is now ready for regression analysis.")