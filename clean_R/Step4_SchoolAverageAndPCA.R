# =================================================================
# Step 4: Post-Imputation Feature Engineering (Simplified Workflow)
# =================================================================

# --- 1. Load Libraries and Data ---
library(mice)
library(here)
library(psych)
library(tidyverse)

# --- Define File Paths ---
# This single input file contains both the imputed data and the corresponding IDs
INPUT_MICE_BUNDLE <- here("dataset", "clean_R", "Step3_Imputed_Mice_Object.rds")
OUTPUT_FINAL_LIST <- here("dataset", "clean_R", "Step4_ListOfEngineeredDatasets.rds")
OUTPUT_PLOT_DIR <- here("dataset", "analysis", "pca_diagnostics_step4")

# --- Load the Bundled Object from Step 3 ---
print("Loading the bundled object from Step 3...")
saved_bundle <- readRDS(INPUT_MICE_BUNDLE)

# --- Extract the two components ---
imputed_object <- saved_bundle$mids_object
ids_df <- saved_bundle$ids
print("Data loaded successfully.")


# --- 2. Configuration: Define Variable Groups and Plotting Function ---
vars_for_school_means <- c(
  "ESCS", "AGE", "BSMJ", "GROSAGR", "ANXMAT", "MATHEFF", "MATHEF21", "MATHPERS",
  "ASSERAGR", "COOPAGR", "CURIOAGR", "EMOCOAGR", "EMPATAGR", "PERSEVAGR",
  "STRESAGR", "CREATEFF", "CREATOP", "IMAGINE", "OPENART", "SDLEFF", "ST268Q04JA",
  "ST268Q07JA", "ST268Q01JA", "MISSSC", "SKIPPING", "TARDYSD", "EXERPRAC",
  "STUDYHMW", "WORKPAY", "WORKHOME", "INFOSEEK", "EXPOFA", "EXPO21ST", "CREATAS",
  "CREATOOS", "TEACHSUP", "RELATST", "COGACRCO", "COGACMCO", "DISCLIM",
  "CREATSCH", "FAMSUP", "CREATFAM", "FAMSUPSL", "FEELLAH", "PROBSELF", "LEARRES",
  "BULLIED", "FEELSAFE", "SCHRISK", "BELONG", "SCHSUST"
)

pca_groups <- list(
  Math_Disposition = c('ANXMAT', 'MATHEFF', 'MATHEF21', 'MATHPERS', 'ST268Q04JA', 'ST268Q07JA', 'ST268Q01JA'),
  Social_Emotional_Skills = c('ASSERAGR', 'COOPAGR', 'EMOCOAGR', 'EMPATAGR', 'PERSEVAGR', 'STRESAGR'),
  Openness_Creativity = c('CURIOAGR', 'CREATEFF', 'CREATOP', 'IMAGINE', 'OPENART'),
  Self_Directed_Learning = c('SDLEFF', 'GROSAGR'),
  Teacher_Classroom_Exp = c('TEACHSUP', 'RELATST', 'COGACRCO', 'COGACMCO', 'DISCLIM', 'CREATSCH'),
  Home_Learning_Env = c('FAMSUP', 'CREATFAM', 'FAMSUPSL'),
  Remote_Learning_Exp = c('FEELLAH', 'PROBSELF', 'LEARRES'),
  School_Experience = c('BULLIED', 'FEELSAFE', 'SCHRISK', 'BELONG', 'SCHSUST')
)

generate_pca_plots <- function(pca_results, group_name) {
  eigenvalues <- pca_results$values
  n_components <- length(eigenvalues)
  plot_data <- tibble(component = 1:n_components, eigenvalue = eigenvalues)
  scree_plot <- ggplot(plot_data, aes(x = component, y = eigenvalue)) +
    geom_line(color = "skyblue") + geom_point(color = "red", size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    labs(title = paste("Scree Plot for", group_name), x = "Component Number", y = "Eigenvalue") +
    scale_x_continuous(breaks = 1:n_components) + theme_minimal()
  if (!dir.exists(OUTPUT_PLOT_DIR)) { dir.create(OUTPUT_PLOT_DIR, recursive = TRUE) }
  ggsave(filename = file.path(OUTPUT_PLOT_DIR, paste0(group_name, "_scree_plot.png")),
         plot = scree_plot, width = 7, height = 5)
}


# --- 3. Loop Through Imputations to Create a LIST of Engineered Datasets ---
print("Beginning feature engineering loop...")
list_of_engineered_datasets <- list()

for (i in 1:imputed_object$m) {
  print(paste("Processing Imputation #", i, "..."))

  # Get the i-th complete dataset (this has no IDs)
  completed_data <- complete(imputed_object, i)

  # Directly bind the IDs from the dataframe loaded at the start
  completed_data_with_ids <- bind_cols(ids_df, completed_data)

  # --- 3a. Perform PCA on the completed data ---
  list_of_pca_results <- list()
  for (group_name in names(pca_groups)) {
    var_list <- pca_groups[[group_name]]
    if(all(var_list %in% names(completed_data_with_ids))) {
      tryCatch({
        pca_data <- completed_data_with_ids %>% select(all_of(var_list))
        pca_data <- data.frame(sapply(pca_data, as.numeric))
        scaled_data <- scale(pca_data)

        pca_full_results <- principal(scaled_data, nfactors = ncol(scaled_data), rotate = "none")

        if (i == 1) { # Only print diagnostics for the first imputation
          print(paste("--- PCA Diagnostics for Group:", group_name, "(from Imputation #1) ---"))
          print(pca_full_results$loadings, cutoff = 0.2)
          print(pca_full_results$Vaccounted)
          cat("\n----------------------------------------------------\n")
          generate_pca_plots(pca_full_results, group_name)
        }

        n_components <- sum(pca_full_results$values > 1)
        if (n_components == 0) n_components <- 1

        final_pca <- principal(scaled_data, nfactors = n_components, rotate = "varimax")
        component_scores <- as.data.frame(final_pca$scores)
        names(component_scores) <- paste0(group_name, "_", names(component_scores))
        list_of_pca_results[[group_name]] <- component_scores
      }, error = function(e) {
        print(paste("Could not perform PCA for group:", group_name, ". Error:", e$message))
      })
    }
  }
  all_pca_scores <- bind_cols(list_of_pca_results)

  # --- 3b. Combine imputed data with new PCA scores ---
  data_with_pca <- bind_cols(completed_data_with_ids, all_pca_scores)

  # --- 3c. Create all school-level averages ---
  all_vars_for_sch_means <- c(vars_for_school_means, names(all_pca_scores))

  data_with_all_means <- data_with_pca %>%
    group_by(CNTSCHID) %>%
    mutate(across(all_of(intersect(all_vars_for_sch_means, names(.))),
                  ~mean(.x, na.rm = TRUE),
                  .names = "{.col}_sch_mean")) %>%
    ungroup()

  # Add this fully engineered dataframe to our list
  list_of_engineered_datasets[[i]] <- data_with_all_means
}
print("Feature engineering loop complete.")


# --- 4. Save the FINAL LIST of Engineered Datasets ---
print("Saving the list of engineered datasets...")
saveRDS(list_of_engineered_datasets, file = OUTPUT_FINAL_LIST)
print(paste("Success! Final list of datasets saved to:", OUTPUT_FINAL_LIST))