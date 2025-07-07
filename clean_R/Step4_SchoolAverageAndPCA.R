
# =================================================================
# Step 4: Post-Imputation Feature Engineering (Complete & Modified)
# =================================================================

# --- 1. Load Libraries and Data ---
library(mice)
library(here)
library(psych)
library(tidyverse)

# --- Define File Paths ---
INPUT_RDS_STEP2 <- here("dataset", "clean_R", "Step2_Merged_Cleaned_for_Imputation.rds")
INPUT_MICE_OBJECT <- here("dataset", "clean_R", "Step3_Imputed_Mice_Object.rds")
OUTPUT_FEATURES_RDS <- here("dataset", "clean_R", "Step4_Engineered_Features_Pooled.rds")
OUTPUT_PLOT_DIR <- here("dataset", "analysis", "pca_diagnostics_step4")

print("Loading the imputed 'mids' object from Step 3...")
imputed_object <- readRDS(INPUT_MICE_OBJECT)

print("Loading data from Step 2 to retrieve student IDs...")
data_with_ids <- readRDS(INPUT_RDS_STEP2)


# --- 2. Configuration: Define Variable Groups ---
# This section is unchanged
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

# This plotting function is unchanged
generate_pca_plots <- function(pca_results, group_name) {
  eigenvalues <- pca_results$values
  n_components <- length(eigenvalues)
  plot_data <- tibble(component = 1:n_components, eigenvalue = eigenvalues)
  scree_plot <- ggplot(plot_data, aes(x = component, y = eigenvalue)) +
    geom_line(color = "skyblue") + geom_point(color = "red", size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    labs(title = "Scree Plot", x = "Component Number", y = "Eigenvalue") +
    scale_x_continuous(breaks = 1:n_components) + theme_minimal()
  if (!dir.exists(OUTPUT_PLOT_DIR)) { dir.create(OUTPUT_PLOT_DIR, recursive = TRUE) }
  ggsave(filename = file.path(OUTPUT_PLOT_DIR, paste0(group_name, "_scree_plot.png")),
         plot = scree_plot, width = 7, height = 5)
}


# --- 3. Loop Through Imputations to Create ALL Features ---
print("Beginning feature engineering loop...")
list_of_feature_dfs <- list()

# Correctly filter the ID list to match only the students included in the imputation
imputed_students_rows <- as.numeric(rownames(imputed_object$data))
original_ids <- data_with_ids[imputed_students_rows, ] %>%
  select(CNTSTUID, CNTSCHID)

for (i in 1:imputed_object$m) {
  print(paste("Processing Imputation #", i, "..."))

  # Get the i-th complete dataset and bind the IDs
  completed_data <- complete(imputed_object, i)
  completed_data_with_ids <- bind_cols(original_ids, completed_data)

  # --- 3a. Perform PCA and create student-level component scores ---
  list_of_pca_results <- list()
  for (group_name in names(pca_groups)) {
    var_list <- pca_groups[[group_name]]
    if(all(var_list %in% names(completed_data_with_ids))) {
      tryCatch({
        pca_data <- completed_data_with_ids %>% select(all_of(var_list))
        pca_data <- data.frame(sapply(pca_data, as.numeric))
        scaled_data <- scale(pca_data)
        pca_full_results <- principal(scaled_data, nfactors = ncol(scaled_data), rotate = "none")

        if (i == 1) {
          print(paste("--- PCA Diagnostics for Group:", group_name, "(from Imputation #1) ---"))
          print("Loadings:")
          print(pca_full_results$loadings, cutoff = 0.2)
          cat("\n")
          print("Variance Accounted For:")
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

  ### --- MODIFICATION: Create ALL school-level means in one go --- ###

  # 1. Create a temporary dataframe with IDs, original data, and new student-level PCA scores
  temp_full_data <- bind_cols(completed_data_with_ids, all_pca_scores)

  # 2. Define ALL variables that need a school-level average
  all_vars_for_sch_means <- c(
    vars_for_school_means, # Original PISA variables
    names(all_pca_scores)  # NEW: Add the names of the PCA scores
  )

  # 3. Create all school means in a single, efficient step
  data_with_all_means <- temp_full_data %>%
    group_by(CNTSCHID) %>%
    mutate(across(all_of(intersect(all_vars_for_sch_means, names(.))),
                  ~mean(.x, na.rm = TRUE),
                  .names = "{.col}_sch_mean")) %>%
    ungroup()

  # --- 3c. Select only the final columns needed for the analysis ---
  # We need the student ID for pooling, the student-level PCA scores, and ALL school-level means.
  features_for_this_imputation <- data_with_all_means %>%
    select(
      CNTSTUID,
      one_of(names(all_pca_scores)), # Select all the student-level PCA scores
      ends_with("_sch_mean")         # Select ALL columns that end with _sch_mean
    )

  list_of_feature_dfs[[i]] <- features_for_this_imputation
}
print("Feature engineering loop complete.")


# --- 4. Pool and Save the Engineered Features ---
# This section is unchanged and will correctly pool all the new variables
print("Pooling engineered features by averaging across imputations...")
all_features_long <- bind_rows(list_of_feature_dfs)
pooled_features <- all_features_long %>%
  group_by(CNTSTUID) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = 'drop')

print("Pooling complete.")
saveRDS(pooled_features, file = OUTPUT_FEATURES_RDS)
print(paste("Success! Pooled engineered features saved to:", OUTPUT_FEATURES_RDS))