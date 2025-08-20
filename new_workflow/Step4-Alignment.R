# =================================================================
# Step 4 - Modified for Multiple Latent Variables and File Types
# =================================================================


# --- 1. Load Libraries ---
library(tidyverse)
library(here)
library(mice)
library(stringr)

# --- 2. Load and Prepare Data for Mplus ---
print("Loading imputed data from Step 3...")
INPUT_MICE_BUNDLE <- here("dataset", "", "Step3_Imputed_Data_List.rds")
saved_bundle <- readRDS(INPUT_MICE_BUNDLE)
imputed_object <- saved_bundle$mids_object

# --- Create Definitive Mapping ---
country_map <- imputed_object$data %>%
  distinct(CNT) %>%
  filter(!is.na(CNT) & CNT != "") %>%
  mutate(
    CNT = as.character(CNT),
    CNT_ID = row_number()
  )

student_country_map <- imputed_object$data %>%
  select(CNTSTUID, CNT) %>%
  mutate(
    CNTSTUID = as.character(CNTSTUID),
    CNT = as.character(CNT)
  ) %>%
  left_join(country_map, by = "CNT")


# --- 3. Loop Through Imputations and Save Corrected .dat Files ---

# --- Path Declaration for easy modification ---
mplus_data_dir <- here("dataset", "mplus_data")

print("Writing imputed datasets to .dat files for Mplus...")
if (!dir.exists(mplus_data_dir)) {
  dir.create(mplus_data_dir, recursive = TRUE)
}

dat_file_names <- c()
final_column_names <- NULL

for (i in 1:imputed_object$m) {
  file_name <- paste0("imputed_data_", i, ".dat")
  file_path <- file.path(mplus_data_dir, file_name)
  dat_file_names <- c(dat_file_names, file_path)

  completed_data <- mice::complete(imputed_object, i)

  # Ensure CNTSTUID is in the final data
  completed_data_final <- completed_data %>%
    mutate(CNTSTUID = as.character(CNTSTUID)) %>%
    select(-CNT) %>%
    left_join(student_country_map, by = "CNTSTUID") %>%
    select(-CNT) %>%
    rename(CNT = CNT_ID) %>%
    # Make sure CNT and CNTSTUID are at the front for consistency
    select(CNT, CNTSTUID, everything())

  if (i == 1) {
    final_column_names <- names(completed_data_final)
  }

  write.table(
    completed_data_final,
    file = file_path,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    quote = FALSE,
    na = "."
  )
  print(paste("Saved:", file_path))
}

# --- Create the file_list.txt that Mplus will read ---
print("Creating file_list.txt for Mplus...")
file_list_path <- file.path(mplus_data_dir, "file_list.txt")
writeLines(dat_file_names, con = file_list_path)
print(paste("Success! File list for Mplus created at:", file_list_path))


# =================================================================
# --- 4. Define Latent Variables and Generate Mplus Input Files ---
# =================================================================

# --- Define the list of latent variables and their indicators ---
latent_variables_list <- list(
  Math_Dispo = c('ANXMAT', 'MATHEFF', 'MATHEF21', 'MATHPERS', 'ST268Q04JA', 'ST268Q07JA', 'ST268Q01JA'),
  Social_Emo_Ski = c('ASSERAGR', 'COOPAGR', 'EMOCOAGR', 'EMPATAGR', 'PERSEVAGR'),
  Open_Creat = c('CURIOAGR', 'CREATEFF', 'CREATOP', 'IMAGINE', 'OPENART'),
  Self_Dir_Lear = c('SDLEFF', 'GROSAGR'),
  Tea_Class_Exp = c('TEACHSUP', 'RELATST', 'COGACRCO', 'COGACMCO', 'DISCLIM', 'CREATSCH'),
  Home_Learning_Env = c('FAMSUP', 'CREATFAM', 'FAMSUPSL'),
  Rem_Learning_Exp = c('FEELLAH', 'PROBSELF', 'LEARRES'),
  Sch_Exp = c('FEELSAFE', 'SCHRISK', 'BELONG', 'SCHSUST')
)

#' @title Generate Mplus Input File for Alignment (Flexible)
#' @description Creates a .inp file for an alignment model, either for pooled imputations or a single imputed dataset.
generate_alignment_inp_flexible <- function(lv_name, indicators, all_var_names, grouping_map, mplus_dir, data_list_path, dat_file_names, imputation_index = NULL) {

  # --- Create strings common to both file types ---
  names_are_string <- paste(all_var_names, collapse = "\n    ")
  model_string <- paste0("  ", lv_name, " BY ", paste(indicators, collapse = " "), ";")

  # --- Customize elements based on whether it's a pooled or single file ---
  if (is.null(imputation_index)) {
    # --- Case 1: POOLED IMPUTATION FILE ---
    title_string <- paste0("TITLE: Alignment for ", lv_name, " (ALL IMPUTATIONS - FREE);")
    inp_file_name <- paste0("Alignment_Imputation_", lv_name, ".inp")
    alignment_string <- "  ALIGNMENT = FREE;"
    usevars_string <- paste(c(indicators, "CNT"), collapse = " ")
    variable_block_additions <- "" # No IDVARIABLE for the pooled run
    savedata_block <- "" # No SAVEDATA for the pooled run

    data_block <- paste(
      "DATA:",
      paste0('  FILE IS "', data_list_path, '";'),
      "  TYPE = IMPUTATION;",
      "  FORMAT IS FREE;",
      sep = "\n"
    )

  } else {
    # --- Case 2: SINGLE IMPUTATION FILE ---
    title_string <- paste0("TITLE: Alignment for ", lv_name, " (IMPUTATION ", imputation_index, " - FIXED);")
    inp_file_name <- paste0("Alignment_Imp", imputation_index, "_", lv_name, ".inp")
    alignment_string <- "  ALIGNMENT = FIXED;"
    # Add CNTSTUID to USEVARIABLES
    usevars_string <- paste(c(indicators, "CNT", "CNTSTUID"), collapse = " ")
    # Add the IDVARIABLE command
    variable_block_additions <- "  IDVARIABLE IS CNTSTUID;"

    specific_dat_file <- dat_file_names[imputation_index]
    data_block <- paste(
      "DATA:",
      paste0('  FILE IS "', specific_dat_file, '";'),
      "  FORMAT IS FREE;",
      sep = "\n"
    )

    fscores_file_name <- paste0(str_to_lower(lv_name), "_scores_imp", imputation_index, ".dat")
    savedata_block <- paste(
      "", "SAVEDATA:",
      paste0('  FILE IS "', file.path(mplus_dir, fscores_file_name), '";'),
      "  SAVE = FSCORES;", "  FORMAT IS FREE;",
      sep = "\n"
    )
  }

  # --- Assemble the final .inp content ---
  final_inp_content <- paste(
    title_string,
    "", data_block,
    "", "VARIABLE:",
    "  NAMES ARE", paste0("    ", names_are_string, ";"),
    "  USEVARIABLES ARE", paste0("    ", usevars_string, ";"),
    variable_block_additions, # This adds IDVARIABLE line for single files
    "  GROUPING IS CNT (", paste0("    ", grouping_map), "  );",
    "", "ANALYSIS:",
    "  ESTIMATOR = MLR;", "  PROCESSORS = 4;",
    alignment_string, # Now uses FREE or FIXED appropriately
    "", "MODEL:", model_string,
    savedata_block,
    "", "OUTPUT:", "  TECH11;", "  ALIGNMENT;",
    sep = "\n"
  )

  # --- Write the Mplus Input File ---
  output_inp_file <- file.path(mplus_dir, inp_file_name)
  writeLines(final_inp_content, con = output_inp_file)
}


# --- Prepare common elements for the function ---
grouping_string <- country_map %>%
  mutate(map_string = paste0(CNT_ID, " = ", CNT)) %>%
  pull(map_string) %>%
  paste(collapse = "\n    ")


# --- Loop through the list and generate all .inp files ---
print("=================================================================")
print("Generating Mplus .inp files for each latent variable...")
num_imputations <- imputed_object$m

purrr::iwalk(
  latent_variables_list,
  ~ {
    lv_name <- .y
    indicators <- .x
    print(paste("--- Generating 6 files for latent variable:", lv_name, "---"))

    # 1. Generate the pooled imputation file (ALIGNMENT=FREE, no SAVEDATA)
    generate_alignment_inp_flexible(
      lv_name = lv_name, indicators = indicators,
      all_var_names = final_column_names, grouping_map = grouping_string,
      mplus_dir = mplus_data_dir, data_list_path = file_list_path,
      dat_file_names = dat_file_names, imputation_index = NULL
    )

    # 2. Generate one file per imputation (ALIGNMENT=FIXED, with SAVEDATA)
    for (i in 1:num_imputations) {
      generate_alignment_inp_flexible(
        lv_name = lv_name, indicators = indicators,
        all_var_names = final_column_names, grouping_map = grouping_string,
        mplus_dir = mplus_data_dir, data_list_path = file_list_path,
        dat_file_names = dat_file_names, imputation_index = i
      )
    }
  }
)

print("=================================================================")
print("All Mplus input files have been generated successfully.")