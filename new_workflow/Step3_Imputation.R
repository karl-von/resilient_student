# =================================================================
# Step 3: Final Sample Selection & MULTILEVEL Multiple Imputation
# =================================================================


# --- 1. Load Libraries and Data ---
library(dplyr)
library(here)
library(mice)
library(haven)
library(miceadds)

# --- Define File Paths ---
INPUT_RDS_STEP2 <- here("dataset", "", "Step2_Merged_Cleaned.rds")
OUTPUT_RDS_MICE <- here("dataset", "", "Step3_Imputed_Mice_Object.rds")
OUTPUT_PLOT_DIR <- here("dataset", "analysis", "imputation_diagnostics")

print("Loading cleaned data from Step 2...")
data_cleaned <- readRDS(INPUT_RDS_STEP2)


# --- 2. Identify Final Analytical Sample ---
# This section is unchanged and remains correct.
# It identifies countries with no structural missingness on key predictors.
print("Identifying countries for the final analytical sample...")
predictor_vars <- c(
  "ST004D01T", "AGE", "ISCEDP", "IMMIG", "EXPECEDU", "BSMJ", "SISCO", "GROSAGR",
  "ANXMAT", "MATHEFF", "MATHEF21", "MATHPERS", "ASSERAGR", "COOPAGR", "CURIOAGR",
  "EMOCOAGR", "EMPATAGR", "PERSEVAGR", "STRESAGR", "CREATEFF", "CREATOP",
  "IMAGINE", "OPENART", "SDLEFF", "ST268Q04JA", "ST268Q07JA", "ST268Q01JA",
  "REPEAT", "MISSSC", "SKIPPING", "TARDYSD", "EXERPRAC", "STUDYHMW", "WORKPAY",
  "WORKHOME", "INFOSEEK", "EXPOFA", "EXPO21ST", "CREATAS", "CREATOOS",
  "TEACHSUP", "RELATST", "COGACRCO", "COGACMCO", "DISCLIM", "CREATSCH", "FAMSUP",
  "CREATFAM", "FAMSUPSL", "FEELLAH", "PROBSELF", "LEARRES", "BULLIED",
  "FEELSAFE", "SCHRISK", "BELONG", "SCHSUST", "OCOD3_major_group"
)
predictor_vars_exist <- predictor_vars[predictor_vars %in% names(data_cleaned)]

problematic_country_list <- data_cleaned %>%
  group_by(CNT) %>%
  summarise(max_missing_pct = max(across(all_of(predictor_vars_exist), ~mean(is.na(.)))), .groups = 'drop') %>%
  filter(max_missing_pct >= 1.0) %>%
  pull(CNT) %>%
  unique()

all_countries <- unique(data_cleaned$CNT)
countries_to_keep <- setdiff(all_countries, problematic_country_list)

print(paste("Identified", length(countries_to_keep), "countries for the final analytical sample."))

# Filter to the final analytical sample
data_for_imputation <- data_cleaned %>%
  filter(CNT %in% countries_to_keep) %>%

  mutate(
    # The cluster variable MUST be an integer for multilevel mice.
    # it use as.factor() first to handle any non-numeric school IDs robustly,
    # then convert the resulting factor levels to integers.
    CNTSCHID = as.integer(as.factor(CNTSCHID)),

    # Other IDs and categorical variables can remain as factors.
    CNT = as.factor(CNT),
    CNTSTUID = as.factor(CNTSTUID),
    ST004D01T = as.factor(ST004D01T), # Gender
    IMMIG = as.factor(IMMIG)
  ) %>%
  # to convert any remaining labelled columns to numeric.
  mutate(across(where(haven::is.labelled), as.numeric))

# --- 3. Run MULTILEVEL Multiple Imputation using mice ---
# This section is heavily modified to respect the nested data structure.

print("Setting up for MULTILEVEL imputation...")

# --- A. Initialize mice to get the configuration ---
# We run with maxit=0 to get the predictor matrix and methods vector without imputing.
ini <- mice(data_for_imputation, maxit = 0)
pred_matrix <- ini$predictorMatrix
meth_vector <- ini$method

# --- B. Refined setup for the Predictor Matrix and Method Vector ---
# This new section correctly includes the sampling weight as a predictor.

# First, get all variable names from the dataset
all_column_names <- names(data_for_imputation)

# Initialize mice to get the configuration
ini <- mice(data_for_imputation, maxit = 0)
pred_matrix <- ini$predictorMatrix
meth_vector <- ini$method

# --- Step B1: Define variables that should NOT BE IMPUTED ---
# This includes IDs, ALL weights (main and replicate), and outcomes.
vars_to_not_impute <- c(
  "CNTSTUID", "CNT", "CNTSCHID",
  "W_FSTUWT",
  all_column_names[startsWith(all_column_names, "W_FSTURWT")],
  all_column_names[startsWith(all_column_names, "RESILIENCE_SCORE_PV")],
  all_column_names[startsWith(all_column_names, "PV")]
)
meth_vector[vars_to_not_impute] <- ""


# --- Step B2: Define variables that should NOT BE USED AS PREDICTORS ---
# This includes IDs and outcomes. Critically, W_FSTUWT is NOT in this list.
# We exclude the 80 replicate weights as predictors to reduce noise and computation.
vars_to_not_use_as_predictors <- c(
  "CNTSTUID", "CNT",
  all_column_names[startsWith(all_column_names, "W_FSTURWT")],
  all_column_names[startsWith(all_column_names, "RESILIENCE_SCORE_PV")]
  # all_column_names[startsWith(all_column_names, "ACADEMIC_RESILIENCE_PV")]
)
pred_matrix[, vars_to_not_use_as_predictors] <- 0


# --- C. Specify the multilevel structure ---
# The cluster variable MUST be an integer and specified with -2.
pred_matrix[, "CNTSCHID"] <- -2


# --- D. Set the two-level imputation method ---
# For all variables that still have an imputation method assigned, setting it to '2l.pmm'.
vars_to_impute <- names(meth_vector[meth_vector != ""])
meth_vector[vars_to_impute] <- "2l.pmm"

# --- E. Run the final imputation ---
print(colSums(is.na(data_for_imputation)))
imputed_object <- mice(
  data_for_imputation,
  m = 5,
  maxit = 30,
  predictorMatrix = pred_matrix,
  method = meth_vector,
  seed = 12345
)

print("Multilevel multiple imputation complete.")

# --- 4. Bundle IDs and Imputed Data and Save ---

# --- PART A: Save the original MICE object and IDs (for diagnostics/archival) ---
print("Bundling IDs and imputed data into a single object...")
OUTPUT_RDS_MICE <- here("dataset", "", "Step3_Imputed_Mice_Object.rds") # Original file path

# Create a clean dataframe of just the ID columns.
ids_df <- data_for_imputation %>%
  select(CNTSTUID, CNTSCHID, CNT)

# Create a list to hold both the mids object and the IDs
output_to_save <- list(
  mids_object = imputed_object,
  ids = ids_df
)
# Save the single list object.
saveRDS(output_to_save, file = OUTPUT_RDS_MICE)
print(paste("Success! Bundled multilevel MICE object saved to:", OUTPUT_RDS_MICE))


# --- PART B (NEW): Convert MICE object to a simple list and save ---
print("Converting MICE object to a simple list of 5 dataframes for easy analysis...")
OUTPUT_RDS_LIST <- here("dataset", "", "Step3_Imputed_Data_List.rds") # New output file path

list_of_imputed_datasets <- list()
for (i in 1:imputed_object$m) {
  list_of_imputed_datasets[[i]] <- mice::complete(imputed_object, i)
}

# Save the simple list of dataframes. This will be the direct input for Step 6.
saveRDS(list_of_imputed_datasets, file = OUTPUT_RDS_LIST)
print(paste("Success! List of 5 imputed dataframes saved to:", OUTPUT_RDS_LIST))
