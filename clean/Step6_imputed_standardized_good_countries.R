# --- 0. Load Libraries ---
library(tidyverse) # For data manipulation (dplyr, readr, etc.)
library(jomo)      # The package for multilevel multiple imputation

# --- Configuration Section ---

# 1. Path to the dataset.
input_file <- "../dataset/clean/STU_includeVariablesSet.csv"

# 2. Path for the final imputed and standardized dataset.
output_file <- "../dataset/analysis/imputed_standardized_good_countries_R.csv"

# 3. Identifiers and Thresholds.
country_column <- "CNT"
school_column <- "CNTSCHID"
MISSING_VALUE_THRESHOLD <- 50.0
PROPORTION_FOR_GOOD_DATA <- 0.10
NUMBER_OF_IMPUTATIONS <- 5 # Standard is 5-10. Start with 5 for speed.

# 4. Define variable types for processing.
#    Add any other continuous variables from your list that you want to standardize.
CONTINUOUS_VARS <- c(
  "AGE", "MATHEFF", "ANXMAT", "MATHEF21", "MATHPERS", "GROSAGR", "ASSERAGR",
  "COOPAGR", "CURIOAGR", "EMOCOAGR", "EMPATAGR", "PERSEVAGR", "STRESAGR",
  "CREATEFF", "CREATOP", "IMAGINE", "OPENART", "SDLEFF"
)


# --- 1. Identify and Select "Good" Countries ---
cat("Step 1: Identifying and selecting 'good' countries...\n")

df <- read_csv(input_file, show_col_types = FALSE)

# Use dplyr to find good countries
missing_summary <- df %>%
  group_by(!!sym(country_column)) %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  ungroup()

is_highly_missing <- missing_summary %>%
  select(-!!sym(country_column)) >= (MISSING_VALUE_THRESHOLD / 100)

prop_missing_in_country <- rowMeans(is_highly_missing)

good_countries <- missing_summary[[country_column]][prop_missing_in_country < PROPORTION_FOR_GOOD_DATA]

if (length(good_countries) == 0) {
  stop("--- ERROR --- No countries met the criteria for 'good' data. Cannot proceed.")
}

df_pilot <- df %>%
  filter(!!sym(country_column) %in% good_countries)

cat(sprintf("Selected %d countries for pilot analysis: %s\n", length(good_countries), paste(good_countries, collapse=", ")))
cat(sprintf("Shape of pilot dataset: %d rows, %d columns\n", nrow(df_pilot), ncol(df_pilot)))


# --- 2. Perform Multilevel Multiple Imputation with jomo ---
cat("\nStep 2: Performing multilevel MICE with 'jomo'...\n")

# Identify variables that actually have missing data in our pilot sample
vars_to_impute <- colnames(df_pilot)[colSums(is.na(df_pilot)) > 0]

# jomo requires the cluster variable to be numeric. We'll convert it if it's not.
df_pilot[[school_column]] <- as.numeric(as.factor(df_pilot[[school_column]]))

# Perform the imputation. jomo handles the formula creation internally.
# It automatically uses other variables as predictors.
# The output is already a long-format dataframe with an 'Imputation' column.
imputed_data_long <- jomo(
  data = df_pilot,
  Y = vars_to_impute,
  clus = school_column,
  nimp = NUMBER_OF_IMPUTATIONS,
  nburn = 1000 # Burn-in iterations for the MCMC chain
)

cat(sprintf("Multilevel imputation complete. Generated %d datasets.\n", NUMBER_OF_IMPUTATIONS))

# --- 3. Standardize Each Imputed Dataset ---
cat("\nStep 3: Standardizing continuous variables within each imputed dataset...\n")

# We can do this elegantly using dplyr's group_by and mutate
standardized_imputed_data <- imputed_data_long %>%
  group_by(Imputation) %>%
  mutate(across(
    all_of(CONTINUOUS_VARS),
    ~ as.vector(scale(.x))
  )) %>%
  ungroup()

cat("Standardization complete for all imputed datasets.\n")


# --- 4. Save Results ---
cat("\nStep 4: Saving final long-format data to file...\n")

# Ensure the output directory exists
output_dir <- dirname(output_file)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(standardized_imputed_data, output_file)

cat(sprintf("\nProcess complete. Imputed and standardized data saved to:\n%s\n", output_file))