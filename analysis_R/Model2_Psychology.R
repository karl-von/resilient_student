# =================================================================
# Analysis Step B: Model 2 - Adding Psychological Factors
# =================================================================

# --- 1. Load Libraries ---
library(survey)
library(dplyr)
library(here)
library(mitools)

# --- 2. Load and Prepare Data ---
print("Loading the list of 10 model-ready datasets...")
INPUT_MODEL_DATA <- here("dataset", "clean_R", "Step5_Final_Model_Ready_List.rds")
OUTPUT_MODEL_1 <- here("dataset", "analysis", "Model1_Results.rds") # Path to save/load M1
OUTPUT_MODEL_2 <- here("dataset", "analysis", "Model2_Results.rds") # Path to save M2

model_ready_list <- readRDS(INPUT_MODEL_DATA)

print("Preparing data...")

# --- Data Cleaning and Centering ---
# This section is identical to Model 1 but includes the new continuous psychological variables
vars_to_numeric_M2 <- c("ESCS", "AGE", "BSMJ", "EXPECEDU", "SISCO",
                        "ESCS_sch_mean", "AGE_sch_mean", "BSMJ_sch_mean")

vars_to_factor_M2 <- c("FEMALE", "ISCEDP", "IMMIG", "OCOD3_major_group", "CNT")

vars_to_center_L1_M2 <- c("ESCS", "AGE", "BSMJ", "EXPECEDU", "SISCO") # Added EXPECEDU & SISCO
vars_to_center_L2_M2 <- c("ESCS_sch_mean", "AGE_sch_mean", "BSMJ_sch_mean")

model_ready_list_centered <- lapply(model_ready_list, function(df) {
  df <- df %>%
    mutate(across(all_of(vars_to_numeric_M2), ~ as.numeric(as.vector(.x)))) %>%
    mutate(across(all_of(vars_to_factor_M2), ~ as.factor(as.vector(.x))))

  df <- df %>%
    mutate(across(all_of(vars_to_center_L2_M2), ~ .x - mean(.x, na.rm = TRUE), .names = "c.{.col}"))

  df <- df %>%
    group_by(CNTSCHID) %>%
    mutate(across(all_of(vars_to_center_L1_M2), ~ .x - mean(.x, na.rm = TRUE), .names = "c.{.col}")) %>%
    ungroup()

  return(df)
})
print("Data preparation complete.")


# --- 3. Define the Model 2 Formula and Run Analysis ---

# This formula adds the psychological variables to the Model 1 formula
model_formula_M2 <- RESILIENCE ~ c.ESCS + c.AGE + c.BSMJ + FEMALE + ISCEDP + IMMIG +
  OCOD3_major_group + c.ESCS_sch_mean + c.AGE_sch_mean + c.BSMJ_sch_mean + CNT +
  c.EXPECEDU + c.SISCO +
  Math_Disposition_RC1 + Math_Disposition_RC2 +
  Social_Emotional_Skills_RC1 + Social_Emotional_Skills_RC2 +
  Openness_Creativity_PC1 +
  Self_Directed_Learning_PC1


# --- DIAGNOSTIC CHECK ---
# Get all variable names from your formula string
formula_vars <- all.vars(model_formula_M2)

# Get all column names from your actual dataset (we'll just check the first one)
data_vars <- names(model_ready_list_centered[[1]])

# Find which variables from the formula are NOT in the dataset
missing_vars <- setdiff(formula_vars, data_vars)

if (length(missing_vars) > 0) {
  print("ERROR: The following variables are in your formula but not in your data:")
  print(missing_vars)
} else {
  print("SUCCESS: All variables in the formula were found in the dataset.")
}
# --- END DIAGNOSTIC ---

print("Running analysis for Model 2 for each of the 10 plausible values...")

list_of_models_M2 <- lapply(model_ready_list_centered, function(pv_data) {
  pisa_design <- svrepdesign(
    data = pv_data,
    cluster = ~CNTSCHID,
    weights = ~W_FSTUWT,
    repweights = "W_FSTURWT[0-9]+",
    type = "Fay",
    rho = 0.5,
    combined.weights = TRUE
  )

  svyglm(
    formula = model_formula_M2,
    design = pisa_design,
    family = quasibinomial()
  )
})


# --- 4. Pool and Save Model 2 Results ---
final_model_M2 <- MIcombine(list_of_models_M2)
saveRDS(final_model_M2, file = OUTPUT_MODEL_2)

print("==================================================")
print("     RESULTS FOR MODEL 2 (Psychological Factors)    ")
print("==================================================")
summary(final_model_M2)
print("==================================================")


# --- 5. Compare Model 1 vs. Model 2 ---
print("Comparing Model 2 against Model 1 using a Wald test...")

# Load the pooled results from Model 1
final_model_M1 <- readRDS(OUTPUT_MODEL_1)

# Define the terms that were ADDED to Model 1 to create Model 2
new_terms_in_M2 <- c(
  "c.EXPECEDU", "c.SISCO",
  "Math_Disposition_RC1", "Math_Disposition_RC2",
  "Social_Emotional_Skills_RC1", "Social_Emotional_Skills_RC2",
  "Openness_Creativity_PC1",
  "Self_Directed_Learning_PC1"
)


# Perform the Wald test
# This tests the null hypothesis that the coefficients for all new terms are jointly zero
model_comparison <- regTermTest(final_model_M2, test.terms = new_terms_in_M2, null=final_model_M1)

print("--- Model Comparison (Model 1 vs Model 2) ---")
print(model_comparison)
print("---")
print("Interpretation: A p-value < 0.05 indicates that adding the psychological variables resulted in a statistically significant improvement in model fit.")
print("==================================================")