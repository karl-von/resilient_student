# =================================================================
# Analysis Step C: Model 3 - Adding Practice Variables
# =================================================================

# --- 1. Load Libraries ---
library(survey)
library(dplyr)
library(here)
library(mitools)

# --- 2. Load and Prepare Data ---
print("Loading the list of 10 model-ready datasets...")
INPUT_MODEL_DATA <- here("dataset", "clean_R", "Step5_Final_Model_Ready_List.rds")
OUTPUT_MODEL_3 <- here("dataset", "analysis", "Model3_Results.rds")

model_ready_list <- readRDS(INPUT_MODEL_DATA)

print("Preparing data...")

# --- Numeric Variables ---
vars_to_numeric <- c("ESCS", "AGE", "BSMJ", "EXPECEDU", "SISCO",
                     "ESCS_sch_mean", "AGE_sch_mean", "BSMJ_sch_mean")

# --- Factor Variables ---
vars_to_factor <- c("FEMALE", "ISCEDP", "IMMIG", "OCOD3_major_group", "CNT")

# --- Behavioral Binary Variables ---
vars_to_factor_behavior <- c("REPEAT", "MISSSC", "SKIPPING", "TARDYSD")

# --- Centering Variables ---
vars_to_center_L1 <- c("ESCS", "AGE", "BSMJ", "EXPECEDU", "SISCO")
vars_to_center_L2 <- c("ESCS_sch_mean", "AGE_sch_mean", "BSMJ_sch_mean")

# --- Apply transformation to each dataset ---
model_ready_list_centered <- lapply(model_ready_list, function(df) {
  df <- df %>%
    mutate(across(all_of(vars_to_numeric), ~ as.numeric(as.vector(.x)))) %>%
    mutate(across(all_of(vars_to_factor), ~ as.factor(as.vector(.x)))) %>%
    mutate(across(all_of(vars_to_factor_behavior), ~ as.factor(as.vector(.x)))) %>%
    mutate(across(all_of(vars_to_center_L2), ~ .x - mean(.x, na.rm = TRUE), .names = "c.{.col}")) %>%
    group_by(CNTSCHID) %>%
    mutate(across(all_of(vars_to_center_L1), ~ .x - mean(.x, na.rm = TRUE), .names = "c.{.col}")) %>%
    ungroup()
  return(df)
})

print("Data preparation complete.")

# --- 3. Define Model 3 Formula ---
model_formula_M3 <- RESILIENCE ~
  # Baseline variables
  c.ESCS + c.AGE + c.BSMJ + FEMALE + ISCEDP + IMMIG +
    OCOD3_major_group + c.ESCS_sch_mean + c.AGE_sch_mean + c.BSMJ_sch_mean + CNT +
    # Psychological variables
    c.EXPECEDU + c.SISCO +
    Math_Disposition_RC1 + Math_Disposition_RC2 +
    Social_Emotional_Skills_RC1 + Social_Emotional_Skills_RC2 +
    Openness_Creativity_PC1 + Self_Directed_Learning_PC1 +
    # Engagement/Activity variables
    REPEAT + MISSSC + SKIPPING + TARDYSD +
    EXERPRAC + STUDYHMW + WORKPAY + WORKHOME +
    INFOSEEK + EXPOFA + EXPO21ST +
    CREATAS + CREATOOS

# --- Diagnostic: Check all variables exist ---
formula_vars <- all.vars(model_formula_M3)
data_vars <- names(model_ready_list_centered[[1]])
missing_vars <- setdiff(formula_vars, data_vars)

if (length(missing_vars) > 0) {
  print("ERROR: The following variables are in your formula but not in your data:")
  print(missing_vars)
} else {
  print("SUCCESS: All variables in the formula were found in the dataset.")
}

# --- 4. Run Analysis for Each Plausible Value ---
print("Running analysis for Model 3 for each of the 10 plausible values...")

list_of_models_M3 <- lapply(model_ready_list_centered, function(pv_data) {
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
    formula = model_formula_M3,
    design = pisa_design,
    family = quasibinomial()
  )
})

# --- 5. Pool and Save Results ---
final_model_M3 <- MIcombine(list_of_models_M3)

print("==================================================")
print("     RESULTS FOR MODEL 3 (Engagement Variables)   ")
print("==================================================")
summary(final_model_M3)
print("==================================================")

saveRDS(final_model_M3, file = OUTPUT_MODEL_3)