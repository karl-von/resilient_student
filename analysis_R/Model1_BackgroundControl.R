# =================================================================
# Analysis Step B: Model 1 - Alternative using the `survey` package (Final)
# =================================================================

# --- 1. Load Libraries ---
library(survey)
library(dplyr)
library(here)
library(mitools)

# --- 2. Load and Prepare Data ---
print("Loading the list of 10 model-ready datasets...")
INPUT_MODEL_DATA <- here("dataset", "clean_R", "Step5_Final_Model_Ready_List.rds")
model_ready_list <- readRDS(INPUT_MODEL_DATA)
OUTPUT_MODEL_1 <- here("dataset", "analysis", "Model1_Results.rds")

print("Preparing data...")

# --- Convert Numeric Variables ---
# Strips haven attributes before converting to numeric
vars_to_numeric <- c("ESCS", "AGE", "BSMJ", "ESCS_sch_mean", "AGE_sch_mean", "BSMJ_sch_mean")
model_ready_list <- lapply(model_ready_list, function(df) {
  df %>% mutate(across(all_of(vars_to_numeric), ~ as.numeric(as.vector(.x))))
})

# --- Convert Categorical Variables to Factors ---
# Strips haven attributes before converting to factor (FINAL FIX)
vars_to_factor <- c("FEMALE", "ISCEDP", "IMMIG", "OCOD3_major_group", "CNT")
model_ready_list <- lapply(model_ready_list, function(df) {
  df %>% mutate(across(all_of(vars_to_factor), ~ as.factor(as.vector(.x))))
})

# --- Center Continuous Variables ---
vars_to_center_L1 <- c("ESCS", "AGE", "BSMJ")
vars_to_center_L2 <- c("ESCS_sch_mean", "AGE_sch_mean", "BSMJ_sch_mean")
model_ready_list_centered <- lapply(model_ready_list, function(df) {
  df <- df %>%
    mutate(across(all_of(vars_to_center_L2), ~ .x - mean(.x, na.rm = TRUE), .names = "c.{.col}"))
  df <- df %>%
    group_by(CNTSCHID) %>%
    mutate(across(all_of(vars_to_center_L1), ~ .x - mean(.x, na.rm = TRUE), .names = "c.{.col}")) %>%
    ungroup()
  return(df)
})
print("Data preparation complete.")


# --- 3. Define the Model and Run Analysis with `survey` ---

# Define the model formula
model_formula <- RESILIENCE ~ c.ESCS + c.AGE + c.BSMJ + FEMALE + ISCEDP + IMMIG +
  OCOD3_major_group + c.ESCS_sch_mean +
  c.AGE_sch_mean + c.BSMJ_sch_mean + CNT

print("Running analysis for each of the 10 plausible values...")

list_of_models <- lapply(model_ready_list_centered, function(pv_data) {
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
    formula = model_formula,
    design = pisa_design,
    family = quasibinomial()
  )
})

print("Analysis complete. Pooling results...")


# --- 4. Pool the Results and Display ---
final_model <- MIcombine(list_of_models)

print("==================================================")
print("     RESULTS FOR MODEL 1 (using `survey` package)   ")
print("==================================================")

summary(final_model)
saveRDS(final_model, file = OUTPUT_MODEL_1)
print("==================================================")