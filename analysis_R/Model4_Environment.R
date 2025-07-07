
# =================================================================
# Analysis Step D: Model 4 (Final Full Model) - Environment & Context
# =================================================================

# --- 1. Load Libraries ---
library(survey)
library(dplyr)
library(here)
library(mitools)

# --- 2. Load and Prepare Data ---
print("Loading the list of 10 model-ready datasets...")
INPUT_MODEL_DATA <- here("dataset", "clean_R", "Step5_Final_Model_Ready_List.rds")
OUTPUT_MODEL_3 <- here("dataset", "analysis", "Model3_Results.rds") # Path to load M3
OUTPUT_MODEL_4 <- here("dataset", "analysis", "Model4_Results.rds") # Path to save M4

model_ready_list <- readRDS(INPUT_MODEL_DATA)

print("Preparing data...")

# --- Define all variable lists for clarity ---
vars_to_numeric <- c("ESCS", "AGE", "BSMJ", "EXPECEDU", "SISCO", "ESCS_sch_mean",
                     "AGE_sch_mean", "BSMJ_sch_mean")
vars_to_factor <- c("FEMALE", "ISCEDP", "IMMIG", "OCOD3_major_group", "CNT",
                    "REPEAT", "MISSSC", "SKIPPING", "TARDYSD")

# NEW: Add all other school-average variables that were created in Step 4
# We will center these and add them to the model
vars_to_center_L2 <- c("ESCS_sch_mean", "AGE_sch_mean", "BSMJ_sch_mean",
                       "Math_Disposition_RC1_sch_mean", "Social_Emotional_Skills_RC1_sch_mean",
                       "Openness_Creativity_PC1_sch_mean", "Self_Directed_Learning_PC1_sch_mean",
                       "Teacher_Classroom_Exp_RC1_sch_mean", "Home_Learning_Env_PC1_sch_mean",
                       "Remote_Learning_Exp_PC1_sch_mean", "School_Experience_RC1_sch_mean", "School_Experience_RC2_sch_mean")

vars_to_center_L1 <- c("ESCS", "AGE", "BSMJ", "EXPECEDU", "SISCO")

# --- Apply all data transformations ---
model_ready_list_centered <- lapply(model_ready_list, function(df) {
  df <- df %>%
    mutate(across(all_of(vars_to_numeric), ~ as.numeric(as.vector(.x)), .names = "{.col}")) %>%
    mutate(across(all_of(vars_to_factor), ~ as.factor(as.vector(.x)), .names = "{.col}"))

  # Center Level 2 (school) variables around the grand mean
  df <- df %>%
    mutate(across(all_of(vars_to_center_L2), ~ .x - mean(.x, na.rm = TRUE), .names = "c.{.col}"))

  # Center Level 1 (student) variables around their school mean
  df <- df %>%
    group_by(CNTSCHID) %>%
    mutate(across(all_of(vars_to_center_L1), ~ .x - mean(.x, na.rm = TRUE), .names = "c.{.col}")) %>%
    ungroup()

  return(df)
})
print("Data preparation complete.")


# --- 3. Define the Full Model 4 Formula ---
model_formula_M4 <- RESILIENCE ~
  # Block 1: Baseline variables
  c.ESCS + c.AGE + c.BSMJ + FEMALE + ISCEDP + IMMIG + OCOD3_major_group + CNT +

    # Block 2: Psychological variables
    c.EXPECEDU + c.SISCO + Math_Disposition_RC1 + Math_Disposition_RC2 +
    Social_Emotional_Skills_RC1 + Social_Emotional_Skills_RC2 +
    Openness_Creativity_PC1 + Self_Directed_Learning_PC1 +

    # Block 3: Practice variables
    REPEAT + MISSSC + SKIPPING + TARDYSD + EXERPRAC + STUDYHMW + WORKPAY + WORKHOME +
    INFOSEEK + EXPOFA + EXPO21ST + CREATAS + CREATOOS +

    # --- NEW VARIABLES FOR MODEL 4 (ENVIRONMENT & CONTEXT) ---
    # Teacher and Home Environment
    Teacher_Classroom_Exp_RC1 + Teacher_Classroom_Exp_RC2 +
    Home_Learning_Env_PC1 + Remote_Learning_Exp_PC1 +

    # School Experience (The overlooked variables)
    School_Experience_RC1 + School_Experience_RC2 +

    # School Contextual Effects (School-level averages)
    c.ESCS_sch_mean + c.AGE_sch_mean + c.BSMJ_sch_mean +
    c.Math_Disposition_RC1_sch_mean + c.Social_Emotional_Skills_RC1_sch_mean +
    c.Openness_Creativity_PC1_sch_mean + c.Self_Directed_Learning_PC1_sch_mean +
    c.Teacher_Classroom_Exp_RC1_sch_mean + c.Home_Learning_Env_PC1_sch_mean +
    c.Remote_Learning_Exp_PC1_sch_mean + c.School_Experience_RC1_sch_mean + c.School_Experience_RC2_sch_mean


# --- 4. Run Analysis for Each Plausible Value ---
print("Running analysis for the Final Full Model (Model 4)...")
list_of_models_M4 <- lapply(model_ready_list_centered, function(pv_data) {
  # (svrepdesign and svyglm code is the same as before, just using the new formula)
  pisa_design <- svrepdesign(data = pv_data, cluster = ~CNTSCHID, weights = ~W_FSTUWT,
                             repweights = "W_FSTURWT[0-9]+", type = "Fay", rho = 0.5)
  svyglm(formula = model_formula_M4, design = pisa_design, family = quasibinomial())
})

# --- 5. Pool, Save, and Display Results ---
final_model_M4 <- MIcombine(list_of_models_M4)
saveRDS(final_model_M4, file = OUTPUT_MODEL_4)

print("==================================================")
print("     RESULTS FOR THE FINAL FULL MODEL (MODEL 4)   ")
print("==================================================")
summary(final_model_M4)
print("==================================================")
