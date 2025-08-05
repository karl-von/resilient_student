# --- 1. Load Libraries ---
library(mice)
library(survey)
library(tidyverse)
library(here)
library(progress)

# --- 2. Load and Prepare Data ---
print("Loading the list of 5 engineered datasets...")
INPUT_FINAL_LIST <- here("dataset", "", "Step4_ListOfEngineeredDatasets.rds")
list_of_engineered_datasets <- readRDS(INPUT_FINAL_LIST)

# --- Pre-process dataframes before creating survey objects ---
list_of_engineered_datasets <- lapply(list_of_engineered_datasets, function(df) {
  df %>%
    mutate(
      # Create FEMALE variable from ST004D01T (1=Female, 2=Male)
      FEMALE = as.factor(if_else(ST004D01T == 1, 1, 0)),
      # Ensure other categorical predictors are factors
      IMMIG = as.factor(IMMIG)
    )
})

print("Data loaded and pre-processed successfully.")

# --- 3. Create a List of Survey Design Objects ---
print("Creating survey design objects for each imputed dataset...")
survey_design_list <- lapply(list_of_engineered_datasets, function(df) {
  svrepdesign(
    weights = ~W_FSTUWT,
    repweights = "W_FSTURWT[0-9]+",
    data = df,
    type = "Fay",
    rho = 0.5,
    combined.weights = TRUE
  )
})
print("List of 5 survey design objects created successfully.")


# --- 4. Define Model Formulas ---

# Model 1: Includes only the main background control variables.
model_1_formula_rhs <- "ESCS + AGE + FEMALE + IMMIG + CNT + ISCEDP"


# Model 2: Includes background controls PLUS all main predictors of interest.
model_2_formula_rhs <- paste(
  # Background Controls
  "ESCS + AGE + FEMALE + IMMIG + CNT + ISCEDP",

  # PCA Components
  "Math_Disposition_RC1 + Math_Disposition_RC2 + Social_Emotional_Skills_RC1",
  "Social_Emotional_Skills_RC2 + Openness_Creativity_PC1 + Self_Directed_Learning_PC1",
  "Teacher_Classroom_Exp_RC1 + Teacher_Classroom_Exp_RC2 + Home_Learning_Env_PC1",
  "Remote_Learning_Exp_PC1 + School_Experience_RC1 + School_Experience_RC2",

  # Other Psychology Variables
  "EXPECEDU + OCOD3_major_group + BSMJ + SISCO",

  # Other Practice & Behavior Variables
  "REPEAT + MISSSC + SKIPPING + TARDYSD + EXERPRAC + STUDYHMW + WORKPAY",
  "WORKHOME + INFOSEEK + EXPOFA + EXPO21ST + CREATAS + CREATOOS",

  sep = " + "
)

# Model 3: sensitivity check:
# **Mathematics self-efficacy: Formal and applied mathematics (MATHEFF)
# **Mathematics self-efficacy: Mathematical reasoning and 21st century mathematics(MATHEF21)
# Agree/disagree: Mathematics is easy for me.(ST268Q04JA)
# It is hard to determine whether such feelings contribute to students’ success in mathematics, or whether their success leads to stronger confidence.
model_3_formula_rhs <- paste(
  # Background Controls
  "ESCS + AGE + FEMALE + IMMIG + CNT + ISCEDP",

  "Math_Disposition_Sensitivity_RC1 + Math_Disposition_Sensitivity_RC2 + Social_Emotional_Skills_RC1",
  "Social_Emotional_Skills_RC2 + Openness_Creativity_PC1 + Self_Directed_Learning_PC1",
  "Teacher_Classroom_Exp_RC1 + Teacher_Classroom_Exp_RC2 + Home_Learning_Env_PC1",
  "Remote_Learning_Exp_PC1 + School_Experience_RC1 + School_Experience_RC2",

  # Other Psychology Variables
  "EXPECEDU + OCOD3_major_group + BSMJ + SISCO",

  # Other Practice & Behavior Variables
  "REPEAT + MISSSC + SKIPPING + TARDYSD + EXERPRAC + STUDYHMW + WORKPAY",
  "WORKHOME + INFOSEEK + EXPOFA + EXPO21ST + CREATAS + CREATOOS",

  sep = " + "
)

# --- 5. Run and Aggregate the Models ---

run_pisa_model <- function(formula_rhs, survey_designs, model_name) {

  # --- Main analysis loop across 10 Plausible Values ---
  print(paste("Running", model_name, "- This may take a while..."))

  resilience_outcomes <- paste0("ACADEMIC_RESILIENCE_PV", 1:10)
  pv_model_list <- list()

  # Initialize the progress bar
  pb <- progress_bar$new(
    format = paste("  Analyzing", model_name, "[:bar] :percent (ETA: :eta)"),
    total = length(resilience_outcomes),
    clear = FALSE,
    width = 60
  )

  for (outcome_variable in resilience_outcomes) {
    model_formula <- as.formula(paste(outcome_variable, "~", formula_rhs))

    # Run the survey model on the list of imputed survey objects
    model_fits <- lapply(survey_designs, function(design_obj) {
      svyglm(formula = model_formula, design = design_obj, family = quasibinomial())
    })

    # Pool the results from the 5 imputations
    pooled_model <- pool(model_fits)
    pv_model_list[[outcome_variable]] <- pooled_model

    # Tick the progress bar
    pb$tick()
  }

  # --- Aggregate results across all 10 Plausible Value models ---
  print(paste("\nAggregating results for", model_name, "..."))

  all_results_df <- tibble()
  for (i in 1:length(pv_model_list)) {
    model_summary <- summary(pv_model_list[[i]])
    model_summary$pv <- names(pv_model_list)[i]
    all_results_df <- bind_rows(all_results_df, model_summary)
  }

  final_summary <- all_results_df %>%
    group_by(term) %>%
    summarise(
      final_estimate = mean(estimate),
      sampling_variance = mean(std.error^2),
      pv_variance = var(estimate),
      M = n(),
      total_variance = sampling_variance + (1 + 1/M) * pv_variance,
      final_std_error = sqrt(total_variance),
      t_statistic = final_estimate / final_std_error,
      .groups = "drop"
    ) %>%
    select(term, final_estimate, final_std_error, t_statistic)

  # --- Save the final results to a CSV file ---
  output_filename <- here("dataset", "analysis", paste0("Results_", model_name, ".csv"))
  write_csv(final_summary, output_filename)
  print(paste("✅ Results for", model_name, "saved to:", output_filename))

  return(final_summary)
}

# --- Run models ---
results_model_1 <- run_pisa_model(model_1_formula_rhs, survey_design_list, "Model_Controls")
results_model_2 <- run_pisa_model(model_2_formula_rhs, survey_design_list, "Model_Full")
results_model_3 <- run_pisa_model(model_3_formula_rhs, survey_design_list, "Model_Sensitivity")
# --- 6. Display Final Results ---
print("==================================================================")
print("          FINAL RESULTS FOR MODEL 1 (CONTROLS ONLY)               ")
print("==================================================================")
print(results_model_1, n = 50)
print("==================================================================")

print("\n\n")

print("==================================================================")
print("            FINAL RESULTS FOR MODEL 2 (FULL MODEL)                ")
print("==================================================================")
print(results_model_2, n = 50)
print("==================================================================")

print("\n\n")

print("==================================================================")
print("            FINAL RESULTS FOR MODEL 3 (SENSITIVITY MODEL)                ")
print("==================================================================")
print(results_model_3, n = 50)
print("==================================================================")