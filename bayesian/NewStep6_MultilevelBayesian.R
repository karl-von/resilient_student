# ==============================================================================
# SOLUTION STEP 2: Robust Bayesian Multilevel Model with brms
#
# Purpose: To run the full multilevel model, correctly incorporating
#          multiple imputations and survey weights in a stable framework.
#
# Note: This model is run on ONE Plausible Value (PV1) as an example.
#       For the bayesian analysis, this should be looped over all 10 PVs.
# ==============================================================================


# --- 1. SETUP | Load Required Libraries ---
# brms is the main package for Bayesian multilevel modeling.
library(brms)
library(dplyr)
library(here)

# Set options for parallel computing to speed up the model
# This tells brms to use 4 cores of your computer's CPU.
options(mc.cores = 4)


# --- 2. DATA INPUT & PREPARATION ---
# This section is identical to the corrected diagnostic script.
# It ensures all data is loaded, merged, and pre-processed.
print("Loading and preparing data...")

# Load imputed data list
imputed_data_path <- here("dataset", "", "Step3_Imputed_Data_List_bayesian.rds")
list_of_imputed_datasets <- readRDS(imputed_data_path)

# Load latent variable scores
lv_scores_path <- here("dataset", "Step5_lantent_variables.rds")
lv_scores_long <- readRDS(lv_scores_path)

# Merge latent variables into each imputed dataset
list_of_merged_datasets <- vector("list", length = length(list_of_imputed_datasets))
for (i in 1:length(list_of_imputed_datasets)) {
  main_df <- list_of_imputed_datasets[[i]]
  lv_scores_for_imp <- lv_scores_long %>% filter(.imp == i)
  main_df$CNTSTUID <- as.character(main_df$CNTSTUID)
  lv_scores_for_imp$.id <- as.character(lv_scores_for_imp$.id)
  merged_df <- left_join(main_df, lv_scores_for_imp, by = c("CNTSTUID" = ".id"))
  list_of_merged_datasets[[i]] <- merged_df
}

# Pre-process each dataset (create FEMALE, etc.)
list_of_final_datasets <- lapply(list_of_merged_datasets, function(df) {
  df %>%
    mutate(
      FEMALE = as.factor(if_else(ST004D01T == 1, 1, 0)),
      IMMIG = as.factor(IMMIG)
    ) %>%
    arrange(CNTSCHID)
})
print("Data loading and preparation complete.")


# --- 3. MODEL SPECIFICATION FOR BRMS ---

# The formula is similar to lmer, but with a special syntax for weights.
# SYNTAX: outcome | weights(weight_variable) ~ fixed_effects + random_effects

# We will use the first plausible value, RESILIENCE_SCORE_PV1, for this example.
# The `brm_multiple` function will automatically use the correct column from each
# dataset in the list.
dv <- "RESILIENCE_SCORE_PV1"
weight_var <- "W_FSTUWT"


# Predictors and random effects are the same as before
original_model_2_predictors <- paste(
  c("ESCS", "FEMALE", "IMMIG",
    "REPEAT", "MISSSC", "SKIPPING", "TARDYSD",
    "STUDYHMW", "WORKPAY", "EXPOFA", "EXPO21ST", "STRESAGR",'SDLEFF', 'GROSAGR'),
  collapse = " + "
)

latent_variable_predictors <- c(
  "Math_Disposition",
   "Social_Emotional_Skills",
   "Openness_Creativity",
   "Teacher_Classroom_Exp",
   "School_Experience"
)

fixed_effects_string <- paste(
  original_model_2_predictors, "+", paste(latent_variable_predictors, collapse = " + ")
)
random_effect <- "(1 | CNTSCHID)"

# Create the brms formula object
brms_formula <- bf(
  paste(dv, "| weights(", weight_var, ") ~", fixed_effects_string, "+", random_effect)
)

# Specify weakly informative priors (good practice)
# This helps regularize the model and prevent extreme results.
# N(0,1) for coefficients, Student-t for variance components.
priors <- c(
  prior(normal(0, 1), class = "b"), # Prior for all regression coefficients
  prior(student_t(3, 0, 2.5), class = "sd"), # Prior for standard deviation of random effects
  prior(student_t(3, 0, 2.5), class = "sigma") # Prior for the residual standard deviation
)


# --- 4. RUN THE BAYESIAN MODEL ---
# We use brm_multiple() to run the model on the list of imputed datasets.
# This will take a significant amount of time.
print("Starting Bayesian model fitting with brms. THIS WILL TAKE A LONG TIME...")
print(paste("Start time:", Sys.time()))

# For a real analysis, you might increase iterations (e.g., iter = 4000).
# For this first run, 2000 is a reasonable start.
brms_model_pv1 <- brm_multiple(
  formula = brms_formula,
  data = list_of_final_datasets,
  family = gaussian(),
  prior = priors,
  chains = 4,          # Number of simulation chains
  iter = 8000,         # Total iterations per chain
  warmup = 4000,       # Burn-in iterations to discard
  seed = 1234,         # For reproducibility
  refresh = 100
)

print(paste("End time:", Sys.time()))


# --- 5. SAVE AND EXAMINE RESULTS ---
# ALWAYS save your model object after it finishes running!
print("Saving model object...")
saveRDS(brms_model_pv1, file = here("../dataset", "analysis", "brms_model_pv1.rds"))
print("Model object saved.")

# Print the summary of the model
# LOOK FOR:
# 1. Rhat values should all be 1.00. This indicates convergence.
# 2. Bulk_ESS and Tail_ESS should be high (e.g., > 400). This indicates effective simulation.
# 3. The 'Estimate' and 'l-95% CI' to 'u-95% CI' are your results. They are now stable and trustworthy.
print("==================================================================")
print("             BAYESIAN MODEL SUMMARY (brms) - PV1                  ")
print("==================================================================")
print(summary(brms_model_pv1))

# Check the Bayesian R-squared
print("==================================================================")
print("                BAYESIAN R-SQUARED                                ")
print("==================================================================")
# print(bayes_R2(brms_model_pv1))