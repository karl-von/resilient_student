# =================================================================
# Step 5: Assemble Final Model-Ready Datasets (Corrected Final Version)
# =================================================================

library(dplyr)
library(here)

# --- 1. Load Data ---
INPUT_RESILIENCE_FLAGS <- here("dataset", "clean_R", "Step1_Resilience_Flags_Corrected.rds")
INPUT_ORIGINAL_PREDICTORS <- here("dataset", "clean_R", "Step2_Merged_Cleaned_for_Imputation.rds")
INPUT_ENGINEERED_PREDICTORS <- here("dataset", "clean_R", "Step4_Engineered_Features_Pooled.rds")
OUTPUT_MODEL_DATA_RDS <- here("dataset", "clean_R", "Step5_Final_Model_Ready_List.rds")

print("Loading data from Step 1, 2, and 4...")
resilience_flags <- readRDS(INPUT_RESILIENCE_FLAGS)
original_predictors <- readRDS(INPUT_ORIGINAL_PREDICTORS)
engineered_predictors <- readRDS(INPUT_ENGINEERED_PREDICTORS)
print("All data loaded.")


# --- 2. Create Clean Subsets to Prevent Duplicate Columns ---
print("Creating clean data subsets...")

# Subset 1: Get resilience flags, weights, ESCS, and core IDs
resilience_subset <- resilience_flags %>%
  select(CNTSTUID, CNT, CNTSCHID, W_FSTUWT, starts_with("W_FSTURWT"), ESCS, starts_with("ACADEMIC_RESILIENCE_PV"))

# Subset 2: Get original student-level variables
student_level_subset <- original_predictors %>%
  select(
    CNTSTUID, ST004D01T, AGE, ISCEDP, IMMIG, BSMJ, OCOD3_major_group,
    EXPECEDU, SISCO, REPEAT, MISSSC, SKIPPING, TARDYSD, EXERPRAC, STUDYHMW,
    WORKPAY, WORKHOME, INFOSEEK, EXPOFA, EXPO21ST, CREATAS, CREATOOS,
    BULLIED, FEELSAFE, SCHRISK, BELONG, SCHSUST
  ) %>%
  # THE FIX IS HERE: Use as.vector() to strip all special attributes.
  # This is a more robust way to convert a <haven_labelled> object to a plain number.
  mutate(ST004D01T = as.vector(ST004D01T))

# Subset 3: Clean the engineered features to prevent duplicate columns
engineered_subset <- engineered_predictors %>%
  select(
    CNTSTUID,
    starts_with("Math_Disposition"),
    starts_with("Social_Emotional_Skills"),
    starts_with("Openness_Creativity"),
    starts_with("Self_Directed_Learning"),
    starts_with("Teacher_Classroom_Exp"),
    starts_with("Home_Learning_Env"),
    starts_with("Remote_Learning_Exp"),
    starts_with("School_Experience"),
    ends_with("_sch_mean")
  )
print("Clean subsets created successfully.")


# --- 3. Merge the Clean Subsets into a Master Dataset ---
print("Merging clean data subsets into a master file...")
master_data <- resilience_subset %>%
  left_join(engineered_subset, by = "CNTSTUID") %>%
  left_join(student_level_subset, by = "CNTSTUID") %>%
  mutate(FEMALE = if_else(ST004D01T == 1, 1, 0, missing = 0))
print("Master data file created successfully.")


# --- 4. Create List of 10 Plausible Value Datasets ---
print("Creating list of 10 model-ready datasets...")
model_ready_list <- list()

for (i in 1:10) {
  pv_flag_name <- paste0("ACADEMIC_RESILIENCE_PV", i)

  temp_df <- master_data %>%
    select(
      CNT, CNTSCHID, W_FSTUWT, starts_with("W_FSTURWT"),
      all_of(pv_flag_name),
      ESCS, AGE, FEMALE, IMMIG, ISCEDP, BSMJ, OCOD3_major_group,
      EXPECEDU, SISCO, REPEAT, MISSSC, SKIPPING, TARDYSD, EXERPRAC, STUDYHMW,
      WORKPAY, WORKHOME, INFOSEEK, EXPOFA, EXPO21ST, CREATAS, CREATOOS,
      BULLIED, FEELSAFE, SCHRISK, BELONG, SCHSUST,
      starts_with("Math_Disposition"),
      starts_with("Social_Emotional_Skills"),
      starts_with("Openness_Creativity"),
      starts_with("Self_Directed_Learning"),
      starts_with("Teacher_Classroom_Exp"),
      starts_with("Home_Learning_Env"),
      starts_with("Remote_Learning_Exp"),
      starts_with("School_Experience"),
      ends_with("_sch_mean")
    ) %>%
    rename(RESILIENCE = !!sym(pv_flag_name))

  model_ready_list[[i]] <- temp_df
}
print("List creation complete.")


# --- 5. Save Final List ---
saveRDS(model_ready_list, file = OUTPUT_MODEL_DATA_RDS)
print("Success! Final model-ready list created.")
print(paste("Final list saved to:", OUTPUT_MODEL_DATA_RDS))