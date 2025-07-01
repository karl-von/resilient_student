import pandas as pd
import statsmodels.formula.api as smf
import os

# --- Configuration Section ---
imputed_file = '../dataset/analysis/imputed_standardized_final.csv'
psych_pca_file = '../dataset/analysis/pca_components_TUR_HKG.csv'
env_pca_file = '../dataset/analysis/environmental_pca_components.csv'

# Define key column names
student_id_column = 'CNTSTUID'
school_id_column = 'CNTSCHID'

# Define the variable blocks
BACKGROUND_CONTROLS = [
    'AGE', 'ST004D01T', 'ISCEDP', 'IMMIG', 'BSMJ', 'EXPECEDU',
    'SISCO', 'OCOD3_major_group'
]

# --- NEW: Added the list of practice variables from your Model 4 script ---
PRACTICE_VARIABLES = [
    'REPEAT', 'MISSSC', 'SKIPPING', 'TARDYSD', 'EXERPRAC', 'STUDYHMW',
    'WORKPAY', 'WORKHOME', 'INFOSEEK', 'EXPOFA', 'EXPO21ST', 'CREATAS', 'CREATOOS'
]

# --- Main Processing Logic ---
def run_full_final_model():
    """
    Loads and merges ALL data blocks (Controls, Psychology, Practice, Environment)
    and runs the final, most comprehensive model.
    """
    print("--- Step 1: Loading and Preparing All Data Blocks ---")
    try:
        imputed_df = pd.read_csv(imputed_file, low_memory=False)
        psych_pca_df = pd.read_csv(psych_pca_file)
        env_pca_df = pd.read_csv(env_pca_file)
        print("All data files loaded successfully.")
    except FileNotFoundError as e:
        print(f"--- ERROR --- \nFile not found. Please check path: {e.filename}")
        return

    print("\nMerging data blocks using a robust strategy...")

    # 1. Start with the base data from the first imputed dataset.
    df_base = imputed_df[
        (imputed_df['imputation_num'] == 1) &
        (imputed_df['CNT'].isin(['TUR', 'HKG']))
        ].copy()

    # --- NEW: Ensure the columns to keep include the PRACTICE_VARIABLES ---
    cols_to_keep = (
            ['CNT', school_id_column, student_id_column, 'ACADEMIC_RESILIENCE'] +
            BACKGROUND_CONTROLS +
            PRACTICE_VARIABLES
    )
    final_df = df_base[cols_to_keep]

    # 2. Prepare and merge the psychological PCA components.
    psych_pca_cols = [student_id_column] + [col for col in psych_pca_df.columns if '_PC' in col]
    final_df = pd.merge(final_df, psych_pca_df[psych_pca_cols], on=student_id_column)

    # 3. Prepare and merge the environmental PCA components.
    env_pca_cols = [student_id_column] + [col for col in env_pca_df.columns if '_PC' in col]
    final_df = pd.merge(final_df, env_pca_df[env_pca_cols], on=student_id_column)

    print("Successfully merged all predictor blocks.")
    print(f"Final dataset for modeling has {final_df.shape[0]} rows and {final_df.shape[1]} columns.")

    # --- Step 2: Define and Run the Full Model ---
    # Define each part of the formula string
    controls_part = "AGE + C(ST004D01T) + C(ISCEDP) + C(IMMIG) + C(CNT) + BSMJ + C(EXPECEDU) + C(SISCO) + C(OCOD3_major_group)"
    psych_pca_part = " + ".join([col for col in final_df.columns if 'Disposition_PC' in col or 'Social_Emotional_Skills_PC' in col or 'Openness_Creativity_PC' in col or 'Self_Directed_Learning_PC' in col])
    env_pca_part = " + ".join([col for col in final_df.columns if 'Teacher_Classroom_Exp_PC' in col or 'Home_Learning_Env_PC' in col or 'Remote_Learning_Exp_PC' in col])

    # --- NEW: Define the practice variables part of the formula ---
    practice_part = "C(REPEAT) + " + " + ".join(PRACTICE_VARIABLES[1:]) # Add C() for REPEAT

    # --- NEW: Combine ALL parts into the final formula ---
    model_formula = f"ACADEMIC_RESILIENCE ~ {controls_part} + {psych_pca_part} + {practice_part} + {env_pca_part}"

    print("\n--- Running Final, Comprehensive Model ---")
    model = smf.mixedlm(
        formula=model_formula,
        data=final_df,
        groups=final_df[school_id_column]
    ).fit(reml=False) # Use reml=False for model comparison

    # --- Step 3: View the Results ---
    print("\n--- Final Model Results (Full Model) ---")
    print(model.summary())
    print(f"AIC: {model.aic}")
    print(f"BIC: {model.bic}")
    print("----------------------------------------\n")


if __name__ == '__main__':
    run_full_final_model()