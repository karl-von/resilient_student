import pandas as pd
import statsmodels.formula.api as smf
import os

# --- Configuration Section ---
imputed_file = '../dataset/analysis/imputed_standardized_good_countries.csv'
psych_pca_file = '../dataset/analysis/pca_components_TUR_HKG.csv'
env_pca_file = '../dataset/analysis/environmental_pca_components.csv'

# Define key column names
student_id_column = 'CNTSTUID'
school_id_column = 'CNTSCHID'

# Define the background variables you want to keep as controls
BACKGROUND_CONTROLS = [
    'AGE', 'ST004D01T', 'ISCEDP', 'IMMIG', 'BSMJ', 'EXPECEDU',
    'SISCO', 'OCOD3_major_group'
]

# --- Main Processing Logic ---
def run_secondary_sensitivity_analysis_corrected():
    """
    Runs a sensitivity analysis by excluding the dominant 'Math_Disposition'
    predictor, using a corrected and robust data merging strategy.
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

    # --- ** NEW, MORE ROBUST MERGING LOGIC ** ---
    print("\nMerging data blocks...")

    # 1. Start with the controls and identifiers from the first imputed dataset.
    df_base = imputed_df[
        (imputed_df['imputation_num'] == 1) &
        (imputed_df['CNT'].isin(['TUR', 'HKG']))
        ].copy()

    # We explicitly keep the outcome variable here as well.
    cols_to_keep = ['CNT', school_id_column, student_id_column, 'ACADEMIC_RESILIENCE'] + BACKGROUND_CONTROLS
    final_df = df_base[cols_to_keep]

    # 2. Prepare the psychological PCA components for merging.
    psych_cols_to_merge = [student_id_column] + [col for col in psych_pca_df.columns if '_PC' in col]

    # 3. Prepare the environmental PCA components for merging.
    env_cols_to_merge = [student_id_column] + [col for col in env_pca_df.columns if '_PC' in col]

    # 4. Merge the dataframes one by one, only bringing in the necessary columns.
    final_df = pd.merge(final_df, psych_pca_df[psych_cols_to_merge], on=student_id_column, how='left')
    final_df = pd.merge(final_df, env_pca_df[env_cols_to_merge], on=student_id_column, how='left')

    print("Successfully merged all predictor blocks.")

    # --- The rest of the script continues as before ---

    # Define formula parts, EXCLUDING Math Disposition
    controls_part = "AGE + C(ST004D01T) + C(ISCEDP) + C(IMMIG) + C(CNT) + BSMJ + C(EXPECEDU) + C(SISCO) + C(OCOD3_major_group)"
    other_psych_pca_part = " + ".join([col for col in final_df.columns if ('Social_Emotional' in col or 'Openness_Creativity' in col or 'Self_Directed_Learning' in col)])
    env_pca_part = " + ".join([col for col in final_df.columns if 'Teacher_Classroom_Exp_PC' in col or 'Home_Learning_Env_PC' in col or 'Remote_Learning_Exp_PC' in col or 'School_Experience_PC' in col])

    # --- Step 2: Run Model F (Controls + Other Psych Factors) ---
    model_f_formula = f"ACADEMIC_RESILIENCE ~ {controls_part} + {other_psych_pca_part}"
    print("\n\n--- Running Model F (Controls + Other Psychological Factors) ---")
    model_f = smf.mixedlm(formula=model_f_formula, data=final_df, groups=final_df[school_id_column]).fit()
    print("\n--- Results for Model F ---")
    print(model_f.summary())
    print("---------------------------\n")

    # --- Step 3: Run Model G (Controls + Other Psych + Environment) ---
    model_g_formula = f"ACADEMIC_RESILIENCE ~ {controls_part} + {other_psych_pca_part} + {env_pca_part}"
    print("\n\n--- Running Model G (Adding Environmental Factors) ---")
    model_g = smf.mixedlm(formula=model_g_formula, data=final_df, groups=final_df[school_id_column]).fit()
    print("\n--- Results for Model G ---")
    print(model_g.summary())
    print("----------------------------\n")

if __name__ == '__main__':
    run_secondary_sensitivity_analysis_corrected()