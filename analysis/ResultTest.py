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
def run_sensitivity_analysis():
    """
    Runs the sequential models in an alternative order to test the
    robustness of the findings and explore mediation effects.
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

    # --- Robustly merge all data into a single dataframe ---
    print("\nMerging data blocks...")
    df_base = imputed_df[
        (imputed_df['imputation_num'] == 1) &
        (imputed_df['CNT'].isin(['TUR', 'HKG']))
        ].copy()
    cols_to_keep = ['CNT', school_id_column, student_id_column, 'ACADEMIC_RESILIENCE'] + BACKGROUND_CONTROLS
    final_df = df_base[cols_to_keep]

    psych_cols = [student_id_column] + [col for col in psych_pca_df.columns if '_PC' in col]
    psych_pca_to_merge = psych_pca_df[psych_cols]

    env_cols = [student_id_column] + [col for col in env_pca_df.columns if '_PC' in col]
    env_pca_to_merge = env_pca_df[env_cols]

    final_df = pd.merge(final_df, psych_pca_to_merge, on=student_id_column)
    final_df = pd.merge(final_df, env_pca_to_merge, on=student_id_column)
    print("Successfully merged all predictor blocks.")

    # --- Step 2: Define and Run Model D-alt (Controls + Environment) ---
    controls_part = "AGE + C(ST004D01T) + C(ISCEDP) + C(IMMIG) + C(CNT) + BSMJ + C(EXPECEDU) + C(SISCO) + C(OCOD3_major_group)"
    env_pca_part = " + ".join([col for col in final_df.columns if 'Teacher_Classroom_Exp_PC' in col or 'Home_Learning_Env_PC' in col or 'Remote_Learning_Exp_PC' in col or 'School_Experience_PC' in col])

    model_d_alt_formula = f"ACADEMIC_RESILIENCE ~ {controls_part} + {env_pca_part}"

    print("\n\n--- Running Model D-alt (Controls + Environmental Factors) ---")
    print("Model Formula:")
    print(model_d_alt_formula)

    model_d_alt = smf.mixedlm(
        formula=model_d_alt_formula,
        data=final_df,
        groups=final_df[school_id_column]
    ).fit()

    print("\n--- Results for Model D-alt ---")
    print(model_d_alt.summary())
    print("---------------------------------\n")

    # --- Step 3: Define and Run Model E-alt (Controls + Environment + Psychology) ---
    psych_pca_part = " + ".join([col for col in final_df.columns if 'Disposition_PC' in col or 'Social_Emotional_Skills_PC' in col or 'Openness_Creativity_PC' in col or 'Self_Directed_Learning_PC' in col])

    model_e_alt_formula = f"ACADEMIC_RESILIENCE ~ {controls_part} + {env_pca_part} + {psych_pca_part}"

    print("\n\n--- Running Model E-alt (Full Model - Adding Psychological Factors Last) ---")
    print("Model Formula:")
    print(model_e_alt_formula)

    model_e_alt = smf.mixedlm(
        formula=model_e_alt_formula,
        data=final_df,
        groups=final_df[school_id_column]
    ).fit()

    print("\n--- Results for Model E-alt ---")
    print(model_e_alt.summary())
    print("--------------------------------\n")


if __name__ == '__main__':
    run_sensitivity_analysis()