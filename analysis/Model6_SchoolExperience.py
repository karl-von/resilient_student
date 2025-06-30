import pandas as pd
import statsmodels.formula.api as smf
import os

# --- Configuration Section ---
imputed_file = '../dataset/analysis/imputed_standardized_final.csv'
psych_pca_file = '../dataset/analysis/pca_components_TUR_HKG.csv'
env_pca_file = '../dataset/analysis/environmental_pca_components.csv'

student_id_column = 'CNTSTUID'
school_id_column = 'CNTSCHID'

BACKGROUND_CONTROLS = [
    'AGE', 'ST004D01T', 'ISCEDP', 'IMMIG', 'BSMJ', 'EXPECEDU',
    'SISCO', 'OCOD3_major_group'
]

# --- Main Processing Logic ---
def run_final_model_e():
    """
    Loads and merges all data blocks and runs the final, all-encompassing
    Model E.
    """
    print("--- Step 1: Loading and Preparing All Data Blocks ---")
    imputed_df = pd.read_csv(imputed_file, low_memory=False)
    psych_pca_df = pd.read_csv(psych_pca_file)
    env_pca_df = pd.read_csv(env_pca_file)
    print("All data files loaded successfully.")

    print("\nMerging data blocks using a robust strategy...")
    df_base = imputed_df[
        (imputed_df['imputation_num'] == 1) &
        (imputed_df['CNT'].isin(['TUR', 'HKG']))
        ].copy()

    cols_to_keep = ['CNT', school_id_column, student_id_column, 'ACADEMIC_RESILIENCE'] + BACKGROUND_CONTROLS
    final_df = df_base[cols_to_keep]

    psych_cols_to_merge = [student_id_column] + [col for col in psych_pca_df.columns if '_PC' in col]
    final_df = pd.merge(final_df, psych_pca_df[psych_cols_to_merge], on=student_id_column)

    env_cols_to_merge = [student_id_column] + [col for col in env_pca_df.columns if '_PC' in col]
    final_df = pd.merge(final_df, env_pca_df[env_cols_to_merge], on=student_id_column)

    print("Successfully merged all predictor blocks.")

    # --- Step 2: Define and Run Model E ---
    controls_part = "AGE + C(ST004D01T) + C(ISCEDP) + C(IMMIG) + C(CNT) + BSMJ + C(EXPECEDU) + C(SISCO) + C(OCOD3_major_group)"
    psych_pca_part = " + ".join([col for col in final_df.columns if 'Disposition_PC' in col or 'Social_Emotional_Skills_PC' in col or 'Openness_Creativity_PC' in col or 'Self_Directed_Learning_PC' in col])
    env_pca_part = " + ".join([col for col in final_df.columns if 'Teacher_Classroom_Exp_PC' in col or 'Home_Learning_Env_PC' in col or 'Remote_Learning_Exp_PC' in col])

    # ** NEW: Add the final block of predictors **
    school_exp_part = " + ".join([col for col in final_df.columns if 'School_Experience_PC' in col])

    # Combine all four parts for the final formula
    model_formula = f"ACADEMIC_RESILIENCE ~ {controls_part} + {psych_pca_part} + {env_pca_part} + {school_exp_part}"

    print("\n--- Running Final Model E (with School Experience) ---")
    print("Model Formula:")
    print(model_formula)

    model = smf.mixedlm(
        formula=model_formula,
        data=final_df,
        groups=final_df[school_id_column]
    ).fit()

    # --- Step 3: View the Results ---
    print("\n--- Final Model Results (Model E) ---")
    print(model.summary())
    print("-------------------------------------\n")


if __name__ == '__main__':
    run_final_model_e()