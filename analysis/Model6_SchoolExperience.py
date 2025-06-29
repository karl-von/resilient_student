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
def run_model_d_final_fix():
    """
    Loads and robustly merges all data blocks using a corrected strategy
    and runs the final Model D.
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
    print("\nMerging data blocks using a robust strategy...")

    # 1. Start with the controls and identifiers from the first imputed dataset.
    #    This will be our clean base.
    df_base = imputed_df[
        (imputed_df['imputation_num'] == 1) &
        (imputed_df['CNT'].isin(['TUR', 'HKG']))
        ].copy()

    cols_to_keep = ['CNT', school_id_column, student_id_column, 'ACADEMIC_RESILIENCE'] + BACKGROUND_CONTROLS
    final_df = df_base[cols_to_keep]

    # 2. Prepare the psychological PCA components for merging.
    #    We only need the student ID (as the key) and the new PC columns.
    psych_pca_cols = [student_id_column] + [col for col in psych_pca_df.columns if '_PC' in col]
    psych_pca_to_merge = psych_pca_df[psych_pca_cols]

    # 3. Prepare the environmental PCA components for merging.
    env_pca_cols = [student_id_column] + [col for col in env_pca_df.columns if '_PC' in col]
    env_pca_to_merge = env_pca_df[env_pca_cols]

    # 4. Merge the dataframes one by one. This prevents column name conflicts.
    final_df = pd.merge(final_df, psych_pca_to_merge, on=student_id_column)
    final_df = pd.merge(final_df, env_pca_to_merge, on=student_id_column)


    print("Successfully merged all predictor blocks.")
    print(f"Final dataset for modeling has {final_df.shape[0]} rows and {final_df.shape[1]} columns.")

    # --- Step 2: Define and Run Model D ---
    controls_part = "AGE + C(ST004D01T) + C(ISCEDP) + C(IMMIG) + C(CNT) + BSMJ + C(EXPECEDU) + C(SISCO) + C(OCOD3_major_group)"
    psych_pca_part = " + ".join([col for col in final_df.columns if 'Disposition_PC' in col or 'Social_Emotional_Skills_PC' in col or 'Openness_Creativity_PC' in col or 'Self_Directed_Learning_PC' in col])
    env_pca_part = " + ".join([col for col in final_df.columns if 'Teacher_Classroom_Exp_PC' in col or 'Home_Learning_Env_PC' in col or 'Remote_Learning_Exp_PC' in col])
    school_exp_part = " + ".join([col for col in final_df.columns if 'School_Experience_PC' in col])

    # Combine ALL parts for the final Model E formula
    model_formula = f"ACADEMIC_RESILIENCE ~ {controls_part} + {psych_pca_part} + {env_pca_part} + {school_exp_part}"


    print("\n--- Running Model D (with Teacher, Home, Remote Variables) ---")
    model = smf.mixedlm(
        formula=model_formula,
        data=final_df,
        groups=final_df[school_id_column] # <-- This will now find the 'CNTSCHID' column successfully
    ).fit()

    # --- Step 3: View the Results ---
    print("\n--- Final Model Results (Model D) ---")
    print(model.summary())
    print("-------------------------------------\n")


if __name__ == '__main__':
    run_model_d_final_fix()