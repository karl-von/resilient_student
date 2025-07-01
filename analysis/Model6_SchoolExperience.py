import pandas as pd
import statsmodels.formula.api as smf
import os

# --- Configuration Section ---
imputed_file = '../dataset/analysis/imputed_standardized_final.csv'
psych_pca_file = '../dataset/analysis/pca_components_TUR_HKG.csv'
env_pca_file = '../dataset/analysis/environmental_pca_components.csv'

student_id_column = 'CNTSTUID'
school_id_column = 'CNTSCHID'

# Define all blocks of variables
BACKGROUND_CONTROLS = [
    'AGE', 'ST004D01T', 'ISCEDP', 'IMMIG', 'BSMJ', 'EXPECEDU',
    'SISCO', 'OCOD3_major_group'
]

# --- ADDED: The list of practice variables from Model 4 ---
PRACTICE_VARIABLES = [
    'REPEAT', 'MISSSC', 'SKIPPING', 'TARDYSD', 'EXERPRAC', 'STUDYHMW',
    'WORKPAY', 'WORKHOME', 'INFOSEEK', 'EXPOFA', 'EXPO21ST', 'CREATAS', 'CREATOOS'
]

# --- Main Processing Logic ---
def run_ultimate_full_model():
    """
    Loads and merges ALL data blocks and runs the final, all-encompassing
    model.
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

    # --- MODIFIED: Ensure cols_to_keep includes all variable blocks ---
    cols_to_keep = (
            ['CNT', school_id_column, student_id_column, 'ACADEMIC_RESILIENCE'] +
            BACKGROUND_CONTROLS +
            PRACTICE_VARIABLES
    )
    final_df = df_base[cols_to_keep]

    # Merge PCA components
    psych_cols_to_merge = [student_id_column] + [col for col in psych_pca_df.columns if '_PC' in col]
    final_df = pd.merge(final_df, psych_pca_df[psych_cols_to_merge], on=student_id_column)

    env_cols_to_merge = [student_id_column] + [col for col in env_pca_df.columns if '_PC' in col]
    final_df = pd.merge(final_df, env_pca_df[env_cols_to_merge], on=student_id_column)

    print("Successfully merged all predictor blocks.")

    # --- Step 2: Define and Run the Ultimate Model ---
    # Define each part of the formula
    controls_part = "AGE + C(ST004D01T) + C(ISCEDP) + C(IMMIG) + C(CNT) + BSMJ + C(EXPECEDU) + C(SISCO) + C(OCOD3_major_group)"
    psych_pca_part = " + ".join([col for col in final_df.columns if 'Disposition_PC' in col or 'Social_Emotional_Skills_PC' in col or 'Openness_Creativity_PC' in col or 'Self_Directed_Learning_PC' in col])
    env_pca_part = " + ".join([col for col in final_df.columns if 'Teacher_Classroom_Exp_PC' in col or 'Home_Learning_Env_PC' in col or 'Remote_Learning_Exp_PC' in col])
    school_exp_part = " + ".join([col for col in final_df.columns if 'School_Experience_PC' in col])

    # --- ADDED: Define the practice variables part of the formula ---
    practice_part = "C(REPEAT) + " + " + ".join(PRACTICE_VARIABLES[1:])

    # Combine all five parts for the final formula
    model_formula = f"ACADEMIC_RESILIENCE ~ {controls_part} + {psych_pca_part} + {practice_part} + {env_pca_part} + {school_exp_part}"

    print("\n--- Running Ultimate Full Model (with all variables) ---")
    print("Model Formula:")
    print(model_formula)

    model = smf.mixedlm(
        formula=model_formula,
        data=final_df,
        groups=final_df[school_id_column]
    ).fit(reml=False)

    # --- Step 3: View the Results ---
    print("\n--- Final Model Results (Ultimate Full Model) ---")
    print(model.summary())
    print(f"AIC: {model.aic}")
    print(f"BIC: {model.bic}")
    print("---------------------------------------------------\n")


if __name__ == '__main__':
    run_ultimate_full_model()