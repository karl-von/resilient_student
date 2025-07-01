import pandas as pd
import statsmodels.formula.api as smf
import os

# --- Configuration Section ---
pca_file = '../dataset/analysis/pca_components_TUR_HKG.csv'
imputed_file = '../dataset/analysis/imputed_standardized_final.csv'
student_id_column = 'CNTSTUID'

# Define the background variables you want to keep as controls
BACKGROUND_CONTROLS = [
    'AGE', 'ST004D01T', 'ISCEDP', 'IMMIG', 'BSMJ', 'EXPECEDU',
    'SISCO', 'OCOD3_major_group'
]

# --- Main Processing Logic ---
def run_model_with_practice_vars_corrected():
    """
    Merges all data blocks and runs the final comprehensive model
    with the corrected formula construction.
    """
    print("--- Step 1: Loading and Preparing Final Analysis Dataset ---")
    try:
        pca_df = pd.read_csv(pca_file)
        print(f"Loaded PCA components from: {pca_file}")

        imputed_df = pd.read_csv(imputed_file, low_memory=False)
        print(f"Loaded imputed data from: {imputed_file}")
    except FileNotFoundError as e:
        print(f"--- ERROR --- \nFile not found. Please check path: {e.filename}")
        return

    # Take the first imputed dataset as our base for control variables
    df_base_controls = imputed_df[imputed_df['imputation_num'] == 1].copy()

    # Also select the 'Practice' variables from the base dataset
    PRACTICE_VARIABLES = [
        'REPEAT', 'MISSSC', 'SKIPPING', 'TARDYSD', 'EXERPRAC', 'STUDYHMW',
        'WORKPAY', 'WORKHOME', 'INFOSEEK', 'EXPOFA', 'EXPO21ST', 'CREATAS', 'CREATOOS'
    ]

    # Select all identifiers, controls, and new practice variables
    cols_to_keep = ['CNT', 'CNTSCHID', student_id_column] + BACKGROUND_CONTROLS + PRACTICE_VARIABLES
    df_full_predictors = df_base_controls[cols_to_keep]

    # Merge the controls and practice variables with the PCA components
    final_df = pd.merge(
        df_full_predictors, pca_df, on=student_id_column, suffixes=('_x', None)
    )
    final_df.drop(columns=['CNT_x', 'CNTSCHID_x'], inplace=True)

    print("\nSuccessfully merged all predictor blocks.")
    print(f"Final dataset for modeling has {final_df.shape[0]} rows and {final_df.shape[1]} columns.")

    # --- Step 2: Define and Run the New Model (CORRECTED FORMULA LOGIC) ---

    # Get the names of the PCA components automatically
    pca_predictors = [col for col in final_df.columns if '_PC' in col]

    # Define each part of the formula separately to ensure correctness
    controls_part = "AGE + C(ST004D01T) + C(ISCEDP) + C(IMMIG) + C(CNT) + BSMJ + C(EXPECEDU) + C(SISCO) + C(OCOD3_major_group)"

    psychological_part = " + ".join(pca_predictors)

    practice_part = "C(REPEAT) + MISSSC + SKIPPING + TARDYSD + EXERPRAC + STUDYHMW + WORKPAY + WORKHOME + INFOSEEK + EXPOFA + EXPO21ST + CREATAS + CREATOOS"

    # Combine all parts into the final, clean formula
    model_formula = (
        f"ACADEMIC_RESILIENCE ~ {controls_part} + {psychological_part} + {practice_part}"
    )

    print("\n--- Running Model with Student Practice Variables ---")
    print("Model Formula:")
    print(model_formula)

    # Fit the multilevel model
    model = smf.mixedlm(
        formula=model_formula,
        data=final_df,
        groups=final_df['CNTSCHID']
    ).fit(reml=False)

    # --- Step 3: View the Results ---
    print("\n--- Final Model Results (with Practice Variables) ---")
    print(model.summary())
    print("------------------------------------------------------\n")
    print(f"AIC: {model.aic}")
    print(f"BIC: {model.bic}")

if __name__ == '__main__':
    run_model_with_practice_vars_corrected()