import pandas as pd
import statsmodels.formula.api as smf
import os

pd.set_option('display.max_rows', None)
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
def run_final_model_corrected():
    """
    Merges background controls with PCA components and runs the final
    multilevel model with the corrected formula.
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

    # Take just the FIRST imputed dataset as our base for control variables
    df_base = imputed_df[imputed_df['imputation_num'] == 1].copy()

    # Select identifiers and the background controls
    cols_to_keep = ['CNT', 'CNTSCHID', student_id_column] + BACKGROUND_CONTROLS
    df_controls_with_ids = df_base[cols_to_keep]

    # Merge the controls with the PCA components using the unique student ID
    final_df = pd.merge(
        df_controls_with_ids, pca_df, on=student_id_column, suffixes=('_x', None)
    )
    final_df.drop(columns=['CNT_x', 'CNTSCHID_x'], inplace=True)

    print("\nSuccessfully merged background controls with PCA components.")
    print(f"Final dataset for modeling has {final_df.shape[0]} rows and {final_df.shape[1]} columns.")

    # --- Step 2: Define and Run the PCA-Based Model (CORRECTED SECTION) ---

    # Get the names of the PCA components automatically
    pca_predictors = [col for col in final_df.columns if '_PC' in col]

    # Define the part of the formula with the control variables
    # We wrap categorical variables in C()
    controls_formula_part = (
        "AGE + C(ST004D01T) + C(ISCEDP) + C(IMMIG) + C(CNT) + "
        "BSMJ + C(EXPECEDU) + C(SISCO) + C(OCOD3_major_group)"
    )

    # Join the PCA predictors into a single string, separated by ' + '
    pca_formula_part = " + ".join(pca_predictors)

    # Combine all parts to create the final, clean formula string
    model_formula = (
        f"ACADEMIC_RESILIENCE ~ {controls_formula_part} + {pca_formula_part}"
    )

    print("\n--- Running Final, PCA-Based Multilevel Model ---")
    print("Model Formula:")
    print(model_formula)

    # Fit the final two-level model
    model = smf.mixedlm(
        formula=model_formula,
        data=final_df,
        groups=final_df['CNTSCHID']
    ).fit(reml=False)

    # --- Step 3: View the Results ---
    print("\n--- Final Model Results ---")
    print(model.summary())
    print("---------------------------\n")
    print(f"AIC: {model.aic}")
    print(f"BIC: {model.bic}")

if __name__ == '__main__':
    run_final_model_corrected()