import pandas as pd
import numpy as np
from scipy import stats
import statsmodels.api as sm
import statsmodels.formula.api as smf
import os

# --- Configuration Section ---
imputed_file = '../../dataset/analysis/imputed_standardized_final.csv'
pca_file = '../../dataset/analysis/pca_components_EXPANDED.csv'
outcome_variable = 'ACADEMIC_RESILIENCE'
school_column = 'CNTSCHID'
imputation_id_column = 'imputation_num'
student_id_column = 'CNTSTUID'

# --- Main Processing Logic ---
def run_model_b_expanded_pooled():
    """
    Runs Model B (Controls + Psych PCA) on the full expanded dataset
    and pools the results from all 5 imputations.
    """
    print(f"Loading data from: {imputed_file} and {pca_file}")
    try:
        long_format_df = pd.read_csv(imputed_file, low_memory=False)
        pca_df = pd.read_csv(pca_file)
    except FileNotFoundError as e:
        print(f"--- ERROR --- \nFile not found. Please check path: {e.filename}")
        return

    print(f"Processing all {long_format_df['CNT'].nunique()} countries in the dataset.")
    num_imputations = int(long_format_df[imputation_id_column].max())
    print(f"Found {num_imputations} imputed datasets.\n")

    # --- Define the full Model B Formula ---
    controls_part = "AGE + C(ST004D01T) + C(ISCEDP) + C(IMMIG) + C(CNT) + BSMJ + C(EXPECEDU) + C(SISCO) + C(OCOD3_major_group)"
    # Get PCA predictors directly from the pca_df columns
    pca_predictors = [col for col in pca_df.columns if '_PC' in col]
    pca_part = " + ".join(pca_predictors)
    model_formula = f"{outcome_variable} ~ {controls_part} + {pca_part}"

    print("--- Running Model B on Expanded Sample ---")
    print(f"Model Formula: {model_formula}")

    params_list = []
    bse_list = []

    for i in range(1, num_imputations + 1):
        print(f"Fitting model on imputation #{int(i)}...")

        # Get the imputed data for this loop
        imputed_subset = long_format_df[long_format_df[imputation_id_column] == i]

        # Merge this imputation with the (averaged) PCA components
        # We only need the student ID and PC columns from the pca_df
        cols_to_merge = [student_id_column] + pca_predictors
        data_for_model = pd.merge(imputed_subset, pca_df[cols_to_merge], on=student_id_column)

        model = smf.mixedlm(
            formula=model_formula,
            data=data_for_model,
            groups=data_for_model[school_column]
        ).fit()

        params_list.append(model.params)
        bse_list.append(model.bse)

    print("\nAll models fitted. Now pooling results manually...")

    # --- Manually Pool the results using Rubin's Rules ---
    m = len(params_list)
    all_params = pd.concat(params_list, axis=1)
    all_vars = pd.concat(bse_list, axis=1)**2
    Q_bar = all_params.mean(axis=1)
    U_bar = all_vars.mean(axis=1)
    B = all_params.var(axis=1, ddof=1)
    T = U_bar + (1 + 1/m) * B
    pooled_se = np.sqrt(T)
    z_scores = Q_bar / pooled_se
    p_values = 2 * (1 - stats.norm.cdf(np.abs(z_scores)))
    conf_int_lower = Q_bar - 1.96 * pooled_se
    conf_int_upper = Q_bar + 1.96 * pooled_se

    summary_df = pd.DataFrame({
        "coef": Q_bar, "std err": pooled_se, "z": z_scores,
        "P>|z|": p_values, "[0.025": conf_int_lower, "0.975]": conf_int_upper
    })

    print("\n--- Final Pooled Results for Model B (Expanded Sample) ---")
    summary_df.rename(index={'Group Var': 'School Var (Random Effect)'}, inplace=True)
    print(summary_df.round(4))
    print("----------------------------------------------------------\n")

if __name__ == '__main__':
    run_model_b_expanded_pooled()