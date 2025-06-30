import pandas as pd
import numpy as np
from scipy import stats
import statsmodels.api as sm
import statsmodels.formula.api as smf
import os

# --- Configuration Section ---
input_file = '../../dataset/analysis/imputed_standardized_final.csv'
outcome_variable = 'ACADEMIC_RESILIENCE'
school_column = 'CNTSCHID'
imputation_id_column = 'imputation_num'

# --- Main Processing Logic ---
def run_model_a_expanded():
    """
    Runs Model A (controls only) on the full, expanded dataset of all
    'good' countries and pools the results.
    """
    print(f"Loading imputed data from: {input_file}")
    try:
        long_format_df = pd.read_csv(input_file, low_memory=False)
    except FileNotFoundError:
        print(f"--- ERROR --- \nFile not found at {input_file}. Please check the path.")
        return

    # --- ** NOTE: The filter for specific countries has been removed. ** ---
    # We are now using all the countries in the file.
    print(f"Processing all {long_format_df['CNT'].nunique()} countries in the dataset.")
    num_imputations = int(long_format_df[imputation_id_column].max())
    print(f"Found {num_imputations} imputed datasets.\n")

    # --- Define Model A Formula ---
    model_formula = (f"{outcome_variable} ~ AGE + C(ST004D01T) + C(ISCEDP) + "
                     f"C(IMMIG) + C(CNT)") # C(CNT) now controls for all 38 countries

    print("--- Running Model A on Expanded Sample ---")
    print(f"Model Formula: {model_formula}")

    params_list = []
    bse_list = []

    for i in range(1, num_imputations + 1):
        print(f"Fitting model on imputation #{int(i)}...")
        data_for_model = long_format_df[long_format_df[imputation_id_column] == i]

        model = smf.mixedlm(
            formula=model_formula,
            data=data_for_model,
            groups=data_for_model[school_column]
        ).fit()

        params_list.append(model.params)
        bse_list.append(model.bse)

    print("\nAll models fitted. Now pooling results manually...")

    # --- Manually Pool the results using Rubin's Rules ---
    # This section is unchanged and correct.
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

    print("\n--- Final Pooled Results for Model A (Expanded Sample) ---")
    summary_df.rename(index={'Group Var': 'School Var (Random Effect)'}, inplace=True)
    print(summary_df.round(4))
    print("----------------------------------------------------------\n")

if __name__ == '__main__':
    run_model_a_expanded()