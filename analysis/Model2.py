import pandas as pd
import numpy as np # <-- Added numpy for calculations
from scipy import stats # <-- Added scipy for p-values
import statsmodels.api as sm
import statsmodels.formula.api as smf
import os

# --- Configuration Section ---

input_file = '../dataset/analysis/imputed_standardized_final.csv'
outcome_variable = 'ACADEMIC_RESILIENCE'
school_column = 'CNTSCHID'
imputation_id_column = 'imputation_num'

# --- Define the specific countries for this pilot analysis ---
COUNTRIES_TO_INCLUDE = ['TUR', 'HKG']


# --- Main Processing Logic ---

def run_subset_model_a_manual_pooling():
    """
    Filters for specific countries, runs Model A on each dataset, and then
    manually pools the results using Rubin's Rules. This version avoids
    versioning issues with statsmodels.
    """
    print(f"Loading imputed data from: {input_file}")
    try:
        long_format_df = pd.read_csv(input_file, low_memory=False)
    except FileNotFoundError:
        print(f"--- ERROR --- \nFile not found at {input_file}. Please check the path.")
        return

    # --- Filter the dataframe to include only the specified countries ---
    print(f"\nFiltering data for the selected regions: {', '.join(COUNTRIES_TO_INCLUDE)}")
    df_subset = long_format_df[long_format_df['CNT'].isin(COUNTRIES_TO_INCLUDE)].copy()

    if df_subset.empty:
        print("--- ERROR --- No data found for the specified countries. Please check the country codes.")
        return

    print(f"Subset created. New shape: {df_subset.shape}")

    num_imputations = int(df_subset[imputation_id_column].max())
    print(f"Found {num_imputations} imputed datasets in the subset.\n")

    # --- Define Model A Formula ---
    model_formula = (f"{outcome_variable} ~ AGE + C(ST004D01T) + C(ISCEDP) + "
                     f"C(IMMIG) + C(CNT)")

    print("--- Running Model A on the Subset ---")
    print(f"Model Formula: {model_formula}")

    params_list = []
    bse_list = []

    for i in range(1, num_imputations + 1):
        print(f"Fitting model on imputation #{int(i)}...")
        data_for_model = df_subset[df_subset[imputation_id_column] == i]

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

    # Concatenate results into dataframes for easier calculation
    all_params = pd.concat(params_list, axis=1)
    all_vars = pd.concat(bse_list, axis=1)**2 # Variances are SE squared

    # Pooled coefficients are the mean of the coefficients
    Q_bar = all_params.mean(axis=1)

    # Within-imputation variance is the mean of the variances
    U_bar = all_vars.mean(axis=1)

    # Between-imputation variance
    B = all_params.var(axis=1, ddof=1)

    # Total variance
    T = U_bar + (1 + 1/m) * B

    # Pooled standard error is the square root of total variance
    pooled_se = np.sqrt(T)

    # Calculate z-scores and p-values
    z_scores = Q_bar / pooled_se
    p_values = 2 * (1 - stats.norm.cdf(np.abs(z_scores)))

    # Get confidence intervals
    conf_int_lower = Q_bar - 1.96 * pooled_se
    conf_int_upper = Q_bar + 1.96 * pooled_se

    # Assemble the final results table
    summary_df = pd.DataFrame({
        "coef": Q_bar,
        "std err": pooled_se,
        "z": z_scores,
        "P>|z|": p_values,
        "[0.025": conf_int_lower,
        "0.975]": conf_int_upper
    })

    print("\n--- Final Pooled Results for Model A (TUR & HKG only) ---")
    # The last row in the summary is the variance of the random effect, which we can label manually
    summary_df.rename(index={'Group Var': 'School Var (Random Effect)'}, inplace=True)
    print(summary_df.round(4))
    print("----------------------------------------------------------\n")

if __name__ == '__main__':
    run_subset_model_a_manual_pooling()