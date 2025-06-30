import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
import os

# --- Configuration Section ---

# 1. Path to the imputed and standardized dataset you just created.
input_file = '../dataset/analysis/imputed_standardized_final.csv'

# 2. Define the names of your key columns.
outcome_variable = 'ACADEMIC_RESILIENCE'
school_column = 'CNTSCHID'
country_column = 'CNT'
imputation_id_column = 'imputation_num'


# --- Main Processing Logic ---

def calculate_null_model_icc():
    """
    Loads multiply imputed data, fits null models for school and country levels
    on each imputed dataset, and pools the resulting ICCs.
    """
    print(f"Loading imputed data from: {input_file}")
    try:
        long_format_df = pd.read_csv(input_file, low_memory=False)
    except FileNotFoundError:
        print(f"--- ERROR --- \nFile not found at {input_file}. Please check the path.")
        return

    # Get the number of imputed datasets
    num_imputations = long_format_df[imputation_id_column].max()
    print(f"Found {int(num_imputations)} imputed datasets.")

    icc_school_list = []
    icc_country_list = []

    print("\nProcessing each imputed dataset...")
    # Group by the imputation number and loop through each dataset
    for i, data_subset in long_format_df.groupby(imputation_id_column):
        print(f"--- Analyzing Imputation #{int(i)} ---")

        # --- a) Calculate School-Level ICC ---
        # We fit a two-level linear mixed model with a random intercept for school.
        # This partitions variance between students and schools.
        # Note: We use a linear model here as it's the standard, simplest way to get a clear ICC.

        # Define the model formula
        school_model_formula = f"{outcome_variable} ~ 1"

        # Fit the model
        school_model = smf.mixedlm(
            formula=school_model_formula,
            data=data_subset,
            groups=data_subset[school_column]
        ).fit()

        # Extract variance components to calculate ICC
        # ICC = Var(school) / (Var(school) + Var(residual))
        var_school = float(school_model.cov_re.iloc[0,0])
        var_residual = school_model.scale
        icc_school = var_school / (var_school + var_residual)
        icc_school_list.append(icc_school)
        print(f"  School-Level ICC: {icc_school:.4f}")

        # --- b) Calculate Country-Level ICC ---
        # We fit a two-level linear mixed model with a random intercept for country.
        # This partitions variance between students and countries.
        country_model_formula = f"{outcome_variable} ~ 1"
        country_model = smf.mixedlm(
            formula=country_model_formula,
            data=data_subset,
            groups=data_subset[country_column]
        ).fit()

        # Extract variance components
        # ICC = Var(country) / (Var(country) + Var(residual))
        var_country = float(country_model.cov_re.iloc[0,0])
        var_residual_country = country_model.scale
        icc_country = var_country / (var_country + var_residual_country)
        icc_country_list.append(icc_country)
        print(f"  Country-Level ICC: {icc_country:.4f}")

    # --- Pool the results by averaging ---
    final_icc_school = sum(icc_school_list) / len(icc_school_list)
    final_icc_country = sum(icc_country_list) / len(icc_country_list)

    print("\n--- Final Pooled Results ---")
    print(f"Average School-Level ICC: {final_icc_school:.4f}")
    print(f"Average Country-Level ICC: {final_icc_country:.4f}")
    print("----------------------------\n")

    # --- Interpretation ---
    interpret_icc(final_icc_school, "school")
    interpret_icc(final_icc_country, "country")


def interpret_icc(icc, level):
    """Provides an interpretation of the ICC value."""
    percentage = icc * 100
    print(f"Interpretation for {level.capitalize()} Level (ICC = {icc:.4f}):")
    print(f"  - Approximately {percentage:.2f}% of the total variance in academic resilience can be attributed to differences between {level}s.")
    if icc > 0.05:
        print("  - Since the ICC is greater than 0.05 (a common threshold), a multilevel model is strongly justified.")
    else:
        print("  - The ICC is low, suggesting minimal clustering at this level, but a multilevel model may still be appropriate given the data's structure.")


if __name__ == '__main__':
    calculate_null_model_icc()