import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.imputation import mice
from sklearn.preprocessing import StandardScaler
import os

# --- Configuration Section ---
input_file = '../dataset/clean/STU_includeVariablesSet.csv'
output_file = '../dataset/analysis/imputed_standardized_final.csv'

# Define key column names
country_column = 'CNT'
school_column = 'CNTSCHID'
student_id_column = 'CNTSTUID'

# --- ** NEW, CORRECTED VARIABLE DEFINITIONS ** ---
# 1. Define variables that should ONLY be standardized (complex scales with no natural units)
VARS_TO_STANDARDIZE = [
    "BSMJ", "GROSAGR", "ANXMAT", "MATHEFF", "MATHEF21", "MATHPERS", "ASSERAGR",
    "COOPAGR", "CURIOAGR", "EMOCOAGR", "EMPATAGR", "PERSEVAGR", "STRESAGR",
    "CREATEFF", "CREATOP", "IMAGINE", "OPENART", "SDLEFF", "INFOSEEK",
    "EXPOFA", "EXPO21ST", "CREATAS", "CREATOOS", "TEACHSUP", "RELATST",
    "COGACRCO", "COGACMCO", "DISCLIM", "CREATSCH", "FAMSUP", "CREATFAM",
    "FAMSUPSL", "FEELLAH", "PROBSELF", "LEARRES", "BULLIED", "FEELSAFE",
    "SCHRISK", "BELONG", "SCHSUST"
]
# 2. Define ALL continuous-like variables needed for creating school-level means
ALL_CONTINUOUS_LIKE_VARS = VARS_TO_STANDARDIZE + [
    "AGE", "EXERPRAC", "STUDYHMW", "WORKPAY", "WORKHOME"
]

# --- Main Processing Logic ---
def final_imputation_and_standardization():
    """
    This definitive script prepares data by transforming variables, and then
    imputes all missing data, but standardizes ONLY the complex scales.
    """
    print(f"Loading data from: {input_file}")
    df = pd.read_csv(input_file, low_memory=False)

    # Step 1: Feature Engineering and Selective Variable Dropping
    print("\nStep 1: Transforming OCOD3 and dropping unnecessary variables...")
    df['OCOD3_cleaned'] = df['OCOD3'].apply(lambda x: np.nan if x in [9997, 9998, 9999] else x)
    df['OCOD3_str'] = df['OCOD3_cleaned'].astype(str)
    df['OCOD3_major_group'] = df['OCOD3_str'].str[0]
    df['OCOD3_major_group'] = pd.to_numeric(df['OCOD3_major_group'], errors='coerce')
    VARS_TO_DROP = ['OCOD3', 'OCOD3_cleaned', 'OCOD3_str', 'LANGN']
    df.drop(columns=VARS_TO_DROP, inplace=True, errors='ignore')
    print("Transformation complete.")

    # Step 2: Select Subset of 'Good' Countries
    print("\nStep 2: Identifying and selecting 'good' countries...")
    is_highly_missing = df.groupby(country_column).apply(
        lambda x: x.isnull().sum() / len(x) * 100
    ) >= 50.0
    prop_missing_in_country = is_highly_missing.mean(axis=1)
    good_countries = prop_missing_in_country[prop_missing_in_country < 0.10].index.tolist()
    df_pilot = df[df[country_column].isin(good_countries)].copy()
    print(f"Selected {len(good_countries)} countries for pilot analysis.")

    # Step 3: Prepare for Imputation (Create school-level means)
    print("\nStep 3: Preparing data and adding school-level variables...")
    for var in ALL_CONTINUOUS_LIKE_VARS:
        if var in df_pilot.columns:
            try:
                school_mean = df_pilot.groupby(school_column)[var].transform('mean')
                df_pilot[f'{var}_sch_mean'] = school_mean
            except TypeError:
                print(f"  - Warning: Could not create school mean for '{var}'.")
    print("School-level mean variables created.")

    # Step 4: Separate Identifiers and Run Imputation
    print(f"\nStep 4: Performing MICE...")
    EXCLUDE_FROM_IMPUTATION = [
        country_column, school_column, student_id_column, 'ACADEMIC_RESILIENCE', 'W_FSTUWT'
    ]
    cols_to_exclude = [col for col in EXCLUDE_FROM_IMPUTATION if col in df_pilot.columns]
    df_for_imputation = df_pilot.drop(columns=cols_to_exclude)
    df_ids = df_pilot[cols_to_exclude]

    mice_imputer = mice.MICEData(df_for_imputation)
    imputed_datasets = []
    for i in range(5): # NUMBER_OF_IMPUTATIONS
        print(f"  - Generating imputed dataset {i+1}/5...")
        mice_imputer.update_all()
        imputed_subset = mice_imputer.data.copy()
        full_imputed_df = pd.concat(
            [df_ids.reset_index(drop=True), imputed_subset.reset_index(drop=True)], axis=1
        )
        imputed_datasets.append(full_imputed_df)
    print("Multiple imputation complete.")

    # --- Step 5: **REFINED** Standardization Step ---
    print("\nStep 5: Standardizing ONLY complex scales within each imputed dataset...")
    standardized_datasets = []
    for i, imputed_df in enumerate(imputed_datasets):
        scaler = StandardScaler()
        # ** This now correctly uses the specific list of variables to standardize **
        vars_to_scale = [v for v in VARS_TO_STANDARDIZE if v in imputed_df.columns]
        imputed_df[vars_to_scale] = scaler.fit_transform(imputed_df[vars_to_scale])
        standardized_datasets.append(imputed_df)
    print("Selective standardization complete.")

    # Step 6: Save Results in Long Format
    print("\nStep 6: Saving final data...")
    # ... (This logic is unchanged) ...
    for i, final_df in enumerate(standardized_datasets):
        final_df['imputation_num'] = i + 1
    long_format_df = pd.concat(standardized_datasets, axis=0)
    cols_to_order = ['imputation_num', student_id_column, country_column, school_column]
    remaining_cols = [c for c in long_format_df.columns if c not in cols_to_order]
    final_cols = cols_to_order + remaining_cols
    long_format_df = long_format_df[final_cols]
    output_dir = os.path.dirname(output_file)
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    long_format_df.to_csv(output_file, index=False)
    print(f"\nProcess complete. Final imputed data saved to:\n{output_file}")

if __name__ == '__main__':
    final_imputation_and_standardization()