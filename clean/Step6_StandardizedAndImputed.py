import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.imputation import mice
from sklearn.preprocessing import StandardScaler
import os

# --- Configuration Section ---
input_file = '../dataset/clean/STU_includeVariablesSet.csv'
output_file = '../dataset/analysis/imputed_standardized_good_countries.csv'

# Define key column names
country_column = 'CNT'
school_column = 'CNTSCHID'
student_id_column = 'CNTSTUID'

# Define thresholds
MISSING_VALUE_THRESHOLD = 50.0
PROPORTION_FOR_GOOD_DATA = 0.10
NUMBER_OF_IMPUTATIONS = 5

# --- Main Processing Logic ---
def final_imputation_script():
    """
    This definitive script prepares the data by transforming and dropping
    variables, checks for data type errors, and then runs the imputation.
    """
    print(f"Loading data from: {input_file}")
    try:
        df = pd.read_csv(input_file, low_memory=False)
    except FileNotFoundError:
        print(f"--- ERROR --- \nFile not found at {input_file}. Please check the path.")
        return

    # --- Step 1: Feature Engineering and Selective Variable Dropping ---
    print("\nStep 1: Transforming OCOD3 and dropping unnecessary variables...")

    # a) Transform OCOD3 into a usable categorical variable
    # This checks for PISA's special codes (e.g., 9998) and turns them into NaN
    df['OCOD3_cleaned'] = df['OCOD3'].apply(lambda x: np.nan if x in [9997, 9998, 9999] else x)
    # Convert to string to safely extract the first digit
    df['OCOD3_str'] = df['OCOD3_cleaned'].astype(str)
    # Create the new column with the first digit
    df['OCOD3_major_group'] = df['OCOD3_str'].str[0]

    # ** THE FIX FOR THE LAST TypeError **
    # Convert the new 'OCOD3_major_group' column back to a numeric type.
    # `errors='coerce'` will handle any stray text by turning it into a missing value (NaN).
    df['OCOD3_major_group'] = pd.to_numeric(df['OCOD3_major_group'], errors='coerce')

    # b) Define variables to drop permanently from the dataset
    VARS_TO_DROP = [
        'OCOD3',         # Original is replaced by OCOD3_major_group
        'OCOD3_cleaned', # Intermediate helper column
        'OCOD3_str',     # Intermediate helper column
        'LANGN'          # Replaced by IMMIG as a simpler proxy
    ]
    df.drop(columns=VARS_TO_DROP, inplace=True, errors='ignore')
    print("Transformation complete. 'OCOD3_major_group' created and old variables dropped.")

    # --- Step 2: Select Subset of 'Good' Countries ---
    print("\nStep 2: Identifying and selecting 'good' countries...")
    is_highly_missing = df.groupby(country_column).apply(
        lambda x: x.isnull().sum() / len(x) * 100
    ) >= MISSING_VALUE_THRESHOLD
    prop_missing_in_country = is_highly_missing.mean(axis=1)
    good_countries = prop_missing_in_country[prop_missing_in_country < PROPORTION_FOR_GOOD_DATA].index.tolist()
    if not good_countries:
        print("--- ERROR --- No countries met the criteria for 'good' data. Cannot proceed.")
        return
    df_pilot = df[df[country_column].isin(good_countries)].copy()
    print(f"Selected {len(good_countries)} countries for pilot analysis.")

    # --- Step 3: Prepare for Imputation ---
    print("\nStep 3: Preparing data and adding school-level variables...")
    CONTINUOUS_VARS = [
        "AGE", "BSMJ", "GROSAGR", "ANXMAT", "MATHEFF", "MATHEF21", "MATHPERS",
        "ASSERAGR", "COOPAGR", "CURIOAGR", "EMOCOAGR", "EMPATAGR", "PERSEVAGR",
        "STRESAGR", "CREATEFF", "CREATOP", "IMAGINE", "OPENART", "SDLEFF",
        "INFOSEEK", "EXPOFA", "EXPO21ST", "CREATAS", "CREATOOS", "TEACHSUP",
        "RELATST", "COGACRCO", "COGACMCO", "DISCLIM", "CREATSCH", "FAMSUP",
        "CREATFAM", "FAMSUPSL", "FEELLAH", "PROBSELF", "LEARRES", "BULLIED",
        "FEELSAFE", "SCHRISK", "BELONG", "SCHSUST"
    ]
    for var in CONTINUOUS_VARS:
        if var in df_pilot.columns:
            try:
                school_mean = df_pilot.groupby(school_column)[var].transform('mean')
                df_pilot[f'{var}_sch_mean'] = school_mean
            except TypeError:
                print(f"  - Warning: Could not create school mean for '{var}' as it may not be numeric.")
                continue
    print("School-level mean variables created.")

    # --- Step 4: Separate Identifiers and Run Imputation ---
    print(f"\nStep 4: Performing MICE to generate {NUMBER_OF_IMPUTATIONS} complete datasets...")
    # This list now correctly includes all identifiers to preserve them.
    EXCLUDE_FROM_IMPUTATION = [
        country_column, school_column, student_id_column, 'ACADEMIC_RESILIENCE', 'W_FSTUWT'
    ]
    cols_to_exclude = [col for col in EXCLUDE_FROM_IMPUTATION if col in df_pilot.columns]
    df_for_imputation = df_pilot.drop(columns=cols_to_exclude)
    df_ids = df_pilot[cols_to_exclude]

    # ** Safety Check for unexpected text columns **
    object_cols = df_for_imputation.select_dtypes(include=['object']).columns
    if not object_cols.empty:
        print("\n--- WARNING: Found unexpected text columns before imputation ---")
        print(f"The following columns will be converted to numeric: {list(object_cols)}")
        for col in object_cols:
            df_for_imputation[col] = pd.to_numeric(df_for_imputation[col], errors='coerce')

    # Continue with MICE
    mice_imputer = mice.MICEData(df_for_imputation)
    imputed_datasets = []
    for i in range(NUMBER_OF_IMPUTATIONS):
        print(f"  - Generating imputed dataset {i+1}/{NUMBER_OF_IMPUTATIONS}...")
        mice_imputer.update_all()
        imputed_subset = mice_imputer.data.copy()
        full_imputed_df = pd.concat(
            [df_ids.reset_index(drop=True), imputed_subset.reset_index(drop=True)], axis=1
        )
        imputed_datasets.append(full_imputed_df)
    print("Multiple imputation complete.")

    # --- Step 5: Standardize Each Imputed Dataset ---
    print("\nStep 5: Standardizing continuous variables within each imputed dataset...")
    standardized_datasets = []
    for i, imputed_df in enumerate(imputed_datasets):
        scaler = StandardScaler()
        vars_to_scale = [v for v in CONTINUOUS_VARS if v in imputed_df.columns]
        imputed_df[vars_to_scale] = scaler.fit_transform(imputed_df[vars_to_scale])
        standardized_datasets.append(imputed_df)

    # --- Step 6: Save Results in Long Format ---
    print("\nStep 6: Combining datasets into long format and saving to file...")
    for i, final_df in enumerate(standardized_datasets):
        final_df['imputation_num'] = i + 1
    long_format_df = pd.concat(standardized_datasets, axis=0)

    # Reorder columns to be tidy
    cols_to_order = ['imputation_num', student_id_column, country_column, school_column]
    remaining_cols = [c for c in long_format_df.columns if c not in cols_to_order]
    final_cols = cols_to_order + remaining_cols
    long_format_df = long_format_df[final_cols]

    # Save the file
    output_dir = os.path.dirname(output_file)
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    long_format_df.to_csv(output_file, index=False)
    print(f"\nProcess complete. Final imputed and standardized data saved to:\n{output_file}")


if __name__ == '__main__':
    final_imputation_script()