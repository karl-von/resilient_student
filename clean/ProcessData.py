import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
import numpy as np
import time

def process_pisa_data(file_path, variables_to_keep, output_file_path):
    """
    Loads, cleans, imputes, and standardizes PISA data on a per-country basis.

    Workflow:
    1. Loads the full dataset.
    2. Filters for the specified list of variables.
    3. Iterates through each country in the dataset.
    4. For each country's data subset:
       a. Replaces PISA-specific missing value codes with NaN.
       b. Imputes missing data (default: median/mode; option for IterativeImputer).
       c. Standardizes continuous predictor variables.
    5. Concatenates the cleaned data from all countries into a single DataFrame.
    6. Saves the final processed data to a new CSV file.

    Args:
        file_path (str): Path to the raw PISA data file (.csv).
        variables_to_keep (list): A list of all variable names to include in the analysis.
        output_file_path (str): Path to save the final cleaned CSV file.
    """
    # --- 1. Load and Filter Data ---
    print("Loading raw PISA data... This might take a moment.")
    try:
        # Load the entire dataset first
        df_raw = pd.read_csv(file_path)
        print(f"Successfully loaded data. Shape: {df_raw.shape}")
    except FileNotFoundError:
        print(f"Error: The file was not found at {file_path}. Please check the path.")
        return

    # Ensure all specified variables exist, warn if not
    existing_vars = [var for var in variables_to_keep if var in df_raw.columns]
    missing_from_df = [var for var in variables_to_keep if var not in df_raw.columns]
    if missing_from_df:
        print("\nWarning: The following specified variables were not found in the dataset and will be ignored:")
        print(missing_from_df)

    # Keep only the subset of columns needed for the analysis
    df = df_raw[existing_vars].copy()
    print(f"Filtered DataFrame to {len(existing_vars)} variables. New shape: {df.shape}\n")


    # --- 2. Define Variable Types and Missing Codes ---
    # This list of continuous variables must be carefully checked against the PISA codebook.
    # It includes composite scores (WLEs) and variables like AGE.
    continuous_vars_template = [
        'AGE', 'ANXMAT', 'MATHEFF', 'MATHEF21', 'MATHPERS', 'ASSERAGR', 'COOPAGR',
        'CURIOAGR', 'EMOCOAGR', 'EMPATAGR', 'PERSEVAGR', 'STRESAGR', 'CREATEFF',
        'CREATOP', 'IMAGINE', 'OPENART', 'SDLEFF', 'EXPOFA', 'EXPO21ST',
        'TEACHSUP', 'RELATST', 'COGACRCO', 'COGACMCO', 'DISCLIM', 'CREATSCH',
        'FAMSUP', 'CREATFAM', 'FAMSUPSL', 'FEELLAH', 'PROBSELF', 'SCHRISK',
        'BELONG', 'INFOSEEK', 'CREATAS', 'CREATOOS', 'LEARRES', 'BULLIED',
        'FEELSAFE', 'SCHSUST'
    ]

    # Comprehensive list of codes that represent missing data
    missing_values_codes = [
        5, 7, 8, 9, 95, 97, 98, 99, 9995, 9997, 9998, 9999,
        5.0, 7.0, 8.0, 9.0, 95.0, 97.0, 98.0, 99.0, 9995.0, 9997.0, 9998.0, 9999.0
    ]

    # --- 3. Process Data Country by Country ---
    cleaned_countries_list = []
    countries = df['CNT'].unique()

    print(f"Starting to process data for {len(countries)} countries.")

    for i, country_code in enumerate(countries):
        start_time = time.time()
        print(f"Processing ({i+1}/{len(countries)}): {country_code}")

        country_df = df[df['CNT'] == country_code].copy()

        # Step 4a: Replace missing codes with NaN
        country_df.replace(missing_values_codes, np.nan, inplace=True)

        # Identify continuous vars that actually exist in the data
        continuous_vars = [v for v in continuous_vars_template if v in country_df.columns]

        # Step 4b: Impute missing data
        # --- OPTION 1: Simple Median/Mode Imputation (Default, Fast) ---
        for col in country_df.columns:
            if col in continuous_vars:
                median_val = country_df[col].median()
                country_df[col].fillna(median_val, inplace=True)
            else: # Categorical variables (including CNT, SCHOOLID, etc.)
                # Check if there are any non-missing values before taking mode
                if country_df[col].notna().any():
                    mode_val = country_df[col].mode()[0]
                    country_df[col].fillna(mode_val, inplace=True)
                else: # If a column is entirely empty for a country, fill with a placeholder
                    country_df[col].fillna('NO_DATA', inplace=True)

        # --- OPTION 2: Multiple Imputation (More accurate, much slower) ---
        # To use this, comment out OPTION 1 above and uncomment this block.
        # imputer = IterativeImputer(max_iter=5, random_state=0)
        # # Important: Impute on a copy to avoid SettingWithCopyWarning
        # country_df_imputed_array = imputer.fit_transform(country_df)
        # country_df = pd.DataFrame(country_df_imputed_array, columns=country_df.columns, index=country_df.index)
        # print(f"  > Multiple Imputation complete for {country_code}")

        # Step 4c: Standardize continuous variables
        if continuous_vars:
            scaler = StandardScaler()
            country_df[continuous_vars] = scaler.fit_transform(country_df[continuous_vars])

        cleaned_countries_list.append(country_df)
        end_time = time.time()
        print(f"  > Finished {country_code} in {end_time - start_time:.2f} seconds.")

    # --- 5. Concatenate and Save ---
    print("\nCombining cleaned data from all countries...")
    final_cleaned_df = pd.concat(cleaned_countries_list, ignore_index=True)

    print(f"Final shape of cleaned data: {final_cleaned_df.shape}")
    print(f"Total missing values in final df: {final_cleaned_df.isnull().sum().sum()} (should be 0)")

    try:
        final_cleaned_df.to_csv(output_file_path, index=False)
        print(f"\n✅ Success! Cleaned data saved to: {output_file_path}")
    except Exception as e:
        print(f"\nError saving file: {e}")


# --- Main execution block ---
if __name__ == '__main__':
    # --- Define File Paths ---
    # !!! UPDATE THIS to your raw PISA data file (e.g., the merged student/school CSV)
    raw_data_file = 'YOUR_PISA_DATA_FILE.csv'

    # !!! UPDATE THIS to be the desired name for your final, clean output file
    cleaned_output_file = 'pisa_2022_processed_final.csv'

    # --- Define all variables needed for your analysis ---
    # This includes your dependent variable, all predictors, and identifiers like CNT and SCHOOLID
    # IMPORTANT: You need a school ID here. Check the PISA codebook; it's often CNTSCHID.
    all_my_variables = [
        'ACADEMIC_RESILIENCE', 'CNT', 'CNTSCHID',
        # Background Control Variables
        'ST004D01T', 'AGE', 'ISCEDP', 'IMMIG', 'LANGN',
        # Student Psychological Variables
        'EXPECEDU', 'OCOD3', 'BSMJ', 'SISCO', 'GROSAGR', 'ANXMAT', 'MATHEFF',
        'MATHEF21', 'MATHPERS', 'ASSERAGR', 'COOPAGR', 'CURIOAGR', 'EMOCOAGR',
        'EMPATAGR', 'PERSEVAGR', 'STRESAGR', 'CREATEFF', 'CREATOP', 'IMAGINE',
        'OPENART', 'SDLEFF', 'ST268Q04JA', 'ST268Q07JA', 'ST268Q01JA',
        # Student Practice Variables
        'REPEAT', 'MISSSC', 'SKIPPING', 'TARDYSD', 'EXERPRAC', 'STUDYHMW',
        'WORKPAY', 'WORKHOME', 'INFOSEEK', 'EXPOFA', 'EXPO21ST', 'CREATAS',
        'CREATOOS',
        # Teacher & Classroom & Home Experience Variables
        'TEACHSUP', 'RELATST', 'COGACRCO', 'COGACMCO', 'DISCLIM', 'CREATSCH',
        'FAMSUP', 'CREATFAM', 'FAMSUPSL', 'FEELLAH', 'PROBSELF', 'LEARRES',
        # School Experience Variables
        'BULLIED', 'FEELSAFE', 'SCHRISK', 'BELONG', 'SCHSUST'
    ]

    process_pisa_data(raw_data_file, all_my_variables, cleaned_output_file)

