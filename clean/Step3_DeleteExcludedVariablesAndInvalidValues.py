import pandas as pd
import numpy as np
import os

# --- Configuration Section ---

# 1. Define the input and output file paths as you requested.
#    The script will create the output directory if it doesn't exist.
raw_data_file = '../dataset/clean/STU_ESCS_below_country_25pct_resilience.csv'
output_file = '../dataset/clean/STU_includeVariablesSet.csv'

# 2. Define the dependent variable from your research plan.
dependent_variable = 'ACADEMIC_RESILIENCE'

# --- Variable Definitions (from your VariableBook.py) ---

# Define the groups of variables to be included in the analysis.
variable_groups = {
    "Background_Control_Variables": [
        "ST004D01T", "AGE", "ISCEDP", "IMMIG", "LANGN", "CNT", "CNTSCHID", "W_FSTUWT","CNTSTUID"
    ],
    "Student_Psychological_Variables": [
        "EXPECEDU", "OCOD3", "BSMJ", "SISCO", "GROSAGR", "ANXMAT", "MATHEFF",
        "MATHEF21", "MATHPERS", "ASSERAGR", "COOPAGR", "CURIOAGR", "EMOCOAGR",
        "EMPATAGR", "PERSEVAGR", "STRESAGR", "CREATEFF", "CREATOP", "IMAGINE",
        "OPENART", "SDLEFF", "ST268Q04JA", "ST268Q07JA", "ST268Q01JA"
    ],
    "Student_Practice_Variables": [
        "REPEAT", "MISSSC", "SKIPPING", "TARDYSD", "EXERPRAC", "STUDYHMW",
        "WORKPAY", "WORKHOME", "INFOSEEK", "EXPOFA", "EXPO21ST", "CREATAS", "CREATOOS"
    ],
    "Teacher_Classroom_Home_Experience": {
        "Teacher_Classroom_Experience": [
            "TEACHSUP", "RELATST", "COGACRCO", "COGACMCO", "DISCLIM", "CREATSCH"
        ],
        "Home_Learning_Environment_Family_Support": [
            "FAMSUP", "CREATFAM", "FAMSUPSL"
        ],
        "Remote_Learning_Global_Crisis_Experience": [
            "FEELLAH", "PROBSELF", "LEARRES"
        ]
    },
    "School_Experience_Variables": [
        "BULLIED", "FEELSAFE", "SCHRISK", "BELONG", "SCHSUST"
    ]
}

def flatten_variable_groups(groups):
    """Flattens the nested dictionary of variables into a single list."""
    all_vars = []
    for v in groups.values():
        if isinstance(v, dict):
            for sub_v in v.values():
                all_vars.extend(sub_v)
        else:
            all_vars.extend(v)
    return all_vars

# Get the complete list of predictor variables.
all_predictors = flatten_variable_groups(variable_groups)

# Dictionary mapping variables to their specific invalid/missing codes.
invalid_values_dict = {
    "ST004D01T": [5.0, 7.0, 8.0, 9.0], "AGE": [9995.0, 9997.0, 9998.0, 9999.0],
    "ISCEDP": [999.0], "IMMIG": [5.0, 7.0, 8.0, 9.0], "LANGN": [997.0, 998.0, 999.0],
    "EXPECEDU": [95.0, 97.0, 98.0, 99.0], "OCOD3": [9999], "BSMJ": [95.0, 97.0, 98.0, 99.0],
    "SISCO": [5.0, 7.0, 8.0, 9.0], "GROSAGR": [95.0, 97.0, 98.0, 99.0],
    "ANXMAT": [95.0, 97.0, 98.0, 99.0], "MATHEFF": [95.0, 97.0, 98.0, 99.0],
    "MATHEF21": [95.0, 97.0, 98.0, 99.0], "MATHPERS": [95.0, 97.0, 98.0, 99.0],
    "ASSERAGR": [95.0, 97.0, 98.0, 99.0], "COOPAGR": [95.0, 97.0, 98.0, 99.0],
    "CURIOAGR": [95.0, 97.0, 98.0, 99.0], "EMOCOAGR": [95.0, 97.0, 98.0, 99.0],
    "EMPATAGR": [95.0, 97.0, 98.0, 99.0], "PERSEVAGR": [95.0, 97.0, 98.0, 99.0],
    "STRESAGR": [95.0, 97.0, 98.0, 99.0], "CREATEFF": [95.0, 97.0, 98.0, 99.0],
    "CREATOP": [95.0, 97.0, 98.0, 99.0], "IMAGINE": [95.0, 97.0, 98.0, 99.0],
    "OPENART": [95.0, 97.0, 98.0, 99.0], "SDLEFF": [95.0, 97.0, 98.0, 99.0],
    "ST268Q04JA": [95.0, 97.0, 98.0, 99.0], "ST268Q07JA": [95.0, 97.0, 98.0, 99.0],
    "ST268Q01JA": [95.0, 97.0, 98.0, 99.0], "REPEAT": [5.0, 7.0, 8.0, 9.0],
    "MISSSC": [5.0, 7.0, 8.0, 9.0], "SKIPPING": [5.0, 7.0, 8.0, 9.0],
    "TARDYSD": [5.0, 7.0, 8.0, 9.0], "EXERPRAC": [95.0, 97.0, 98.0, 99.0],
    "STUDYHMW": [95.0, 97.0, 98.0, 99.0], "WORKPAY": [95.0, 97.0, 98.0, 99.0],
    "WORKHOME": [95.0, 97.0, 98.0, 99.0], "INFOSEEK": [95.0, 97.0, 98.0, 99.0],
    "EXPOFA": [95.0, 97.0, 98.0, 99.0], "EXPO21ST": [95.0, 97.0, 98.0, 99.0],
    "CREATAS": [95.0, 97.0, 98.0, 99.0], "CREATOOS": [95.0, 97.0, 98.0, 99.0],
    "TEACHSUP": [95.0, 97.0, 98.0, 99.0], "RELATST": [95.0, 97.0, 98.0, 99.0],
    "COGACRCO": [95.0, 97.0, 98.0, 99.0], "COGACMCO": [95.0, 97.0, 98.0, 99.0],
    "DISCLIM": [95.0, 97.0, 98.0, 99.0], "CREATSCH": [95.0, 97.0, 98.0, 99.0],
    "FAMSUP": [95.0, 97.0, 98.0, 99.0], "CREATFAM": [95.0, 97.0, 98.0, 99.0],
    "FAMSUPSL": [95.0, 97.0, 98.0, 99.0], "FEELLAH": [95.0, 97.0, 98.0, 99.0],
    "PROBSELF": [95.0, 97.0, 98.0, 99.0], "LEARRES": [95.0, 97.0, 98.0, 99.0],
    "BULLIED": [95.0, 97.0, 98.0, 99.0], "FEELSAFE": [95.0, 97.0, 98.0, 99.0],
    "SCHRISK": [95.0, 97.0, 98.0, 99.0], "BELONG": [95.0, 97.0, 98.0, 99.0],
    "SCHSUST": [95.0, 97.0, 98.0, 99.0]
}

# --- Main Processing Logic ---

def main():
    """Main function to run the data cleaning process."""
    print(f"Loading data from: {raw_data_file}")
    try:
        df = pd.read_csv(raw_data_file, low_memory=False)
        print("Data loaded successfully.")
        print(f"Original dataset shape: {df.shape}")
    except FileNotFoundError:
        print(f"Error: The file was not found at {raw_data_file}. Please check the path.")
        return

    # --- Step 1: Filter dataset to keep only specified variables ---
    columns_to_keep = all_predictors + [dependent_variable]

    # Check which of the desired columns are actually in the dataframe
    existing_columns = [col for col in columns_to_keep if col in df.columns]
    missing_from_df = [col for col in columns_to_keep if col not in df.columns]

    if missing_from_df:
        print("\nWarning: The following specified variables were not found in the dataset and will be ignored:")
        print(f"  {', '.join(missing_from_df)}")

    # Create the new dataframe with only the columns that exist
    df_filtered = df[existing_columns].copy()
    print(f"\nDataset shape after selecting variables: {df_filtered.shape}")
    print(f"Kept {len(existing_columns)} columns.")

    # --- Step 2: Mark invalid values as missing (NaN) ---
    print("\nReplacing invalid values with NaN based on the codebook...")
    for column, invalid_values in invalid_values_dict.items():
        if column in df_filtered.columns:
            # Replace the list of invalid values with numpy.nan
            df_filtered[column] = df_filtered[column].replace(invalid_values, np.nan)
    print("Invalid value replacement complete.")

    # --- Step 3: Save the cleaned data ---
    # Ensure the output directory exists
    output_dir = os.path.dirname(output_file)
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Created directory: {output_dir}")

    print(f"\nSaving cleaned data to: {output_file}")
    df_filtered.to_csv(output_file, index=False)
    print("File saved successfully.")

    # --- Final Report ---
    print("\n--- Missing Value Report for the new dataset ---")
    missing_counts = df_filtered.isnull().sum()
    print("Top 15 variables with missing values:")
    print(missing_counts[missing_counts > 0].sort_values(ascending=False).head(15))
    print("--------------------------------------------")

if __name__ == '__main__':
    main()