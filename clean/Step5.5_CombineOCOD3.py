import pandas as pd
import numpy as np
import os

# --- Configuration Section ---

# 1. The input file from your initial cleaning steps.
input_file = '../dataset/clean/STU_includeVariablesSet.csv'

# 2. The new output file that will contain the transformed variable.
output_file = '../dataset/clean/STU_with_transformed_vars.csv'


# --- Main Processing Logic ---

def transform_categorical_variables():
    """
    Loads the dataset, creates new engineered features like OCOD3_major_group,
    and saves the result to a new file for use in imputation and modeling.
    """
    print(f"Loading data from: {input_file}")
    try:
        df = pd.read_csv(input_file, low_memory=False)
    except FileNotFoundError:
        print(f"--- ERROR --- \nFile not found at {input_file}. Please check the path.")
        return

    print("Transforming OCOD3 into major occupational groups...")

    # --- Transform OCOD3 ---
    # As we discussed, we will convert the 4-digit occupation code into its
    # 1-digit major skill group (ISCO-08 standard).

    # 1. Handle special PISA codes for missing/invalid data (e.g., 9997, 9998).
    #    We turn them into NaN (Not a Number) so they are treated as missing.
    df['OCOD3_cleaned'] = df['OCOD3'].apply(lambda x: np.nan if x in [9997, 9998, 9999] else x)

    # 2. Convert the column to a string to safely extract the first character.
    df['OCOD3_str'] = df['OCOD3_cleaned'].astype(str)

    # 3. Create the new variable by taking the first character of the string.
    df['OCOD3_major_group'] = df['OCOD3_str'].str[0]

    # We can now drop the intermediate columns
    df.drop(columns=['OCOD3_cleaned', 'OCOD3_str'], inplace=True)

    print("New column 'OCOD3_major_group' created successfully.")

    # --- Save the new dataset ---
    output_dir = os.path.dirname(output_file)
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    df.to_csv(output_file, index=False)
    print(f"\nSuccess! New dataset with transformed variables saved to:\n{output_file}")

    # Display the first few rows to verify
    print("\nVerification: First 5 rows of the new OCOD3_major_group column:")
    print(df[['OCOD3', 'OCOD3_major_group']].head())


if __name__ == '__main__':
    transform_categorical_variables()