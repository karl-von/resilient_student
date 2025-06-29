import pandas as pd
import pyreadstat
import os

def extract_variable_info(spss_file_path, output_csv_path, variables_to_extract):
    """
    Reads an SPSS (.sav) file to extract metadata for a specific list of variables.

    For each variable, it extracts:
    - The variable name.
    - The variable label (its description/explanation).
    - The value labels (the mapping of numerical values to their meanings).

    This information is then saved to a CSV file.

    Args:
        spss_file_path (str): The path to the input .sav file.
        output_csv_path (str): The path where the output .csv file will be saved.
        variables_to_extract (list): A list of strings with the names of the
                                     variables to look up.
    """
    print(f"Attempting to read metadata from: {spss_file_path}")

    if not os.path.exists(spss_file_path):
        print(f"--- ERROR ---")
        print(f"The file was not found at the specified path: {spss_file_path}")
        print("Please make sure the file path is correct and the file is in the right directory.")
        return

    try:
        _, meta = pyreadstat.read_sav(spss_file_path, metadataonly=True)
        print("Successfully read file metadata.")
    except Exception as e:
        print(f"An error occurred while reading the SPSS file: {e}")
        return

    extracted_data = []

    print(f"\nProcessing {len(variables_to_extract)} specified variables...")

    for var_name in variables_to_extract:
        if var_name in meta.column_names:
            var_index = meta.column_names.index(var_name)
            var_explanation = meta.column_labels[var_index]

            if var_name in meta.variable_value_labels:
                value_dict = meta.variable_value_labels[var_name]
                possible_values_str = "; ".join([f"{k}: '{v}'" for k, v in value_dict.items()])
            else:
                possible_values_str = "Continuous variable or no defined value labels."

            # Append the data with the new column name 'variableNames'
            extracted_data.append({
                'variableNames': var_name,
                'VariableExplanation': var_explanation,
                'PossibleValues': possible_values_str
            })
        else:
            # Append the data with the new column name 'variableNames'
            extracted_data.append({
                'variableNames': var_name,
                'VariableExplanation': '--- VARIABLE NOT FOUND IN FILE ---',
                'PossibleValues': '--- N/A ---'
            })

    output_df = pd.DataFrame(extracted_data)

    try:
        output_df.to_csv(output_csv_path, index=False, encoding='utf-8-sig')
        print(f"\n✅ Successfully extracted information and saved to: {output_csv_path}")
        print("\n--- First 5 rows of the output file ---")
        print(output_df.head())
    except Exception as e:
        print(f"An error occurred while saving the CSV file: {e}")


# --- Main execution block ---
if __name__ == '__main__':
    # --- Step 1: Define File Paths ---
    # In your case, the script automatically found this, which is great.
    # Adjust if needed.
    spss_file = '../dataset/meta/CY08MSP_STU_QQQ.SAV'
    output_csv = 'Variables_explanation.csv'

    # --- Step 2: Define the list of variables you are interested in ---
    variable_list = [
        # Background Control Variables
        'ST004D01T', 'AGE', 'ISCEDP', 'IMMIG', 'LANGN', 'CNT', 'CNTSCHID'
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

    # --- Step 3: Run the extraction function ---
    extract_variable_info(spss_file, output_csv, variable_list)
