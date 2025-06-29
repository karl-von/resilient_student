import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import seaborn as sns
import os

# --- Configuration Section ---
input_file = '../dataset/analysis/imputed_standardized_good_countries.csv'
output_file = '../dataset/analysis/environmental_pca_components.csv' # Will be overwritten
COUNTRIES_TO_INCLUDE = ['TUR', 'HKG']
imputation_id_column = 'imputation_num'
student_id_column = 'CNTSTUID'
school_id_column = 'CNTSCHID' #<-- Defined School ID

PCA_GROUPS = {
    "Teacher_Classroom_Exp": ['TEACHSUP', 'RELATST', 'COGACRCO', 'COGACMCO', 'DISCLIM', 'CREATSCH'],
    "Home_Learning_Env": ['FAMSUP', 'CREATFAM', 'FAMSUPSL'],
    "Remote_Learning_Exp": ['FEELLAH', 'PROBSELF', 'LEARRES'],
    "School_Experience": ['BULLIED', 'FEELSAFE', 'SCHRISK', 'BELONG', 'SCHSUST']
}

# --- Main Processing Logic ---
def create_environmental_pca_components_corrected():
    print(f"Loading imputed data from: {input_file}")
    long_format_df = pd.read_csv(input_file, low_memory=False)
    df_subset = long_format_df[long_format_df['CNT'].isin(COUNTRIES_TO_INCLUDE)].copy()
    num_imputations = int(df_subset[imputation_id_column].max())
    all_imputation_components = []

    print("\n--- Running PCA on each Imputation for Environmental Variables ---")
    for i in range(1, num_imputations + 1):
        print(f"Processing Imputation #{i}...")
        df_imputation = df_subset[df_subset[imputation_id_column] == i]
        imputation_results = []
        for group_name, var_list in PCA_GROUPS.items():
            if not all(v in df_imputation.columns for v in var_list):
                continue
            group_data = df_imputation[var_list]
            scaler = StandardScaler()
            scaled_data = scaler.fit_transform(group_data)
            pca = PCA(n_components=None).fit(scaled_data)
            eigenvalues = pca.explained_variance_
            n_components_to_keep = np.sum(eigenvalues > 1.0)
            if n_components_to_keep == 0: n_components_to_keep = 1
            pca_final = PCA(n_components=n_components_to_keep)
            component_scores = pca_final.fit_transform(scaled_data)
            component_cols = [f"{group_name}_PC{j+1}" for j in range(n_components_to_keep)]
            component_df = pd.DataFrame(component_scores, columns=component_cols, index=group_data.index)
            imputation_results.append(component_df)

        all_imputation_components.append(pd.concat(imputation_results, axis=1))

    print("\n--- Averaging Component Scores ---")
    final_components_df = pd.concat(all_imputation_components)
    averaged_components = final_components_df.groupby(final_components_df.index).mean()

    print("\nCreating final dataset with ALL necessary IDs...")
    # --- ** THIS IS THE CORRECTED PART ** ---
    ids_to_keep = df_subset[df_subset[imputation_id_column] == 1][
        [student_id_column, school_id_column] # <-- CORRECTLY INCLUDES BOTH IDs
    ].reset_index(drop=True)

    final_df = pd.concat([ids_to_keep, averaged_components.reset_index(drop=True)], axis=1)

    final_df.to_csv(output_file, index=False)
    print(f"\nSuccess! New dataset with Environmental PCA components saved to:\n{output_file}")

if __name__ == '__main__':
    create_environmental_pca_components_corrected()