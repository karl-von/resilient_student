import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import seaborn as sns
import os

# --- Configuration Section ---
input_file = '../dataset/analysis/imputed_standardized_good_countries.csv'
output_file = '../dataset/analysis/pca_components_TUR_HKG.csv'
COUNTRIES_TO_INCLUDE = ['TUR', 'HKG']
imputation_id_column = 'imputation_num'

# --- ** IMPORTANT: Make sure this column name is correct ** ---
student_id_column = 'CNTSTUID'

# Define Your Variable Groupings for PCA
PCA_GROUPS = {
    "Math_Disposition": [
        'ANXMAT', 'MATHEFF', 'MATHEF21', 'MATHPERS',
        'ST268Q04JA', 'ST268Q07JA', 'ST268Q01JA'
    ],
    "Social_Emotional_Skills": [
        'ASSERAGR', 'COOPAGR', 'EMOCOAGR', 'EMPATAGR', 'PERSEVAGR', 'STRESAGR'
    ],
    "Openness_Creativity": [
        'CURIOAGR', 'CREATEFF', 'CREATOP', 'IMAGINE', 'OPENART'
    ],
    "Self_Directed_Learning": [
        'SDLEFF', 'GROSAGR'
    ]
}

# --- Main Processing Logic ---
def create_pca_components_corrected():
    """
    Loads imputed data, runs PCA on defined variable groups for each imputation,
    averages the component scores, and generates diagnostic plots.
    """
    print(f"Loading imputed data from: {input_file}")
    try:
        long_format_df = pd.read_csv(input_file, low_memory=False)
    except FileNotFoundError:
        print(f"--- ERROR --- \nFile not found at {input_file}. Please check the path.")
        return

    print(f"Filtering data for: {', '.join(COUNTRIES_TO_INCLUDE)}")
    df_subset = long_format_df[long_format_df['CNT'].isin(COUNTRIES_TO_INCLUDE)].copy()
    num_imputations = int(df_subset[imputation_id_column].max())
    all_imputation_components = []

    print("\n--- Running PCA on each Imputation ---")
    for i in range(1, num_imputations + 1):
        print(f"Processing Imputation #{i}...")
        df_imputation = df_subset[df_subset[imputation_id_column] == i]
        imputation_results = []

        for group_name, var_list in PCA_GROUPS.items():
            group_data = df_imputation[var_list]
            scaler = StandardScaler()
            scaled_data = scaler.fit_transform(group_data)
            pca = PCA(n_components=None)
            pca.fit(scaled_data)
            eigenvalues = pca.explained_variance_
            n_components_to_keep = np.sum(eigenvalues > 1.0)
            if n_components_to_keep == 0: n_components_to_keep = 1
            pca_final = PCA(n_components=n_components_to_keep)
            component_scores = pca_final.fit_transform(scaled_data)
            component_cols = [f"{group_name}_PC{j+1}" for j in range(n_components_to_keep)]
            component_df = pd.DataFrame(component_scores, columns=component_cols, index=group_data.index)
            imputation_results.append(component_df)

            if i == 1:
                generate_pca_plots(eigenvalues, group_name)

        all_imputation_components.append(pd.concat(imputation_results, axis=1))

    print("\n--- Averaging Component Scores Across All Imputations ---")
    final_components_df = pd.concat(all_imputation_components)
    averaged_components = final_components_df.groupby(final_components_df.index).mean()
    print("Averaging complete.")

    # --- Create final dataset for modeling ---
    print("\nCreating final dataset with student IDs...")

    # --- ** THIS IS THE CORRECTED LINE ** ---
    ids_and_outcome = df_subset[df_subset[imputation_id_column] == 1][
        ['CNT', 'CNTSCHID', student_id_column, 'ACADEMIC_RESILIENCE'] # <-- CORRECTLY INCLUDES STUDENT ID
    ].reset_index(drop=True)

    final_df_for_model = pd.concat([ids_and_outcome, averaged_components.reset_index(drop=True)], axis=1)

    output_dir = os.path.dirname(output_file)
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    final_df_for_model.to_csv(output_file, index=False)
    print(f"\nSuccess! New dataset WITH STUDENT IDs saved to:\n{output_file}")


def generate_pca_plots(eigenvalues, group_name):
    """Generates and saves a Scree Plot and Explained Variance Plot for a PCA run."""
    n_components = len(eigenvalues)
    component_numbers = np.arange(1, n_components + 1)
    explained_variance_ratio = eigenvalues / np.sum(eigenvalues)
    cumulative_explained_variance = np.cumsum(explained_variance_ratio)
    plt.style.use('seaborn-v0_8-whitegrid')
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))
    fig.suptitle(f'PCA Diagnostics for: {group_name}', fontsize=16)
    ax1.plot(component_numbers, eigenvalues, 'o-', markerfacecolor='red', markersize=8, color='skyblue')
    ax1.axhline(y=1, color='gray', linestyle='--', label='Kaiser Criterion (Eigenvalue=1)')
    ax1.set_title('Scree Plot')
    ax1.set_xlabel('Principal Component')
    ax1.set_ylabel('Eigenvalue')
    ax1.set_xticks(component_numbers)
    ax1.legend()
    ax2.bar(component_numbers, explained_variance_ratio, alpha=0.6, color='g', label='Individual Explained Variance')
    ax2.plot(component_numbers, cumulative_explained_variance, 'o-', markerfacecolor='purple', markersize=8, color='purple', label='Cumulative Explained Variance')
    ax2.set_title('Explained Variance Plot')
    ax2.set_xlabel('Principal Component')
    ax2.set_ylabel('Proportion of Variance Explained')
    ax2.set_ylim(0, 1.1)
    ax2.set_xticks(component_numbers)
    ax2.legend(loc='center right')
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    output_dir = "../dataset/analysis/pca_plots/"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    plt.savefig(f"{output_dir}{group_name}_pca_diagnostics.png", dpi=300)
    plt.close()
    print(f"  - Diagnostic plots saved for '{group_name}'")


if __name__ == '__main__':
    create_pca_components_corrected()