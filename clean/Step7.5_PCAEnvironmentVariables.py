import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import seaborn as sns
import os

# --- Configuration Section ---
input_file = '../dataset/analysis/imputed_standardized_final.csv'
output_file = '../dataset/analysis/environmental_pca_components.csv'
COUNTRIES_TO_INCLUDE = ['TUR', 'HKG']
imputation_id_column = 'imputation_num'
student_id_column = 'CNTSTUID'
school_id_column = 'CNTSCHID'

PCA_GROUPS = {
    "Teacher_Classroom_Exp": ['TEACHSUP', 'RELATST', 'COGACRCO', 'COGACMCO', 'DISCLIM', 'CREATSCH'],
    "Home_Learning_Env": ['FAMSUP', 'CREATFAM', 'FAMSUPSL'],
    "Remote_Learning_Exp": ['FEELLAH', 'PROBSELF', 'LEARRES'],
    "School_Experience": ['BULLIED', 'FEELSAFE', 'SCHRISK', 'BELONG', 'SCHSUST']
}

# --- Main Processing Logic ---
def create_environmental_pca_components_with_plots():
    """
    Loads imputed data, runs PCA on environmental variable groups,
    generates diagnostic plots, and saves the final component scores.
    """
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
                print(f"  - Skipping PCA for '{group_name}' as some variables are missing.")
                continue

            group_data = df_imputation[var_list]
            scaler = StandardScaler()
            scaled_data = scaler.fit_transform(group_data)
            pca = PCA(n_components=None).fit(scaled_data)
            eigenvalues = pca.explained_variance_

            # --- ** NEW: Generate Diagnostic Plots (only for the first imputation) ** ---
            if i == 1:
                generate_pca_plots(eigenvalues, group_name)

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
    ids_to_keep = df_subset[df_subset[imputation_id_column] == 1][
        [student_id_column, school_id_column]
    ].reset_index(drop=True)

    final_df = pd.concat([ids_to_keep, averaged_components.reset_index(drop=True)], axis=1)

    final_df.to_csv(output_file, index=False)
    print(f"\nSuccess! New dataset with Environmental PCA components saved to:\n{output_file}")


# --- ** NEW: Added the plotting function from our previous script ** ---
def generate_pca_plots(eigenvalues, group_name):
    """Generates and saves a Scree Plot and Explained Variance Plot for a PCA run."""
    n_components = len(eigenvalues)
    component_numbers = np.arange(1, n_components + 1)
    explained_variance_ratio = eigenvalues / np.sum(eigenvalues)
    cumulative_explained_variance = np.cumsum(explained_variance_ratio)

    plt.style.use('seaborn-v0_8-whitegrid')
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))
    fig.suptitle(f'PCA Diagnostics for: {group_name}', fontsize=16)

    # Scree Plot (Elbow Method)
    ax1.plot(component_numbers, eigenvalues, 'o-', markerfacecolor='red', markersize=8, color='skyblue')
    ax1.axhline(y=1, color='gray', linestyle='--', label='Kaiser Criterion (Eigenvalue=1)')
    ax1.set_title('Scree Plot')
    ax1.set_xlabel('Principal Component')
    ax1.set_ylabel('Eigenvalue')
    ax1.set_xticks(component_numbers)
    ax1.legend()

    # Explained Variance Plot
    ax2.bar(component_numbers, explained_variance_ratio, alpha=0.6, color='g', label='Individual Explained Variance')
    ax2.plot(component_numbers, cumulative_explained_variance, 'o-', markerfacecolor='purple', markersize=8, color='purple', label='Cumulative Explained Variance')
    ax2.set_title('Explained Variance Plot')
    ax2.set_xlabel('Principal Component')
    ax2.set_ylabel('Proportion of Variance Explained')
    ax2.set_ylim(0, 1.1)
    ax2.set_xticks(component_numbers)
    ax2.legend(loc='center right')

    plt.tight_layout(rect=[0, 0.03, 1, 0.95])

    # Save the plot
    output_dir = "../dataset/analysis/pca_plots/"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    plt.savefig(f"{output_dir}{group_name}_pca_diagnostics.png", dpi=300)
    plt.close() # Close the plot to save memory
    print(f"  - Diagnostic plots saved for '{group_name}'")


if __name__ == '__main__':
    create_environmental_pca_components_with_plots()