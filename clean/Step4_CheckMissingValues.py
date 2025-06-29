import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import os
from matplotlib.colors import ListedColormap, BoundaryNorm # <-- New imports

# --- Configuration Section ---

# 1. Path to the dataset you created in the last step.
input_file = '../dataset/clean/STU_includeVariablesSet.csv'

# 2. Name of the column that identifies the country. From your codebook, this is 'CNT'.
country_column = 'CNT'

# 3. Threshold (in percent) for reporting a variable as having "high" missingness.
#    This is used for both the text report and the heatmap threshold.
HIGH_MISSING_THRESHOLD = 50.0

# 4. Path for the output heatmap image.
output_heatmap_file = '../dataset/analysis/missing_data_heatmap_binary.png'


# --- Main Processing Logic ---

def diagnose_missingness_binary():
    """
    Analyzes missing data percentages and visualizes them using a binary
    heatmap (Red for >50% missing, White for <=50%).
    """
    print(f"Loading data from: {input_file}")
    try:
        df = pd.read_csv(input_file, low_memory=False)
        print("Data loaded successfully.")
    except FileNotFoundError:
        print(f"--- ERROR --- \nThe file was not found at {input_file}. Please check the path.")
        return

    # --- 1. Calculate Missing Data Percentage by Country (No Changes Here) ---
    print("\nCalculating the percentage of missing values for each variable by country...")
    missing_percentage = df.groupby(country_column).apply(
        lambda x: x.isnull().sum() / len(x) * 100
    )
    print("Calculation complete.")

    # --- 2. Generate a Text-Based Report (No Changes Here) ---
    print(f"\n--- High Missingness Report (Threshold > {HIGH_MISSING_THRESHOLD}%) ---")
    high_missing_series = missing_percentage[missing_percentage > HIGH_MISSING_THRESHOLD].stack()
    if high_missing_series.empty:
        print(f"No variables found with missingness greater than {HIGH_MISSING_THRESHOLD}%.")
    else:
        for (country, variable), percentage in high_missing_series.items():
            print(f"  - Country '{country}': Variable '{variable}' is missing {percentage:.2f}% of its data.")
    print("--------------------------------------------------")

    # --- 3. Generate and Save a Binary Heatmap (MODIFIED SECTION) ---
    print(f"\nGenerating a binary heatmap (Red if >{HIGH_MISSING_THRESHOLD}% missing)...")

    vars_with_missing_data = missing_percentage.columns[missing_percentage.max() > 0]
    if vars_with_missing_data.empty:
        print("No missing data found. Skipping heatmap generation.")
        return

    filtered_percentages = missing_percentage[vars_with_missing_data]

    plt.figure(figsize=(20, 12))

    # Define a custom binary colormap and the boundaries for the colors.
    # [0, 50] -> white, (50, 101] -> red
    cmap = ListedColormap(['white', 'red'])
    bounds = [0, HIGH_MISSING_THRESHOLD, 101]
    norm = BoundaryNorm(bounds, cmap.N)

    # Create the heatmap using the custom colormap and normalization.
    # Add light gray lines to distinguish the cells clearly.
    sns.heatmap(
        filtered_percentages,
        cmap=cmap,
        norm=norm,
        linewidths=.5,
        linecolor='lightgray',
        cbar_kws={'ticks': [25, 75.5]} # Position ticks in the middle of each color block
    )

    # Get the colorbar object to customize its labels for clarity
    colorbar = plt.gcf().axes[1]
    colorbar.set_yticklabels([f'≤ {HIGH_MISSING_THRESHOLD}%', f'> {HIGH_MISSING_THRESHOLD}%'])
    colorbar.set_ylabel('Missing Data Status', rotation=270, labelpad=20)

    plt.title(f'Binary Heatmap of Missing Data (Threshold: {HIGH_MISSING_THRESHOLD}%)', fontsize=16)
    plt.xlabel('Variables')
    plt.ylabel('Countries')
    plt.xticks(rotation=45, ha="right", fontsize=8)
    plt.yticks(fontsize=8)
    plt.tight_layout()

    output_dir = os.path.dirname(output_heatmap_file)
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Created analysis directory: {output_dir}")

    plt.savefig(output_heatmap_file, dpi=300)
    print(f"Binary heatmap successfully saved to: {output_heatmap_file}")


if __name__ == '__main__':
    diagnose_missingness_binary()