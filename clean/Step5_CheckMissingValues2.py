import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import os
from matplotlib.colors import ListedColormap, BoundaryNorm # To create the binary heatmap

# --- Configuration Section ---

# 1. Path to the dataset.
input_file = '../dataset/clean/STU_includeVariablesSet.csv'

# 2. Name of the country identifier column.
country_column = 'CNT'

# 3. Thresholds for the logic.
MISSING_VALUE_THRESHOLD = 50.0 # The value considered "highly missing" (50%).
PROPORTION_TO_EXCLUDE = 0.50   # Proportion to flag a country/variable for exclusion (80%).
PROPORTION_FOR_GOOD_DATA = 0.20 # Proportion to flag a country as "good" (10%).

# 4. Path for the new filtered heatmap image.
output_heatmap_file = '../dataset/analysis/missing_data_heatmap_filtered_binary.png'



# --- Main Processing Logic ---

def advanced_analysis_and_binary_plot():
    """
    Runs a full analysis of missing data, provides a detailed report on both
    highly problematic and high-quality countries/variables, and generates a
    filtered, binary heatmap.
    """
    print(f"Loading data from: {input_file}")
    try:
        df = pd.read_csv(input_file, low_memory=False)
        print("Data loaded successfully.")
    except FileNotFoundError:
        print(f"--- ERROR --- \nThe file was not found at {input_file}. Please check the path.")
        return

    # --- 1. Calculate Base Missing Data Percentage Table ---
    print("\nCalculating the percentage of missing values for each variable by country...")
    missing_percentage = df.groupby(country_column).apply(
        lambda x: x.isnull().sum() / len(x) * 100
    )
    print("Calculation complete.")

    # --- 2. Identify Countries and Variables for Exclusion and Reporting ---
    is_highly_missing = missing_percentage >= MISSING_VALUE_THRESHOLD

    # a) Identify countries/variables to EXCLUDE (>80% rule)
    prop_missing_in_country = is_highly_missing.mean(axis=1)
    countries_to_exclude = prop_missing_in_country[prop_missing_in_country > PROPORTION_TO_EXCLUDE].index.tolist()

    prop_missing_in_variable = is_highly_missing.mean(axis=0)
    variables_to_exclude = prop_missing_in_variable[prop_missing_in_variable > PROPORTION_TO_EXCLUDE].index.tolist()

    # b) Identify "Good" countries for REPORTING (<10% rule)
    good_countries = prop_missing_in_country[prop_missing_in_country < PROPORTION_FOR_GOOD_DATA].index.tolist()

    # --- 3. Generate the Expanded Console Report ---
    print("\n--- Comprehensive Missing Data Report ---")

    # Part A: Report on items to be excluded
    print(f"\nPart A: Exclusion Analysis (Based on >{PROPORTION_TO_EXCLUDE*100:.0f}% Rule)")
    if countries_to_exclude:
        print(f"[!] Countries to Exclude ({len(countries_to_exclude)} found): {', '.join(countries_to_exclude)}")
    else:
        print("[✓] No countries met the criteria for exclusion.")
    if variables_to_exclude:
        print(f"[!] Variables to Exclude ({len(variables_to_exclude)} found): {', '.join(variables_to_exclude)}")
    else:
        print("[✓] No variables met the criteria for exclusion.")

    # Part B: Report on "Good" countries
    print(f"\nPart B: 'Good' Country Analysis (Based on <{PROPORTION_FOR_GOOD_DATA*100:.0f}% Rule)")
    if good_countries:
        print(f"[✓] Found {len(good_countries)} countries with high data quality:")
        for country in good_countries:
            # Find the specific variables with >50% missingness for this "good" country
            problem_vars = is_highly_missing.loc[country]
            problem_vars_list = problem_vars[problem_vars].index.tolist()
            if not problem_vars_list:
                print(f"  - {country}: Excellent. Has NO variables with >{MISSING_VALUE_THRESHOLD}% missing data.")
            else:
                print(f"  - {country}: Very good. Only {len(problem_vars_list)} problematic variable(s): {', '.join(problem_vars_list)}")
    else:
        print("[!] No countries met the criteria for 'Good' data quality.")
    print("---------------------------------------")

    # --- 4. Prepare Data for the Binary Heatmap ---
    filtered_heatmap_data = missing_percentage.drop(
        index=countries_to_exclude, columns=variables_to_exclude, errors='ignore'
    )

    if filtered_heatmap_data.empty:
        print("\n--- WARNING --- After filtering, no data remains to be plotted. Skipping heatmap.")
        return

    print("\nGenerating a filtered, binary heatmap...")

    # --- 5. Generate the Filtered, Binary Heatmap ---
    plt.figure(figsize=(20, max(10, len(filtered_heatmap_data.index) * 0.3))) # Dynamic height

    cmap = ListedColormap(['white', 'red'])
    bounds = [0, MISSING_VALUE_THRESHOLD, 101]
    norm = BoundaryNorm(bounds, cmap.N)

    sns.heatmap(
        filtered_heatmap_data, cmap=cmap, norm=norm, linewidths=.5, linecolor='lightgray',
        cbar_kws={'ticks': [25, 75.5]}
    )

    colorbar = plt.gcf().axes[1]
    colorbar.set_yticklabels([f'≤ {MISSING_VALUE_THRESHOLD}%', f'> {MISSING_VALUE_THRESHOLD}%'])
    colorbar.set_ylabel('Missing Data Status', rotation=270, labelpad=20)

    plt.title(f'Binary Heatmap of Filtered Data (Threshold: {MISSING_VALUE_THRESHOLD}%)', fontsize=16, pad=20)
    plt.xlabel('Variables')
    plt.ylabel('Countries (Excluding Highly Problematic Ones)')
    plt.xticks(rotation=45, ha="right", fontsize=8)
    plt.yticks(fontsize=8)
    plt.tight_layout()

    output_dir = os.path.dirname(output_heatmap_file)
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    plt.savefig(output_heatmap_file, dpi=300)
    print(f"Filtered binary heatmap successfully saved to: {output_heatmap_file}")

if __name__ == '__main__':
    advanced_analysis_and_binary_plot()