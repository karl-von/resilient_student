import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt
import seaborn as sns
import os

# --- Configuration Section ---
imputed_file = '../dataset/analysis/imputed_standardized_final.csv'
psych_pca_file = '../dataset/analysis/pca_components_TUR_HKG.csv'
env_pca_file = '../dataset/analysis/environmental_pca_components.csv'

# Define key column names
student_id_column = 'CNTSTUID'
# --- ** FIX: Added the missing variable definitions below ** ---
imputation_id_column = 'imputation_num'
school_id_column = 'CNTSCHID'


# --- Main Processing Logic ---
def validate_with_random_forest():
    """
    Uses a Random Forest model to find the most important predictors,
    acting as a validation for the statistical model's findings.
    """
    print("--- Step 1: Loading and Preparing Final Analysis Dataset ---")
    try:
        imputed_df = pd.read_csv(imputed_file, low_memory=False)
        psych_pca_df = pd.read_csv(psych_pca_file)
        env_pca_df = pd.read_csv(env_pca_file)
        print("All data files loaded successfully.")
    except FileNotFoundError as e:
        print(f"--- ERROR --- \nFile not found. Please check path: {e.filename}")
        return

    # Take the first imputed dataset as our base for analysis
    df_base = imputed_df[imputed_df[imputation_id_column] == 1].copy()

    # Merge all data blocks
    # Start with the psych file which has the outcome variable
    final_df = pd.merge(psych_pca_df, df_base, on=student_id_column, suffixes=('', '_base'))
    final_df = pd.merge(final_df, env_pca_df, on=student_id_column, suffixes=('', '_env'))

    # Clean up columns for modeling
    # Drop IDs and redundant columns from the merge
    cols_to_drop = [col for col in final_df.columns if '_base' in col or '_env' in col]
    # This line below is now fixed because the variables are defined
    cols_to_drop.extend([imputation_id_column, student_id_column, school_id_column])
    final_df.drop(columns=cols_to_drop, inplace=True, errors='ignore')

    print("Successfully created final dataset for modeling.")

    # --- Step 2: Prepare Data for Scikit-Learn ---
    print("\n--- Step 2: Preparing data for machine learning model ---")
    y = final_df['ACADEMIC_RESILIENCE']
    X = final_df.drop('ACADEMIC_RESILIENCE', axis=1)
    X_dummified = pd.get_dummies(X, drop_first=True)
    print(f"Created dummy variables. The model will use {X_dummified.shape[1]} features.")

    # --- Step 3: Split Data into Training and Testing Sets ---
    X_train, X_test, y_train, y_test = train_test_split(
        X_dummified, y, test_size=0.3, random_state=42, stratify=y
    )
    print("Split data into 70% for training and 30% for testing.")

    # --- Step 4: Train the Random Forest Model ---
    print("\n--- Step 4: Training the Random Forest Classifier ---")
    rf_model = RandomForestClassifier(n_estimators=100, random_state=42, n_jobs=-1)
    rf_model.fit(X_train, y_train)
    print("Model training complete.")

    # --- Step 5: Evaluate the Model's Predictive Accuracy ---
    predictions = rf_model.predict(X_test)
    accuracy = accuracy_score(y_test, predictions)
    print(f"\n--- Step 5: Model Predictive Accuracy on test data: {accuracy:.2%} ---")

    # --- Step 6: Extract and Plot Feature Importances ---
    print("\n--- Step 6: Identifying Most Important Variables ---")
    importances = rf_model.feature_importances_
    feature_names = X_dummified.columns

    feature_importance_df = pd.DataFrame({
        'Feature': feature_names,
        'Importance': importances
    }).sort_values(by='Importance', ascending=False)

    print("\nTop 10 Most Important Features According to Random Forest:")
    print(feature_importance_df.head(10))

    # Plot the results
    plt.style.use('seaborn-v0_8-whitegrid')
    plt.figure(figsize=(10, 12))
    sns.barplot(x='Importance', y='Feature', data=feature_importance_df.head(20), palette='viridis')
    plt.title('Top 20 Most Important Features for Predicting Academic Resilience', fontsize=16)
    plt.xlabel('Importance Score')
    plt.ylabel('Feature')
    plt.tight_layout()

    output_path = "../dataset/analysis/feature_importance_plot.png"
    plt.savefig(output_path, dpi=300)
    print(f"\nFeature importance plot saved to: {output_path}")

if __name__ == '__main__':
    validate_with_random_forest()