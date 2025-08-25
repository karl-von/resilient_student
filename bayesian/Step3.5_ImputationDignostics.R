# =================================================================
# Step 4: Imputation Diagnostics
# 步骤 4: 插补诊断
# =================================================================

# --- 1. Load Libraries ---
# --- 1. 加载程序包 ---
library(mice)
library(dplyr)
library(here)

# --- 2. Define File Paths ---
# --- 2. 定义文件路径 ---

# // This is the main output file from your Step 3 script.
# // 这是步骤3脚本的主要输出文件。
INPUT_RDS_MICE <- here("dataset", "", "Step3_Imputed_Mice_Object_bayesian.rds")

# // This is the directory where the diagnostic plots will be saved.
# // 这是诊断图表将被保存的目录。
OUTPUT_PLOT_DIR <- here("dataset", "analysis", "imputation_diagnostics_bayesian")

# --- 3. Create Output Directory ---
# --- 3. 创建输出目录 ---

# // Create the directory if it doesn't already exist.
# // 如果目录尚不存在，则创建它。
if (!dir.exists(OUTPUT_PLOT_DIR)) {
  dir.create(OUTPUT_PLOT_DIR, recursive = TRUE)
  print(paste("Created directory:", OUTPUT_PLOT_DIR))
}

# --- 4. Load the Imputed `mids` Object ---
# --- 4. 加载已插补的 `mids` 对象 ---

print(paste("Loading imputed MICE object from:", INPUT_RDS_MICE))
# The saved object is a list containing the mids_object and IDs
# 保存的对象是一个列表，包含 mids_object 和 ID
saved_bundle <- readRDS(INPUT_RDS_MICE)
imputed_object <- saved_bundle$mids_object
print("MICE object loaded successfully.")

# --- 5. Identify Variables That Were Imputed ---
# --- 5. 识别被插补的变量 ---

# // We only need to create plots for variables that had missing data.
# // 我们只需要为存在缺失数据的变量创建图表。
vars_with_missing_data <- names(imputed_object$nmis[imputed_object$nmis > 0])
print(paste(
  "Found", length(vars_with_missing_data),
  "variables with missing data to diagnose."
))


# --- 6. Generate and Save Convergence Plot ---
# --- 6. 生成并保存收敛图 ---

# // This plot shows the mean and standard deviation of imputed values across iterations.
# // It helps you check if the algorithm converged to a stable solution.
# // 该图显示了每次迭代中插补值的均值和标准差。
# // 它可以帮助您检查算法是否收敛到一个稳定的解。
convergence_plot_path <- file.path(OUTPUT_PLOT_DIR, "00_convergence_plot.png")

print(paste("Saving convergence plot to:", convergence_plot_path))

# Open a PNG device
png(filename = convergence_plot_path, width = 1200, height = 800, res = 100)

# Create the plot
plot(imputed_object, layout = c(2, 1))

# Close the PNG device
dev.off()


# --- 7. Generate and Save Density Plots for Each Variable ---
# --- 7. 为每个变量生成并保存密度图 ---

# // These plots compare the distribution of the observed data (blue)
# // with the distributions of the imputed data (red).
# // 这些图表比较了观测数据(蓝色)的分布与插补数据(红色)的分布。
print("Generating density plots for each imputed variable...")

for (variable_name in vars_with_missing_data) {

  plot_filename <- paste0("density_", variable_name, ".png")
  plot_filepath <- file.path(OUTPUT_PLOT_DIR, plot_filename)

  print(paste("  - Saving plot for:", variable_name))

  png(filename = plot_filepath, width = 800, height = 600, res = 100)

  # The formula needs to be constructed like ~VARIABLE_NAME
  plot_object <- densityplot(imputed_object, data = as.formula(paste0("~", variable_name)))

  # You must explicitly print lattice plots inside a loop
  print(plot_object)

  dev.off()
}

print("--------------------------------------------------")
print("Diagnostic plotting complete.")
print(paste("All plots have been saved in:", OUTPUT_PLOT_DIR))
print("--------------------------------------------------")