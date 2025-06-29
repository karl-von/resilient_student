import pandas as pd
import statsmodels.api as sm
from statsmodels.formula.api import mixedlm

# ---------- Step 1: 读取带 ACADEMIC_RESILIENCE 的数据（国家或全局） ----------
DATA_FILE = '../dataset/clean/STU_ESCS_below_country_25pct_resilience.csv'  # 或换成 global 的
df = pd.read_csv(DATA_FILE)

# ---------- Step 2: 去除缺失值 ----------
df = df.dropna(subset=['SCHID', 'ACADEMIC_RESILIENCE'])

# ---------- Step 3: Mixed Effects Logistic Model ----------
# 模型公式：ACADEMIC_RESILIENCE ~ 1 + (1 | SCHID)
# 使用 MixedLM 拟合二分类变量的随机截距模型（按学校）

# 注意：statsmodels MixedLM 不能直接处理二分类，需要手动处理
# 所以我们使用二分类但仍然用 Linear 模型近似 ICC（这是常见估算方法）

model = mixedlm("ACADEMIC_RESILIENCE ~ 1", df, groups=df["SCHID"])
result = model.fit()

# 提取方差组件
var_school = result.cov_re.iloc[0, 0]  # 学校层面方差
var_resid = result.scale               # 残差方差

# 计算 ICC（线性近似）
icc_linear = var_school / (var_school + var_resid)

# 计算 ICC（二分类逻辑模型近似）
icc_logit = var_school / (var_school + (3.29))  # logistic residual variance ≈ π² / 3

print("📊 ICC (linear model approximation):", round(icc_linear, 4))
print("📊 ICC (logistic model approximation):", round(icc_logit, 4))
