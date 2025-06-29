import pandas as pd
import pyreadstat
from tqdm import tqdm

# ----------- 文件路径配置 -----------
INPUT_SAV_FILE = '../dataset/meta/CY08MSP_STU_QQQ.SAV'
COUNTRY_LOW_FILE = '../dataset/clean/STU_ESCS_below_country_25pct.csv'
OUTPUT_COUNTRY_RESILIENCE = '../dataset/clean/STU_ESCS_below_country_25pct_resilience.csv'

# ----------- 成绩列、标识列 -----------
score_cols = [f'PV{i}{s}' for s in ['MATH'] for i in range(1, 11)]
id_col = 'CNTSTUID'
country_col = 'CNT'
CHUNK_SIZE = 5000

# ----------- Step 1: 从原始文件读取分数列 + 计算平均分和百分位数 -----------
print("Step 1: 计算全局/国家平均分75百分位...")

df_scores, _ = pyreadstat.read_sav(INPUT_SAV_FILE, usecols=[id_col, country_col] + score_cols)
df_scores['AVG_SCORE'] = df_scores[score_cols].mean(axis=1, skipna=True)

# 全局前25%的分数线（即 top 25%）
global_75 = df_scores['AVG_SCORE'].quantile(0.75)

# 国家前25%的分数线（每个国家各算）
country_75_dict = df_scores.groupby(country_col)['AVG_SCORE'].quantile(0.75).to_dict()

# 建立快速查询映射
id_to_avg = df_scores.set_index(id_col)['AVG_SCORE'].to_dict()
id_to_country = df_scores.set_index(id_col)[country_col].to_dict()

del df_scores  # 节省内存


# ----------- Step 3: 添加ACADEMIC_RESILIENCE列（按国家分组） -----------
print("Step 3: 添加 ACADEMIC_RESILIENCE (按国家分组)...")
reader = pd.read_csv(COUNTRY_LOW_FILE, chunksize=CHUNK_SIZE)
with tqdm(total=sum(1 for _ in open(COUNTRY_LOW_FILE)) - 1, desc="国家数据处理中") as pbar:
    for i, chunk in enumerate(reader):
        chunk['AVG_SCORE'] = chunk[id_col].map(id_to_avg)
        chunk['CNT_MAP'] = chunk[id_col].map(id_to_country)

        chunk['ACADEMIC_RESILIENCE'] = chunk.apply(
            lambda row: 1 if row['AVG_SCORE'] > country_75_dict.get(row['CNT_MAP'], float('inf')) else 0,
            axis=1
        )

        chunk.drop(columns=['AVG_SCORE', 'CNT_MAP'], inplace=True)
        cols = ['ACADEMIC_RESILIENCE'] + [col for col in chunk.columns if col != 'ACADEMIC_RESILIENCE']
        chunk = chunk[cols]

        chunk.to_csv(OUTPUT_COUNTRY_RESILIENCE, mode='w' if i == 0 else 'a', index=False, header=(i == 0))
        pbar.update(len(chunk))

print("✅ 全部处理完成，已生成含 ACADEMIC_RESILIENCE 标签的新数据集")
