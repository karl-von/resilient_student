import pandas as pd
import pyreadstat
from tqdm import tqdm

# 文件路径
INPUT_SAV_FILE = '../dataset/meta/CY08MSP_STU_QQQ.SAV'
OUTPUT_METADATA_FILE = '../dataset/clean/STU_variables_explains.csv'
OUTPUT_COUNTRY_FILTER_FILE = '../dataset/clean/STU_ESCS_below_country_25pct.csv'

CHUNK_SIZE = 5000  # 极小块，适合你的电脑

# Step 1: 提取变量说明 + 总行数
print("Step 1: Getting metadata...")
_, meta = pyreadstat.read_sav(INPUT_SAV_FILE, metadataonly=True)
row_count = meta.number_rows

# 保存变量标签
var_labels = pd.DataFrame({
    'Variable': meta.column_names,
    'Label': meta.column_labels
})
var_labels.to_csv(OUTPUT_METADATA_FILE, index=False)

# Step 2: 提前读 ESCS 和 CNT 做统计
print("Step 2: Pre-scanning ESCS & CNT...")
df_meta, _ = pyreadstat.read_sav(INPUT_SAV_FILE, usecols=['CNT', 'ESCS'])
df_meta = df_meta.dropna(subset=['ESCS'])
global_25 = df_meta['ESCS'].quantile(0.25)
valid_countries = df_meta['CNT'].value_counts()
valid_countries = valid_countries[valid_countries >= 100].index
country_25 = df_meta[df_meta['CNT'].isin(valid_countries)] \
    .groupby('CNT')['ESCS'].quantile(0.25).to_dict()
del df_meta

# Step 3: 分块读取原始文件 + 过滤 + 写出（超低内存版）
print("Step 3: Chunked filtering with progress bar...")

# 先写出空文件（含表头）
empty_df, _ = pyreadstat.read_sav(INPUT_SAV_FILE, row_limit=1)
cols = empty_df.columns
pd.DataFrame(columns=cols).to_csv(OUTPUT_COUNTRY_FILTER_FILE, index=False)

# 分块处理 + tqdm
with tqdm(total=row_count, desc="Filtering chunks") as pbar:
    offset = 0
    while offset < row_count:
        try:
            chunk, _ = pyreadstat.read_sav(INPUT_SAV_FILE,
                                           row_offset=offset,
                                           row_limit=CHUNK_SIZE)
            chunk = chunk.dropna(subset=['ESCS'])

            # Country filter
            chunk = chunk[chunk['CNT'].isin(valid_countries)]
            chunk = chunk[chunk['ESCS'] <= chunk['CNT'].map(country_25)]
            chunk.to_csv(OUTPUT_COUNTRY_FILTER_FILE, mode='a', header=False, index=False)

            offset += CHUNK_SIZE
            pbar.update(CHUNK_SIZE)
        except Exception as e:
            print(f"❌ Error reading at offset {offset}: {e}")
            break

print("✅ Done without crashing.")
