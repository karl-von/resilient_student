# =================================================================
# Step 5: Final Assembly - Add Outcomes and Weights to Engineered Datasets
# =================================================================

# --- 1. Load Libraries ---
library(dplyr)
library(here)

# --- 2. Define File Paths ---
# Input is the LIST of engineered datasets from the new Step 4
INPUT_ENGINEERED_LIST <- here("dataset", "clean_R", "Step4_ListOfEngineeredDatasets.rds")
# Input is also the foundational data from Step 1, which has outcomes and weights
INPUT_FOUNDATIONAL_DATA <- here("dataset", "clean_R", "Step1_Resilience_Flags_Corrected.rds")
# The final output is the list of datasets ready for the analysis models
OUTPUT_MODEL_READY_LIST <- here("dataset", "clean_R", "Step5_Final_Model_Ready_List.rds")


# --- 3. Load Input Data ---
print("Loading engineered predictor datasets from Step 4...")
list_of_engineered_datasets <- readRDS(INPUT_ENGINEERED_LIST)

print("Loading foundational data (outcomes & weights) from Step 1...")
foundational_data <- readRDS(INPUT_FOUNDATIONAL_DATA)


# --- 4. Prepare and Merge ---

# To make the merge clean, select only the columns we need from the foundational data
outcomes_and_weights <- foundational_data %>%
  select(
    CNTSTUID, # The key for merging
    W_FSTUWT,
    starts_with("W_FSTURWT"),
    starts_with("ACADEMIC_RESILIENCE_PV")
  )

print("Merging outcomes and weights into each of the 5 datasets...")
# Loop through the list of engineered datasets
final_model_ready_list <- lapply(list_of_engineered_datasets, function(engineered_df) {

  # Use a left_join to add the outcomes and weights by student ID
  final_df <- left_join(engineered_df, outcomes_and_weights, by = "CNTSTUID")

  return(final_df)
})


# --- 5. Save Final Model-Ready List ---
print("Saving the final, complete, model-ready list of datasets...")
saveRDS(final_model_ready_list, file = OUTPUT_MODEL_READY_LIST)

print("✅ Success! Your data is now fully prepared and ready for your analysis scripts.")

# # Load your final list of datasets
# df <- readRDS(here("dataset", "clean_R", "Step5_Final_Model_Ready_List.rds"))
#
# # To see the variable names of the FIRST dataframe in the list:
# names(df[[1]])
#
# # To see the structure of the FIRST dataframe in the list:
# str(df[[1]])
#  [1] "CNTSTUID"
#   [2] "CNTSCHID"
#   [3] "ESCS"
#   [4] "PV1MATH"
#   [5] "PV2MATH"
#   [6] "PV3MATH"
#   [7] "PV4MATH"
#   [8] "PV5MATH"
#   [9] "PV6MATH"
#  [10] "PV7MATH"
#  [11] "PV8MATH"
#  [12] "PV9MATH"
#  [13] "PV10MATH"
#  [14] "ST004D01T"
#  [15] "ST268Q01JA"
#  [16] "ST268Q04JA"
#  [17] "ST268Q07JA"
#  [18] "AGE"
#  [19] "ISCEDP"
#  [20] "IMMIG"
#  [21] "REPEAT"
#  [22] "MISSSC"
#  [23] "SKIPPING"
#  [24] "TARDYSD"
#  [25] "EXERPRAC"
#  [26] "STUDYHMW"
#  [27] "WORKPAY"
#  [28] "WORKHOME"
#  [29] "EXPECEDU"
#  [30] "BSMJ"
#  [31] "SISCO"
#  [32] "RELATST"
#  [33] "BELONG"
#  [34] "BULLIED"
#  [35] "FEELSAFE"
#  [36] "SCHRISK"
#  [37] "PERSEVAGR"
#  [38] "CURIOAGR"
#  [39] "COOPAGR"
#  [40] "EMPATAGR"
#  [41] "ASSERAGR"
#  [42] "STRESAGR"
#  [43] "EMOCOAGR"
#  [44] "GROSAGR"
#  [45] "INFOSEEK"
#  [46] "FAMSUP"
#  [47] "DISCLIM"
#  [48] "TEACHSUP"
#  [49] "COGACRCO"
#  [50] "COGACMCO"
#  [51] "EXPOFA"
#  [52] "EXPO21ST"
#  [53] "MATHEFF"
#  [54] "MATHEF21"
#  [55] "ANXMAT"
#  [56] "MATHPERS"
#  [57] "CREATEFF"
#  [58] "CREATSCH"
#  [59] "CREATFAM"
#  [60] "CREATAS"
#  [61] "CREATOOS"
#  [62] "CREATOP"
#  [63] "OPENART"
#  [64] "IMAGINE"
#  [65] "SCHSUST"
#  [66] "LEARRES"
#  [67] "PROBSELF"
#  [68] "FAMSUPSL"
#  [69] "FEELLAH"
#  [70] "SDLEFF"
#  [71] "OCOD3_major_group"
#  [72] "Math_Disposition_RC1"
#  [73] "Math_Disposition_RC2"
#  [74] "Social_Emotional_Skills_RC1"
#  [75] "Social_Emotional_Skills_RC2"
#  [76] "Openness_Creativity_PC1"
#  [77] "Self_Directed_Learning_PC1"
#  [78] "Teacher_Classroom_Exp_RC1"
#  [79] "Teacher_Classroom_Exp_RC2"
#  [80] "Home_Learning_Env_PC1"
#  [81] "Remote_Learning_Exp_PC1"
#  [82] "School_Experience_RC1"
#  [83] "School_Experience_RC2"
#  [84] "ESCS_sch_mean"
#  [85] "AGE_sch_mean"
#  [86] "BSMJ_sch_mean"
#  [87] "GROSAGR_sch_mean"
#  [88] "ANXMAT_sch_mean"
#  [89] "MATHEFF_sch_mean"
#  [90] "MATHEF21_sch_mean"
#  [91] "MATHPERS_sch_mean"
#  [92] "ASSERAGR_sch_mean"
#  [93] "COOPAGR_sch_mean"
#  [94] "CURIOAGR_sch_mean"
#  [95] "EMOCOAGR_sch_mean"
#  [96] "EMPATAGR_sch_mean"
#  [97] "PERSEVAGR_sch_mean"
#  [98] "STRESAGR_sch_mean"
#  [99] "CREATEFF_sch_mean"
# [100] "CREATOP_sch_mean"
# [101] "IMAGINE_sch_mean"
# [102] "OPENART_sch_mean"
# [103] "SDLEFF_sch_mean"
# [104] "ST268Q04JA_sch_mean"
# [105] "ST268Q07JA_sch_mean"
# [106] "ST268Q01JA_sch_mean"
# [107] "MISSSC_sch_mean"
# [108] "SKIPPING_sch_mean"
# [109] "TARDYSD_sch_mean"
# [110] "EXERPRAC_sch_mean"
# [111] "STUDYHMW_sch_mean"
# [112] "WORKPAY_sch_mean"
# [113] "WORKHOME_sch_mean"
# [114] "INFOSEEK_sch_mean"
# [115] "EXPOFA_sch_mean"
# [116] "EXPO21ST_sch_mean"
# [117] "CREATAS_sch_mean"
# [118] "CREATOOS_sch_mean"
# [119] "TEACHSUP_sch_mean"
# [120] "RELATST_sch_mean"
# [121] "COGACRCO_sch_mean"
# [122] "COGACMCO_sch_mean"
# [123] "DISCLIM_sch_mean"
# [124] "CREATSCH_sch_mean"
# [125] "FAMSUP_sch_mean"
# [126] "CREATFAM_sch_mean"
# [127] "FAMSUPSL_sch_mean"
# [128] "FEELLAH_sch_mean"
# [129] "PROBSELF_sch_mean"
# [130] "LEARRES_sch_mean"
# [131] "BULLIED_sch_mean"
# [132] "FEELSAFE_sch_mean"
# [133] "SCHRISK_sch_mean"
# [134] "BELONG_sch_mean"
# [135] "SCHSUST_sch_mean"
# [136] "Math_Disposition_RC1_sch_mean"
# [137] "Math_Disposition_RC2_sch_mean"
# [138] "Social_Emotional_Skills_RC1_sch_mean"
# [139] "Social_Emotional_Skills_RC2_sch_mean"
# [140] "Openness_Creativity_PC1_sch_mean"
# [141] "Self_Directed_Learning_PC1_sch_mean"
# [142] "Teacher_Classroom_Exp_RC1_sch_mean"
# [143] "Teacher_Classroom_Exp_RC2_sch_mean"
# [144] "Home_Learning_Env_PC1_sch_mean"
# [145] "Remote_Learning_Exp_PC1_sch_mean"
# [146] "School_Experience_RC1_sch_mean"
# [147] "School_Experience_RC2_sch_mean"
# [148] "W_FSTUWT"
# [149] "W_FSTURWT1"
# [150] "W_FSTURWT2"
# [151] "W_FSTURWT3"
# [152] "W_FSTURWT4"
# [153] "W_FSTURWT5"
# [154] "W_FSTURWT6"
# [155] "W_FSTURWT7"
# [156] "W_FSTURWT8"
# [157] "W_FSTURWT9"
# [158] "W_FSTURWT10"
# [159] "W_FSTURWT11"
# [160] "W_FSTURWT12"
# [161] "W_FSTURWT13"
# [162] "W_FSTURWT14"
# [163] "W_FSTURWT15"
# [164] "W_FSTURWT16"
# [165] "W_FSTURWT17"
# [166] "W_FSTURWT18"
# [167] "W_FSTURWT19"
# [168] "W_FSTURWT20"
# [169] "W_FSTURWT21"
# [170] "W_FSTURWT22"
# [171] "W_FSTURWT23"
# [172] "W_FSTURWT24"
# [173] "W_FSTURWT25"
# [174] "W_FSTURWT26"
# [175] "W_FSTURWT27"
# [176] "W_FSTURWT28"
# [177] "W_FSTURWT29"
# [178] "W_FSTURWT30"
# [179] "W_FSTURWT31"
# [180] "W_FSTURWT32"
# [181] "W_FSTURWT33"
# [182] "W_FSTURWT34"
# [183] "W_FSTURWT35"
# [184] "W_FSTURWT36"
# [185] "W_FSTURWT37"
# [186] "W_FSTURWT38"
# [187] "W_FSTURWT39"
# [188] "W_FSTURWT40"
# [189] "W_FSTURWT41"
# [190] "W_FSTURWT42"
# [191] "W_FSTURWT43"
# [192] "W_FSTURWT44"
# [193] "W_FSTURWT45"
# [194] "W_FSTURWT46"
# [195] "W_FSTURWT47"
# [196] "W_FSTURWT48"
# [197] "W_FSTURWT49"
# [198] "W_FSTURWT50"
# [199] "W_FSTURWT51"
# [200] "W_FSTURWT52"
# [201] "W_FSTURWT53"
# [202] "W_FSTURWT54"
# [203] "W_FSTURWT55"
# [204] "W_FSTURWT56"
# [205] "W_FSTURWT57"
# [206] "W_FSTURWT58"
# [207] "W_FSTURWT59"
# [208] "W_FSTURWT60"
# [209] "W_FSTURWT61"
# [210] "W_FSTURWT62"
# [211] "W_FSTURWT63"
# [212] "W_FSTURWT64"
# [213] "W_FSTURWT65"
# [214] "W_FSTURWT66"
# [215] "W_FSTURWT67"
# [216] "W_FSTURWT68"
# [217] "W_FSTURWT69"
# [218] "W_FSTURWT70"
# [219] "W_FSTURWT71"
# [220] "W_FSTURWT72"
# [221] "W_FSTURWT73"
# [222] "W_FSTURWT74"
# [223] "W_FSTURWT75"
# [224] "W_FSTURWT76"
# [225] "W_FSTURWT77"
# [226] "W_FSTURWT78"
# [227] "W_FSTURWT79"
# [228] "W_FSTURWT80"
# [229] "ACADEMIC_RESILIENCE_PV1"
# [230] "ACADEMIC_RESILIENCE_PV2"
# [231] "ACADEMIC_RESILIENCE_PV3"
# [232] "ACADEMIC_RESILIENCE_PV4"
# [233] "ACADEMIC_RESILIENCE_PV5"
# [234] "ACADEMIC_RESILIENCE_PV6"
# [235] "ACADEMIC_RESILIENCE_PV7"
# [236] "ACADEMIC_RESILIENCE_PV8"
# [237] "ACADEMIC_RESILIENCE_PV9"
# [238] "ACADEMIC_RESILIENCE_PV10"