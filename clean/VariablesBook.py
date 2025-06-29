# 定义各组变量的映射
variable_groups = {
    "Background_Control_Variables": [
        "ST004D01T", "AGE", "ISCEDP", "IMMIG", "LANGN", "CNT", "CNTSCHID","W_FSTUWT"
    ],
    "Student_Psychological_Variables": [
        "EXPECEDU", "OCOD3", "BSMJ", "SISCO", "GROSAGR", "ANXMAT", "MATHEFF",
        "MATHEF21", "MATHPERS", "ASSERAGR", "COOPAGR", "CURIOAGR", "EMOCOAGR",
        "EMPATAGR", "PERSEVAGR", "STRESAGR", "CREATEFF", "CREATOP", "IMAGINE",
        "OPENART", "SDLEFF", "ST268Q04JA", "ST268Q07JA", "ST268Q01JA"
    ],
    "Student_Practice_Variables": [
        "REPEAT", "MISSSC", "SKIPPING", "TARDYSD", "EXERPRAC", "STUDYHMW",
        "WORKPAY", "WORKHOME", "INFOSEEK", "EXPOFA", "EXPO21ST", "CREATAS", "CREATOOS"
    ],
    "Teacher_Classroom_Home_Experience": {
        "Teacher_Classroom_Experience": [
            "TEACHSUP", "RELATST", "COGACRCO", "COGACMCO", "DISCLIM", "CREATSCH"
        ],
        "Home_Learning_Environment_Family_Support": [
            "FAMSUP", "CREATFAM", "FAMSUPSL"
        ],
        "Remote_Learning_Global_Crisis_Experience": [
            "FEELLAH", "PROBSELF", "LEARRES"
        ]
    },
    "School_Experience_Variables": [
        "BULLIED", "FEELSAFE", "SCHRISK", "BELONG", "SCHSUST"
    ]
}

def flatten_variable_groups(groups):
    all_vars = []
    for v in groups.values():
        if isinstance(v, dict):
            for sub_v in v.values():
                all_vars.extend(sub_v)
        else:
            all_vars.extend(v)
    return all_vars

all_variables = flatten_variable_groups(variable_groups)

invalid_values_dict = {
    "ST004D01T": [5.0, 7.0, 8.0, 9.0],
    "AGE": [9995.0, 9997.0, 9998.0, 9999.0],
    "ISCEDP": [999.0],
    "IMMIG": [5.0, 7.0, 8.0, 9.0],
    "LANGN": [997.0, 998.0, 999.0],
    "CNT": [],  # 国家代码本身没有标示无效值
    "W_FSTUWT":[],
    "EXPECEDU": [95.0, 97.0, 98.0, 99.0],
    "OCOD3": [9999],
    "BSMJ": [95.0, 97.0, 98.0, 99.0],
    "SISCO": [5.0, 7.0, 8.0, 9.0],
    "GROSAGR": [95.0, 97.0, 98.0, 99.0],
    "ANXMAT": [95.0, 97.0, 98.0, 99.0],
    "MATHEFF": [95.0, 97.0, 98.0, 99.0],
    "MATHEF21": [95.0, 97.0, 98.0, 99.0],
    "MATHPERS": [95.0, 97.0, 98.0, 99.0],
    "ASSERAGR": [95.0, 97.0, 98.0, 99.0],
    "COOPAGR": [95.0, 97.0, 98.0, 99.0],
    "CURIOAGR": [95.0, 97.0, 98.0, 99.0],
    "EMOCOAGR": [95.0, 97.0, 98.0, 99.0],
    "EMPATAGR": [95.0, 97.0, 98.0, 99.0],
    "PERSEVAGR": [95.0, 97.0, 98.0, 99.0],
    "STRESAGR": [95.0, 97.0, 98.0, 99.0],
    "CREATEFF": [95.0, 97.0, 98.0, 99.0],
    "CREATOP": [95.0, 97.0, 98.0, 99.0],
    "IMAGINE": [95.0, 97.0, 98.0, 99.0],
    "OPENART": [95.0, 97.0, 98.0, 99.0],
    "SDLEFF": [95.0, 97.0, 98.0, 99.0],
    "ST268Q04JA": [95.0, 97.0, 98.0, 99.0],
    "ST268Q07JA": [95.0, 97.0, 98.0, 99.0],
    "ST268Q01JA": [95.0, 97.0, 98.0, 99.0],
    "REPEAT": [5.0, 7.0, 8.0, 9.0],
    "MISSSC": [5.0, 7.0, 8.0, 9.0],
    "SKIPPING": [5.0, 7.0, 8.0, 9.0],
    "TARDYSD": [5.0, 7.0, 8.0, 9.0],
    "EXERPRAC": [95.0, 97.0, 98.0, 99.0],
    "STUDYHMW": [95.0, 97.0, 98.0, 99.0],
    "WORKPAY": [95.0, 97.0, 98.0, 99.0],
    "WORKHOME": [95.0, 97.0, 98.0, 99.0],
    "INFOSEEK": [95.0, 97.0, 98.0, 99.0],
    "EXPOFA": [95.0, 97.0, 98.0, 99.0],
    "EXPO21ST": [95.0, 97.0, 98.0, 99.0],
    "CREATAS": [95.0, 97.0, 98.0, 99.0],
    "CREATOOS": [95.0, 97.0, 98.0, 99.0],
    "TEACHSUP": [95.0, 97.0, 98.0, 99.0],
    "RELATST": [95.0, 97.0, 98.0, 99.0],
    "COGACRCO": [95.0, 97.0, 98.0, 99.0],
    "COGACMCO": [95.0, 97.0, 98.0, 99.0],
    "DISCLIM": [95.0, 97.0, 98.0, 99.0],
    "CREATSCH": [95.0, 97.0, 98.0, 99.0],
    "FAMSUP": [95.0, 97.0, 98.0, 99.0],
    "CREATFAM": [95.0, 97.0, 98.0, 99.0],
    "FAMSUPSL": [95.0, 97.0, 98.0, 99.0],
    "FEELLAH": [95.0, 97.0, 98.0, 99.0],
    "PROBSELF": [95.0, 97.0, 98.0, 99.0],
    "LEARRES": [95.0, 97.0, 98.0, 99.0],
    "BULLIED": [95.0, 97.0, 98.0, 99.0],
    "FEELSAFE": [95.0, 97.0, 98.0, 99.0],
    "SCHRISK": [95.0, 97.0, 98.0, 99.0],
    "BELONG": [95.0, 97.0, 98.0, 99.0],
    "SCHSUST": [95.0, 97.0, 98.0, 99.0],
}
