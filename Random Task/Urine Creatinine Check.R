# 1. 读取数据
file_path <- "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/THC Urine Results/ALL Urine data- July 2024.csv"



urine_data <- read.csv(file_path, stringsAsFactors = FALSE)

# 2. 查看 creatinine 相关列的 summary
summary(
  urine_data[, c(
    "creatinine_urine1",
    "creatinine_urine2",
    "creatinine_urine3"
  )]
)

urine_data %>%
  select(creatinine_urine1, creatinine_urine2, creatinine_urine3) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    p25 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    p75 = quantile(value, 0.75, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  )



library(dplyr)
library(readr)

file_extant <- "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/ECHO Extant Bioassay Data Uploads/12902/12902_12902MISUE_2025_0613/12902_12902MISU_Analysis_2025_0613.csv"

file_thc <- "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/THC Urine Results/ALL Urine data- July 2024.csv"

crosswalk_path <- "Z:/ECHO/CHARM/Data/Miscellaneous/Global Crosswalk/global_crosswalk.xlsx"

extant <- read_csv(file_extant, show_col_types = FALSE)
thc    <- read_csv(file_thc, show_col_types = FALSE)
global_crosswalk <- read_excel(crosswalk_path, sheet = 1)

# ---- 1) Fix Specimen_ID: keep last 4 digits (e.g., 13UR8902 -> 8902) ----
# Keep as character so leading zeros won't be lost (just in case).
extant2 <- extant %>%
  mutate(
    Specimen_ID_raw = Specimen_ID,
    Specimen_ID = str_extract(Specimen_ID_raw, "\\d{4}$")
  )

# ---- 2) Join crosswalk to add Mom_ECHO_ID ----
# Ensure crosswalk Specimen_ID is also character and matches the same 4-digit format.
cw2 <- global_crosswalk %>%
  mutate(Specimen_ID = as.character(Specimen_ID)) %>%
  select(Specimen_ID, MomID) %>%
  distinct()


extant3 <- extant2 %>%
  left_join(cw2, by = "Specimen_ID")

# ---- 3) Recode analyte names to match THC columns, then pivot wider ----
# IMPORTANT: replace `analysis_result_value` with the actual result column in your extant file.
# Common possibilities: result_value, analysis_result_value, results_value, value, concentration, etc.
# Run: names(extant3) to confirm the correct column name.
extant_wide <- extant3 %>%
  mutate(
    analyte_std = case_when(
      analysis_analyte_name == "11-Nor-9-carboxy-delta-9-tetrahydrocannabinol" ~ "THCCOOH_extant",
      analysis_analyte_name == "Creatinine" ~ "Creatinine_extant",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(analyte_std)) %>%
  # 给每个 MomID × analyte 的多条记录编号（保留全部结果）
  arrange(MomID, analyte_std, Specimen_ID_raw) %>%   # 如果你有日期列，用日期替换 Specimen_ID_raw 更好
  group_by(MomID, analyte_std) %>%
  mutate(rep = row_number()) %>%
  ungroup() %>%
  # 生成带后缀的列名：THCCOOH_extant_1 / _2 / _3...
  mutate(analyte_rep = paste0(analyte_std, "_", rep)) %>%
  select(MomID, analyte_rep, value = analysis_result) %>%
  pivot_wider(
    names_from = analyte_rep,
    values_from = value
  )

extant_wide_clean <- extant_wide %>%
  select(where(~ !all(is.na(.))))



pivot_analyte_to_wide <- function(df, analyte_name, prefix, value_col = "analysis_result") {
  df %>%
    filter(analysis_analyte_name == analyte_name) %>%
    arrange(MomID, Specimen_ID_raw) %>%   # 如果有日期列，建议替换成日期排序
    group_by(MomID) %>%
    mutate(rep = row_number()) %>%
    ungroup() %>%
    mutate(colname = paste0(prefix, "_", rep)) %>%
    select(MomID, colname, value = all_of(value_col)) %>%
    pivot_wider(names_from = colname, values_from = value) %>%
    select(where(~ !all(is.na(.))))
}


thccooh_extant_wide <- pivot_analyte_to_wide(
  df = extant3,
  analyte_name = "11-Nor-9-carboxy-delta-9-tetrahydrocannabinol",
  prefix = "THCCOOH_extant",
  value_col = "analysis_result"
)

creatinine_extant_wide <- pivot_analyte_to_wide(
  df = extant3,
  analyte_name = "Creatinine",
  prefix = "Creatinine_extant",
  value_col = "analysis_result"
)

# quick check
names(thccooh_extant_wide)
names(creatinine_extant_wide)


dedup_rowwise_extant <- function(df, prefix) {
  value_cols <- names(df)[str_detect(names(df), paste0("^", prefix, "_\\d+$"))]
  
  df %>%
    rowwise() %>%
    mutate(
      vals_unique = list(unique(na.omit(as.numeric(unlist(pick(all_of(value_cols)))))))
    ) %>%
    ungroup() %>%
    select(-all_of(value_cols)) %>%
    mutate(
      # 把 unique 值补齐到原来的列数长度（不够用 NA 补）
      vals_unique = lapply(vals_unique, function(v) {
        length(v) <- length(value_cols)
        v
      })
    ) %>%
    unnest_wider(vals_unique, names_sep = "_") %>%
    # 把展开出来的 vals_unique_1..n 改回原列名（prefix_1..n）
    rename_with(
      ~ value_cols,
      .cols = str_subset(names(.), "^vals_unique_\\d+$")
    )
}

thccooh_extant_wide_unique <- dedup_rowwise_extant(thccooh_extant_wide, "THCCOOH_extant") %>%
  select(where(~ !all(is.na(.))))

creatinine_extant_wide_unique <- dedup_rowwise_extant(creatinine_extant_wide, "Creatinine_extant") %>%
  select(where(~ !all(is.na(.))))


thc_long <- thc %>%
  pivot_longer(
    cols = matches("_urine\\d$"),
    names_to = c("analyte", "urine_idx"),
    names_pattern = "(.*)_urine(\\d)",
    values_to = "value"
  ) %>%
  mutate(
    urine_idx = as.integer(urine_idx)
  ) %>%
  filter(!analyte %in% c("THCCOOH_ratio", "thcscreen"))


thccooh_extant_long <- thccooh_extant_wide_unique %>%
  pivot_longer(
    cols = starts_with("THCCOOH_extant_"),
    names_to = "extant_idx",
    values_to = "value_extant"
  ) %>%
  mutate(
    extant_idx = as.integer(gsub("THCCOOH_extant_", "", extant_idx)),
    analyte = "THCCOOH"
  )

creatinine_extant_long <- creatinine_extant_wide_unique %>%
  pivot_longer(
    cols = starts_with("Creatinine_extant_"),
    names_to = "extant_idx",
    values_to = "value_extant"
  ) %>%
  mutate(
    extant_idx = as.integer(gsub("Creatinine_extant_", "", extant_idx)),
    analyte = "creatinine"
  )

extant_long <- bind_rows(thccooh_extant_long, creatinine_extant_long)

compare_df <- thc_long %>%
  filter(analyte %in% c("THCCOOH", "creatinine")) %>%
  left_join(
    extant_long,
    by = c(
      "SampleId" = "MomID",
      "analyte" = "analyte",
      "urine_idx" = "extant_idx"
    )
  ) %>%
  mutate(
    diff = value - value_extant,
    abs_diff = abs(diff),
    ratio = value / value_extant
  )

