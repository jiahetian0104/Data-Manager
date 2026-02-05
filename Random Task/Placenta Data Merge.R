# Notes: This R script combine placenta report data from October 2023 and placenta skeltrace output from July 2021.
#       The final output is a combined placenta data with both pathology report and skeltrace measurements.
#       The sample size for each variable can be checked after running this script.

# Author: Jiahe Tian
# Date: 2026-02-03

# empty work space, using rm() function
rm(list = ls())


# 1. Load Library ---------------------------------------------------------

library(tidyverse)
library(readxl)

# 2. Import Data ----------------------------------------------------------

placenta_report <- read_excel("Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Placenta/Pathology/MSU Pathology_SentOct23.xlsx", 
                              sheet = 1)

placenta_addition <- read_excel("Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Placenta/Pathology/Combined Data_Placenta_Report_07212023.xlsx",
                                    sheet = 1)

skeltrace_output <- read_excel("Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Placenta/placenta slide/salafia (Placenta Skeltrace)/MSU_SkeltraceOutput_July2021.xlsx",
                             sheet = 1)

# 3. Data Cleaning --------------------------------------------------------

# check columns existing in placenta_addition but not in placenta_report
variables_needed <- setdiff(colnames(placenta_addition), colnames(placenta_report))

# only take variables needed from placenta_addition
placenta_addition_subset <- placenta_addition %>% 
  select(StudyID, all_of(variables_needed)) %>%
  mutate(StudyID = as.character(StudyID))

# combine placenta_report and placenta_addition_subset
placenta_report_update <- placenta_report %>%
  left_join(placenta_addition_subset, by = "StudyID")

# rename StudyID to Specimen_ID in placenta_report_update
placenta_report_update <- placenta_report_update %>% rename(Specimen_ID = StudyID)

# take StudyID last four digits as Specimen_ID
skeltrace_output <- skeltrace_output %>%
  mutate(Specimen_ID = str_sub(StudyID, -4, -1)) %>%
  select(-StudyID)

# 4. Merge Data -----------------------------------------------------------
placenta_combined <- placenta_report_update %>%
  left_join(skeltrace_output, by = "Specimen_ID")


# 5. Data Dictionary ------------------------------------------------------

pathology_dictionary <- read_excel("Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Placenta/Pathology/Combined Data_Placenta_Report_07212023.xlsx",
                                    sheet = 2)

skeltrace_dictionary <- read_excel("Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Placenta/placenta slide/salafia (Placenta Skeltrace)/MSU_SkeltraceOutput_July2021.xlsx",
                                   sheet = 2)
## 5.1. Standardize dictionaries --------------------------------------------

# Clean/standardize header field to improve matching (trim spaces, keep as character)
pathology_dict_std <- pathology_dictionary %>%
  mutate(
    `Column Header` = as.character(`Column Header`) %>% str_trim()
  ) %>%
  distinct(`Column Header`, .keep_all = TRUE)  # keep first if duplicated

skeltrace_dict_std <- skeltrace_dictionary %>%
  mutate(
    `Column Header` = as.character(`Column Header`) %>% str_trim()
  ) %>%
  distinct(`Column Header`, .keep_all = TRUE)

## 5.2. Extract placenta_combined column names -------------------------------

combined_cols <- colnames(placenta_combined) %>%
  as.character() %>%
  str_trim()

col_df <- tibble(`Column Header` = combined_cols)

## 5.3 Compute response rate ---------------------------------------------

# Denominator: number of unique Specimen_IDs
denominator_n <- placenta_combined %>%
  filter(!is.na(Specimen_ID)) %>%
  distinct(Specimen_ID) %>%
  nrow()

# Compute non-missing count for each variable
response_rate_df <- placenta_combined %>%
  pivot_longer(
    cols = -Specimen_ID,
    names_to = "Column Header",
    values_to = "value",
    values_transform = list(value = as.character)  # <--- key fix
  ) %>%
  filter(!is.na(Specimen_ID)) %>%
  group_by(`Column Header`) %>%
  summarise(
    NonMissing_n = sum(!is.na(value) & value != ""), # also treat "" as missing
    Denominator_n = denominator_n,
    Response_Rate = NonMissing_n / Denominator_n,
    .groups = "drop"
  )

# 5.3. First match: pathology_dictionary ------------------------------------

dict_from_path <- col_df %>%
  left_join(pathology_dict_std, by = "Column Header") %>%
  mutate(Source_Dictionary = if_else(!is.na(Description), "Pathology", NA_character_))

# 5.4. Second match: skeltrace_dictionary for remaining ---------------------

# Identify headers still missing Description after pathology match
missing_headers <- dict_from_path %>%
  filter(is.na(Description)) %>%
  pull(`Column Header`)

dict_from_skel <- tibble(`Column Header` = missing_headers) %>%
  left_join(skeltrace_dict_std, by = "Column Header") %>%
  mutate(
    # Add missing fields that only exist in pathology dictionary schema
    `Response Notes` = NA_character_,
    `Variable Type` = NA_character_,
    Notes = NA_character_,
    Source_Dictionary = if_else(!is.na(Description), "Skeltrace", "Not Found")
  )

# 5.5 Combine results & harmonize final schema -----------------------------

# Keep a consistent set of columns in final dictionary
final_dictionary <- dict_from_path %>%
  filter(!(`Column Header` %in% missing_headers)) %>%
  mutate(
    Source_Dictionary = coalesce(Source_Dictionary, "Not Found")
  ) %>%
  bind_rows(dict_from_skel) %>%
  left_join(response_rate_df, by = "Column Header") %>%   # <<<<<< 新增
  select(
    `Column Header`,
    Description,
    `Response Notes`,
    `Variable Type`,
    `Unit, If Applicable`,
    Notes,
    NonMissing_n,
    `Percentage of 585 Contracted` = Response_Rate,
    Source_Dictionary
  ) %>%
  mutate(`Column Header` = factor(`Column Header`, levels = combined_cols)) %>%
  arrange(`Column Header`) %>%
  mutate(`Column Header` = as.character(`Column Header`))


# 5.6. Quick QC summaries ---------------------------------------------------

qc_summary <- final_dictionary %>%
  count(Source_Dictionary)

print(qc_summary)

# View variables not found in either dictionary (need manual curation)
not_found <- final_dictionary %>%
  filter(Source_Dictionary == "Not Found") %>%
  select(`Column Header`)

print(not_found)


# 6 Manual Change -------------------------------------------------------

## 6.1 Manually add/update dictionary entry for Specimen_ID ---------------------

specimen_row <- tibble(
  `Column Header` = "Specimen_ID",
  Description = "Specimen ID",
  `Response Notes` = "MARCH family specimen ID",
  `Variable Type` = "Text",
  `Unit, If Applicable` = NA_character_,
  Notes = NA_character_,
  Source_Dictionary = "Manual",
  NonMissing_n = NA_integer_,
  `Percentage of 585 Contracted` = NA_real_
)

final_dictionary <- final_dictionary %>%
  # Remove existing Specimen_ID row if present
  filter(`Column Header` != "Specimen_ID") %>%
  # Add the manual row
  bind_rows(specimen_row) %>%
  # Keep order aligned with placenta_combined columns (Specimen_ID usually first)
  mutate(`Column Header` = factor(`Column Header`, levels = combined_cols)) %>%
  arrange(`Column Header`) %>%
  mutate(`Column Header` = as.character(`Column Header`))

## 6.2. Create Condition Category Dictionary ---------------------------------

# --- Assumptions ---
# placenta_combined exists and has a column named Condition
# NA is recoded to "Unknown"
# Denominator for percent is fixed at 585 (contracted denominator)

denom_contracted <- 585

condition_category_dict <- placenta_combined %>%
  mutate(
    Condition = as.character(Condition),
    Condition = str_trim(Condition),
    Condition = if_else(is.na(Condition) | Condition == "", "Unknown", Condition)
  ) %>%
  count(Condition, name = "NonMissing_n") %>%
  mutate(
    `Column Header` = "",  # leave blank as requested (optional: set to "Condition")
    Description = Condition,
    `Response Notes` = "Condition Category",
    `Variable Type` = "Text",
    `Unit, If Applicable` = NA_character_,
    Notes = "Out of the 585 that have a noted physical description",
    Source_Dictionary = "Manual",
    `Percentage of 585 Contracted` = NonMissing_n / denom_contracted
  ) %>%
  select(
    `Column Header`,
    Description,
    `Response Notes`,
    `Variable Type`,
    `Unit, If Applicable`,
    Notes,
    Source_Dictionary,
    NonMissing_n,
    `Percentage of 585 Contracted`
  ) %>%
  arrange(desc(NonMissing_n))

print(condition_category_dict)

## 6.3. Insert Condition Category Dictionary after Condition row -------------
# Add row order to the main dictionary
final_dictionary_ordered <- final_dictionary %>%
  mutate(
    row_order = row_number()
  )

# Find the row number of "Condition"
condition_order <- final_dictionary_ordered %>%
  filter(`Column Header` == "Condition") %>%
  pull(row_order)

# Safety check
if (length(condition_order) == 0) {
  stop("Condition not found in final_dictionary")
}

# Prepare condition category dictionary

condition_category_ordered <- condition_category_dict %>%
  mutate(
    row_order = condition_order + 0.1   # ensure it comes right after Condition
  )

# Bind and re-order 

final_dictionary_plus <- bind_rows(
  final_dictionary_ordered,
  condition_category_ordered
) %>%
  arrange(row_order) %>%
  select(-row_order)



not_found <- final_dictionary_plus %>%
  filter(Source_Dictionary == "Not Found") %>%
  select(`Column Header`)

print(not_found)


# 7. Save the dataset -----------------------------------------------------

library(openxlsx)

# Output path -------------------------------------------------------------
output_path <- "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Placenta/Placenta_Combined_With_Dictionary.xlsx"

# Create workbook ---------------------------------------------------------
wb <- createWorkbook()

# 1. Data sheet -----------------------------------------------------------
addWorksheet(wb, "Data")
writeData(
  wb,
  sheet = "Data",
  x = placenta_combined,
  na.string = "NA"
)

# Optional: auto column width
setColWidths(wb, "Data", cols = 1:ncol(placenta_combined), widths = "auto")

# 2. Data Dictionary sheet ------------------------------------------------
addWorksheet(wb, "DataDictionary")
writeData(
  wb,
  sheet = "DataDictionary",
  x = final_dictionary_plus,
  na.string = "NA"
)

setColWidths(wb, "DataDictionary", cols = 1:ncol(final_dictionary_plus), widths = "auto")

# 3. Notes sheet ----------------------------------------------------------
notes <- c(
  "1. This dataset was created by merging the MSU placenta pathology report (October 2023) with the placenta Skeltrace output provided by MSU Pathology (July 2021).",
  "2. The accompanying data dictionary was constructed based on the original pathology report and Skeltrace documentation, with additional manual annotations where applicable.",
  "3. The dataset and data dictionary were prepared by Jiahe Tian on February 4, 2026.",
  "4. Blank cells in the dataset represent missing values (NA)."
)

addWorksheet(wb, "Notes")
writeData(
  wb,
  sheet = "Notes",
  x = notes,
  colNames = FALSE
)

setColWidths(wb, "Notes", cols = 1, widths = 120)

# Save workbook -----------------------------------------------------------
saveWorkbook(wb, output_path, overwrite = TRUE)

