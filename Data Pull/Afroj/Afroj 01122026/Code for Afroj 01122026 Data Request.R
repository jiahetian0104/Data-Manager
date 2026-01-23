# This code is to prepare data for Sara Afroj's request on 01/12/2026.

# Clean the list
rm(list=ls())
# set up work path
setwd("Z:/ECHO/CHARM/Data/")

# 1. Load library ---------------------------------------------------------
library(tidyverse)
library(readxl)

# 2. Import Data ----------------------------------------------------------

## 2.0 Data Request Path ----------------------------------------------------
data_request_path <- "Z:/ECHO/CHARM/Data/Data Pulls/Afroj/Afroj_01122026_CHARMRequest.xlsx"

## 2.1 Requested MARCH_ID --------------------------------------------------

# If there is a specific ID list, then use the list. Other wise, use cohort-specific ID list.

# ID_raw <- read_xlsx(data_request_path, sheet = 3)
# # Keep only first column, drop first row, set 2nd row as column name
# ID <- ID_raw %>%
#   select(1) %>%                # keep only first column
#   slice(-1,-2) %>%                # remove first row
#   rename(col1 = 1)             # temporarily rename column
# # Use the first value (second row of raw data) as column name
# colnames(ID) <- ID_raw[[2,1]]  # take value of first column, 2nd row from ID_raw

# Since there is no ID list in the data request, use all MARCH_IDs in the cohort

crosswalk_raw <- read_excel(
  "Miscellaneous/Global Crosswalk/global_crosswalk.xlsx",
  na = c("", "NA"),
  sheet = 1
)

# Filter and clean mother-child crosswalk
crosswalk_mothers <- crosswalk_raw %>%
  filter(str_starts(MomID, "P")) %>%
  filter(
    !(is.na(ChildID) & is.na(Child_ECHO_ID))
  ) %>%
  select(
    child_id = ChildID,
    mom_id = MomID,
    child_echo_id = Child_ECHO_ID,
    mom_echo_id = Mom_ECHO_ID
  ) %>%
  distinct(mom_id, child_id, .keep_all = TRUE) # remove duplicates if any


## 2.2 Requested Variables -------------------------------------------------
variables <- read_xlsx(
  data_request_path,
  sheet = 2
) %>%
  select(1, 3)   # keep only column 1 and 4

# check out what visits are request, then prepare datasets
# Code Derived needs to be prepared separately
table(variables$Visit)

## 2.3 Import Datasets -----------------------------------------------------

# Birth Certificate Data
birth_certificate <- read.csv("ECHO 1/MDHHS Data/Birth_Certificate/MARCH_BC_FinalUpd.csv")

# Prenatal 1 Data
prenatal1 <- read.csv("ECHO 1/SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_1/Data_Delivery/Prenatal_Survey1/PRENATAL_1_SURVEY_mixedformats.csv")

# 3 Month Data
postnatal_3month_IW <- read.csv("ECHO 1/SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_2/Phase2_3month_Data_Delivery/IW/echo3mo_data_all_extra_MixedFormats.csv")

postnatal_3month_prior_IW <- read.csv("ECHO 1/SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_2/Phase2_3month_Data_Delivery/Prior_IW_Data_Older_Instrument/echo3mo_data_all_extra_mixedformats.csv")


# 3. Prepare Variables ----------------------------------------------------


## 3.0 Function ------------------------------------------------------------

prep_requested_vars_by_visit <- function(
    variables_df,
    dataset,
    visit_value,
    var_col = "Variable Name",
    visit_col = "Visit",
    id_vars = c("march_id", "ChildID"),
    rename_ids = c(child_id = "ChildID"),
    verbose = TRUE
) {
  # ---- 1) Pull requested variables for the given visit ----
  # visit_value can be a single value or a vector of values
  req_vars <- variables_df %>%
    dplyr::filter(.data[[visit_col]] %in% visit_value) %>%
    dplyr::pull(.data[[var_col]]) %>%
    unique() %>%
    as.character()
  
  # ---- 2) Check existence in dataset ----
  exists_flag <- req_vars %in% colnames(dataset)
  
  check_df <- data.frame(
    Variable_Name = req_vars,
    Exists_in_dataset = exists_flag,
    stringsAsFactors = FALSE
  )
  
  if (verbose) {
    print(check_df)
    message("---- Summary ----")
    message("Requested: ", length(req_vars),
            " | Exists: ", sum(exists_flag),
            " | Missing: ", sum(!exists_flag))
    if (sum(!exists_flag) > 0) {
      message("Missing variables: ", paste(req_vars[!exists_flag], collapse = ", "))
    }
  }
  
  # ---- 3) Select IDs + valid vars ----
  valid_vars <- check_df %>%
    dplyr::filter(.data$Exists_in_dataset) %>%
    dplyr::pull(.data$Variable_Name) %>%
    unique()
  
  # Keep only ID vars that exist in dataset (avoid select errors)
  id_exist <- id_vars[id_vars %in% colnames(dataset)]
  id_missing <- setdiff(id_vars, id_exist)
  
  if (verbose && length(id_missing) > 0) {
    message("Warning: These id_vars are not in dataset and will be ignored: ",
            paste(id_missing, collapse = ", "))
  }
  
  selected_df <- dataset %>%
    dplyr::select(dplyr::all_of(id_exist), dplyr::all_of(valid_vars))
  
  # ---- Optional: rename ID columns (safe version) ----
  # rename_ids: named vector, new_name = old_name
  if (!is.null(rename_ids) && length(rename_ids) > 0) {
    
    # keep only renames where old_name exists in selected_df
    rename_ids_valid <- rename_ids[
      unname(rename_ids) %in% colnames(selected_df)
    ]
    
    if (length(rename_ids_valid) > 0) {
      selected_df <- dplyr::rename(
        selected_df,
        !!!rename_ids_valid
      )
    }
  }
  
  # ---- Return objects ----
  list(
    selected = selected_df,
    check = check_df,
    missing = req_vars[!exists_flag],
    valid = valid_vars,
    id_missing = id_missing
  )
}

## 3.1 Prepare datasets by visit -------------------------------------------

birth_certificate_prep <- prep_requested_vars_by_visit(
  variables_df = variables,
  dataset = birth_certificate,
  visit_value = "Birth Certificate",  
  id_vars = c("sampleid", "march_id"),
  rename_ids = c(mom_id = "march_id", child_id = "sampleid"),
  verbose = TRUE
)

birth_certificate_selected_df <- birth_certificate_prep$selected

prenatal1_prep <- prep_requested_vars_by_visit(
  variables_df = variables,
  dataset = prenatal1,
  visit_value = "Prenatal 1",  
  id_vars = c("SAMPLEID"),
  rename_ids = c(mom_id = "SAMPLEID"),
  verbose = TRUE
)

prenatal1_selected_df <- prenatal1_prep$selected

postnatal_3month_IW_prep <- prep_requested_vars_by_visit(
  variables_df = variables,
  dataset = prenatal_3month_IW,
  visit_value = "3 Month",  
  id_vars = c("SAMPLEID"),
  rename_ids = c(child_id = "SAMPLEID"),
  verbose = TRUE
)

postnatal_3month_IW_selected_df <- postnatal_3month_IW_prep$selected

postnatal_3month_prior_IW_prep <- prep_requested_vars_by_visit(
  variables_df = variables,
  dataset = prenatal_3month_prior_IW,
  visit_value = "3 Month",  
  id_vars = c("SAMPLEID"),
  rename_ids = c(child_id = "SAMPLEID"),
  verbose = TRUE
)

postnatal_3month_prior_IW_selected_df <- postnatal_3month_prior_IW_prep$selected

postnatal_3month_selected_df <- bind_rows(
  postnatal_3month_IW_selected_df %>% mutate(INSURANCE_MOTHER = as.character(INSURANCE_MOTHER)),
  postnatal_3month_prior_IW_selected_df
)


## 3.2 Code Derived: Plurality ---------------------------------------------

# step 1: generate mom-child crosswalk with march_id
plurality_map <- crosswalk_mothers %>%
  group_by(mom_id) %>%
  summarise(
    n_children = n_distinct(child_id),
    .groups = "drop"
  ) %>%
  mutate(
    Plurality = case_when(
      n_children == 1 ~ "Single",
      n_children == 2 ~ "Twin",
      n_children == 3 ~ "Triplet",
      n_children >= 4 ~ "Multiple",
      TRUE ~ NA_character_
    )
  )

# step 2: add plurality info to crosswalk_mothers
crosswalk_mothers <- crosswalk_mothers %>%
  left_join(
    plurality_map %>% select(mom_id, Plurality),
    by = "mom_id"
  )

## 3.3 Code Derived: Mother_Race -------------------------------------------

mom_race <- read.csv("Z:/ECHO/CHARM/Data/Code Derived/Race_Ethnicity/MARCH_Race_Mom_breakdown.csv")

mom_race_select <- mom_race %>%
  select(mom_id = march_id, Mother_Race = Race)


## 3.4 Code Derived: maternal_age_birth ------------------------------------

maternal_age <- read.csv("Z:/ECHO/CHARM/Data/Code Derived/Ages/maternal_age_birth.csv")

maternal_age_select <- maternal_age %>%
  select(mom_id = march_id, maternal_age_birth = maternal_age_birth)


## 3.5 Code Derived: final_income ------------------------------------------

income <- read.csv("Z:/ECHO/CHARM/Data/Code Derived/Income/INCOME_ALL.csv")

income_select <- income %>%
  select(mom_id = march_id, child_id, final_income) %>%
  distinct(mom_id, child_id, .keep_all = TRUE)


## 3.6 Code Derived: MOMZIP ------------------------------------------------

urban <-read.csv("Z:/ECHO/CHARM/Data/Code Derived/Urban&Rural/urban_rural_final.csv")

urban_select <- urban %>%
  select(mom_id = march_id, child_id, UR2KX)


## 3.7 Code Derived: GA_Birth ----------------------------------------------

lmp_ga <- read.csv("Z:/ECHO/CHARM/Data/Code Derived/LMP/LMP_GA_Data.csv") 

lmp_ga_selected <- lmp_ga%>%
  select(mom_id = march_id, child_id, GA_Birth)


## 3.8 Code Derived: Dietary Score -----------------------------------------

dietary <- read_xlsx("Z:/ECHO/CHARM/Data/Code Derived/PN Dietary/PhenX and DSQ scores combined.xlsx") %>%
  rename(
    mom_id = SAMPLEID
  )


# 4. Merge Datasets -------------------------------------------------------

# Step 1: ID list
final_merged <- crosswalk_mothers %>%
  
  # Step 2: Birth Certificate, mom and child level
  left_join(birth_certificate_selected_df, by = c("mom_id", "child_id")) %>%
  
  # Step 3: Prenatal 1, mom level
  left_join(prenatal1_selected_df, by = "mom_id") %>%
  
  # Step 4: Postnatal 3 month（mother level）
  left_join(postnatal_3month_selected_df, by = "child_id") %>%
  
  # Step 5: Mother_Race, mother level
  left_join(mom_race_select, by = "mom_id") %>%
  
  # Step 6: Maternal age at birth, mother level
  left_join(maternal_age_select, by = "mom_id") %>%
  
  # Step 7: Final income, mother-child level）
  left_join(income_select, by = c("mom_id", "child_id")) %>%
  
  # Step 8: UR2KX, mother-child level）
  left_join(urban_select, by = c("mom_id", "child_id")) %>%
  
  # Step 9: GA_Birth, mother-child level）
  left_join(lmp_ga_selected, by = c("mom_id", "child_id")) %>%
  
  # Step 10: Dietary Score, mother level）
  left_join(dietary, by = "mom_id")



# 5. Double check requested variables (Quality Assurance) ---------------------

requested_vars <- variables$`Variable Name`

# check all if the requested variables are in final_merged
check_vars <- requested_vars %in% colnames(final_merged)

# generate results table
variable_check <- data.frame(
  Variable_Name = requested_vars,
  Exists_in_final_merged = check_vars
)

# check missing variables
missing_vars <- variable_check %>%
  filter(!Exists_in_final_merged)

print(variable_check)   
print(missing_vars)     

# 6. Save dataset ---------------------------------------------------------

library(openxlsx)

# Create a new workbook
wb <- createWorkbook()

# Add data sheet
addWorksheet(wb, "Data")
writeData(wb, "Data", final_merged, na.string = "NA")

# Add notes sheet
notes <- c(
  "1. This dataset was prepared on January 26, 2026 by Jiahe Tian.",
  "2. MOMZIP is not included because it was used internally to derive the urban–rural classification. Please refer to UR2KX for urban–rural status (U = Urban, R = Rural).",
  "3. UR2KX is a derived variable indicating urban–rural classification based on residential information.",
  "4. MOMSMOKE reflects prenatal smoking status during pregnancy only. For smoking status before and during pregnancy, please refer to CIGARETTE_SMOKING, SMOK_NUM, and SMOK_PREGNUM.",
  "5. FRUIT and GREENSALAD variables are not included. Dietary Scores are provided to harmonize dietary intake information.",
  "6. Blank cells in the dataset indicate missing values (NA)."
)

addWorksheet(wb, "Notes")
writeData(wb, "Notes", notes)

# Save
saveWorkbook(wb, "Z:/ECHO/CHARM/Data/Data Pulls/Afroj/Afroj_01122026.xlsx", overwrite = TRUE)
