# 1. Load library ---------------------------------------------------------

library(tidyverse)

# 2. Import Data ----------------------------------------------------------

## 2.1 Set up file paths ---------------------------------------------------

# Define file paths
# Detect OS
if (Sys.info()["sysname"] == "Windows") {
  BASE_PATH <- "Z:/ECHO/CHARM"
} else {
  BASE_PATH <- "/Volumes/Groups/ECHO/CHARM"
}

arch_bc_path  <- file.path(BASE_PATH, "Data/ECHO 1/MDHHS Data/Birth_Certificate/ARCH_BC_Combined.csv")
march_bc_path <- file.path(BASE_PATH, "Data/ECHO 1/MDHHS Data/Birth_Certificate/MARCH_BC_FinalUpd.csv")

# Child medical records
cbmra_path <- file.path(BASE_PATH,'Data/ECHO 1/Medical Record/Final MRA MARCH/Child MRA/REDCap_ECHO1_ess_hhx_cbmra.csv')

# urban and rural info
march_ur_path <- file.path(BASE_PATH, 'Data/Code Derived/Geoinformation/Urban_Rural_Info.csv')
arch_ur_path <- file.path(BASE_PATH, 'Data/Website/ARCH Descriptive/UA_MANUAL.csv')

# weight gain
arch_weight_gain_path <- file.path(BASE_PATH,'Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_prg_tpwtgsr.csv')
march_weight_gain_path <- file.path(BASE_PATH,'Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_prg_tpwtgsr.csv')

# Gestational age data from LMP, MARCH only
march_ga_path <- file.path(
  BASE_PATH,
  "Data/Code Derived/LMP/LMP_GA_Data.csv"
)

# cross walk
cross_walk_path <- file.path(BASE_PATH, 'Data/Miscellaneous/Global Crosswalk/global_crosswalk.xlsx')

# 2.2 Read data files -----------------------------------------------------

# Read raw birth certificate data and keep them unchanged
arch_bc_raw <- read_csv(arch_bc_path, show_col_types = FALSE)
march_bc_raw <- read_csv(march_bc_path, show_col_types = FALSE)

# Child medical records
cbmra <- read_csv(cbmra_path, show_col_types = FALSE)

# Urban/rural info
march_ur <- read_csv(march_ur_path, show_col_types = FALSE)
arch_ur <- read_csv(arch_ur_path, show_col_types = FALSE)

# Weight gain
arch_weight_gain <- read_csv(arch_weight_gain_path, show_col_types = FALSE)
march_weight_gain <- read_csv(march_weight_gain_path, show_col_types = FALSE)

# MARCH gestational age data from LMP
march_ga <- read_csv(march_ga_path, show_col_types = FALSE)

# Crosswalk
cross_walk <- readxl::read_excel(cross_walk_path)

# 3. Data Cleaning --------------------------------------------------------

# 3.0 Select birth certificate variables ---------------------------------

# Variables needed from birth certificate data
bc_vars_keep <- c(
  # IDs
  "arch_id",
  "sampleid",
  "march_id",
  
  # Delivery route / outcome
  "MD_FINAL_ROUTE",
  
  # Date of birth variables for maternal age
  "BXYEAR",
  "BXMONTH",
  "BXDAY",
  "MOMBXYR",
  "MOMBXMO",
  "MOMBXDAY",
  
  # BMI
  "BMI",
  
  # Singleton / multiple birth
  "PLURALITY",
  
  # Prior birth / previous children
  "LASTLVBXYR",
  "LASTLVBXMO",
  "LASTLVBXDY",
  "NOWLIVING",
  "NOWDEAD",
  "BORNDEAD"
  
  # # Optional: previous C-section, useful for C-section analysis
  # "RF_PREV_CSEC",
  # "RF_NumbPREVCSEC",
  # 
  # # Optional: pregnancy risk factors
  # "RF_PPREG_DIAB",
  # "RF_GEST_DIAB",
  # "RF_PPREG_HYPER",
  # "RF_GEST_HYPER",
  # "RF_HYP_ECLAMP",
  # 
  # # Optional: demographic/social covariates
  # "MOMRACE",
  # "MOMHISP",
  # "MOMRACHISP",
  # "BRIDGEMOMRACE",
  # "MOMEDUC",
  # "MARITALSTATUS",
  # "PAYSOURCE"
)

# Create working datasets from raw birth certificate data
# Keep only variables that exist in each dataset
arch_bc_work <- arch_bc_raw %>%
  select(any_of(bc_vars_keep))

march_bc_work <- march_bc_raw %>%
  select(any_of(bc_vars_keep))

## 3.1 Convert ID variables ------------------------------------------------

arch_bc_work <- arch_bc_work %>%
  mutate(
    arch_id = as.character(arch_id)
  )

march_bc_work <- march_bc_work %>%
  mutate(
    sampleid = as.character(sampleid),
    march_id = as.character(march_id)
  )

## 3.2 Cesarean recode -----------------------------------------------------

route_levels <- c(
  "1" = "Vaginal/Spontaneous",
  "2" = "Vaginal/Forceps",
  "3" = "Vaginal/Vacuum",
  "4" = "Cesarean",
  "9" = "Unknown"
)

add_delivery_vars <- function(df) {
  
  df %>%
    mutate(
      # Recode final delivery route
      MD_FINAL_ROUTE = recode(as.character(MD_FINAL_ROUTE), !!!route_levels),
      MD_FINAL_ROUTE = factor(
        MD_FINAL_ROUTE,
        levels = route_levels
      ),
      
      # Create delivery binary outcome
      delivery_binary = case_when(
        MD_FINAL_ROUTE == "Cesarean" ~ "C-section",
        MD_FINAL_ROUTE %in% c(
          "Vaginal/Spontaneous",
          "Vaginal/Forceps",
          "Vaginal/Vacuum"
        ) ~ "Vaginal",
        TRUE ~ NA_character_
      )
    )
}

arch_bc_work <- add_delivery_vars(arch_bc_work)
march_bc_work <- add_delivery_vars(march_bc_work)

# 3.3 ARCH Urban/Rural ----------------------------------------------------

# UACE10 / UR10 from ARCH birth certificate
# Birth certificate is child-level, but arch_id is mom-level
ur_arch <- arch_bc_raw %>%
  select(arch_id, MOMZIP, RES_ZIPCODE, UR10, UACE10, CENSUS) %>%
  mutate(
    arch_id = as.character(arch_id)
  )

# Manual updated UACE10 code for ARCH
MANUAL_UA_CODE <- arch_ur %>%
  select(arch_id, UACE10)

ur_arch_clean <- ur_arch %>%
  mutate(
    arch_id = as.character(arch_id),
    UACE10 = as.character(UACE10),
    UR10 = as.character(UR10)
  )

MANUAL_UA_CODE_clean <- MANUAL_UA_CODE %>%
  mutate(
    arch_id = as.character(arch_id),
    UACE10 = as.character(UACE10)
  )

# Check duplicated arch_id in birth certificate UACE10
ur_arch_uace_check <- ur_arch_clean %>%
  group_by(arch_id) %>%
  summarise(
    n_records = n(),
    n_unique_UACE10 = n_distinct(UACE10, na.rm = FALSE),
    UACE10_values = paste(unique(UACE10), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(n_records > 1) %>%
  arrange(desc(n_unique_UACE10), arch_id)

# Check duplicated arch_id in manual UACE10
manual_uace_check <- MANUAL_UA_CODE_clean %>%
  group_by(arch_id) %>%
  summarise(
    n_records = n(),
    n_unique_UACE10 = n_distinct(UACE10, na.rm = FALSE),
    UACE10_values = paste(unique(UACE10), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(n_records > 1) %>%
  arrange(desc(n_unique_UACE10), arch_id)

# Deduplicate birth certificate UACE10 / UR10 to mom-level
ur_arch_clean_dedup <- ur_arch_clean %>%
  group_by(arch_id) %>%
  summarise(
    # Keep non-missing UACE10 first; if all are missing, keep NA
    UACE10 = if_else(
      any(!is.na(UACE10)),
      first(na.omit(UACE10)),
      NA_character_
    ),
    
    # Keep non-missing UR10 first; if all are missing, keep NA
    UR10 = if_else(
      any(!is.na(UR10)),
      first(na.omit(UR10)),
      NA_character_
    ),
    
    .groups = "drop"
  )

# Deduplicate manual UACE10 to mom-level
MANUAL_UA_CODE_clean_dedup <- MANUAL_UA_CODE_clean %>%
  group_by(arch_id) %>%
  summarise(
    # Keep non-missing UACE10 first; if all are missing, keep NA
    UACE10 = if_else(
      any(!is.na(UACE10)),
      first(na.omit(UACE10)),
      NA_character_
    ),
    .groups = "drop"
  )

# Merge manual UACE10 with birth certificate UACE10
UACE <- ur_arch_clean_dedup %>%
  left_join(
    MANUAL_UA_CODE_clean_dedup,
    by = "arch_id",
    suffix = c("", "_manual")
  ) %>%
  mutate(
    # Use manual UACE10 first, then birth certificate UACE10
    UACE10 = coalesce(UACE10_manual, UACE10),
    
    # If UACE10 is R or missing, classify UR10 as R; otherwise classify as U
    UR10 = if_else(
      UACE10 == "R" | is.na(UACE10),
      "R",
      "U"
    )
  ) %>%
  select(-UACE10_manual)

table(UACE$UR10, useNA = "ifany")

## 3.4 Maternal age, BMI, Singleton ---------------------------------------

add_basic_bc_vars <- function(df) {
  
  df %>%
    mutate(
      # Create child and mother date of birth
      child_dob = make_date(BXYEAR, BXMONTH, BXDAY),
      mom_dob = make_date(MOMBXYR, MOMBXMO, MOMBXDAY),
      
      # Calculate maternal age at delivery
      maternal_age = time_length(interval(mom_dob, child_dob), "years"),
      
      # Treat BMI = 999 as missing
      BMI = na_if(BMI, 999),
      
      # Create singleton indicator
      Singleton = case_when(
        PLURALITY == 1 ~ 1,
        PLURALITY > 1 ~ 0,
        TRUE ~ NA_real_
      )
    )
}

arch_bc_work <- add_basic_bc_vars(arch_bc_work) %>%
  filter(
    maternal_age >= 18
  )

march_bc_work <- add_basic_bc_vars(march_bc_work)

## 3.5 Prior birth variables -----------------------------------------------

add_birth_order_vars <- function(df) {
  
  df %>%
    mutate(
      # Identify whether the mother had any prior live birth
      # LASTLVBXYR == 0 means no prior live birth
      has_prior_live_birth = case_when(
        is.na(LASTLVBXYR) ~ NA,
        LASTLVBXYR == 0 ~ FALSE,
        LASTLVBXYR > 0 ~ TRUE
      ),
      
      # Count how many previous children variables are non-missing
      n_previous_children_vars_nonmissing = rowSums(
        !is.na(across(c(NOWLIVING, NOWDEAD, BORNDEAD)))
      ),
      
      # Calculate total number of previous children
      previous_children_total = if_else(
        n_previous_children_vars_nonmissing == 0,
        NA_real_,
        rowSums(across(c(NOWLIVING, NOWDEAD, BORNDEAD)), na.rm = TRUE)
      ),
      
      # Identify whether the mother had any previous children
      had_previous_children = case_when(
        is.na(previous_children_total) ~ NA,
        previous_children_total > 0 ~ TRUE,
        previous_children_total == 0 ~ FALSE
      ),
      
      # Classify birth order based on previous children count
      birth_order_group = case_when(
        is.na(previous_children_total) ~ NA_character_,
        previous_children_total == 0 ~ "First birth",
        previous_children_total == 1 ~ "Second birth",
        previous_children_total >= 2 ~ "Third or later birth"
      ),
      
      # Create second birth indicator
      is_second_birth = case_when(
        is.na(previous_children_total) ~ NA,
        previous_children_total == 1 ~ TRUE,
        TRUE ~ FALSE
      )
    )
}

arch_bc_work <- add_birth_order_vars(arch_bc_work)
march_bc_work <- add_birth_order_vars(march_bc_work)

## 3.6 Weight gain info ----------------------------------------------------

clean_weight_gain <- function(df, cross_walk) {
  
  cross_walk_mom <- cross_walk %>%
    transmute(
      MomID = as.character(MomID),
      Mom_ECHO_ID = as.character(Mom_ECHO_ID)
    ) %>%
    filter(!is.na(Mom_ECHO_ID), Mom_ECHO_ID != "") %>%
    distinct(Mom_ECHO_ID, .keep_all = TRUE)
  
  df %>%
    mutate(
      # Convert ID to character for joining
      participantid = as.character(participantid),
      
      # Convert variables to numeric
      tpwtgsr_1 = as.numeric(tpwtgsr_1),
      tpwtgsr_3 = as.numeric(tpwtgsr_3),
      tpwtgsr_3a_lb = as.numeric(tpwtgsr_3a_lb),
      
      # Treat refused / don't know as missing
      tpwtgsr_3 = if_else(
        tpwtgsr_3 %in% c(-7, -8),
        NA_real_,
        tpwtgsr_3
      ),
      
      # Create signed pregnancy weight change in pounds
      pregnancy_weight_change_lb = case_when(
        tpwtgsr_3 == 1 ~ tpwtgsr_3a_lb,
        tpwtgsr_3 == 2 ~ -tpwtgsr_3a_lb,
        is.na(tpwtgsr_3) ~ NA_real_,
        TRUE ~ NA_real_
      )
    ) %>%
    rename(
      mom_echo_id = participantid,
      gestational_age_weeks = tpwtgsr_1
    ) %>%
    left_join(
      cross_walk_mom,
      by = c("mom_echo_id" = "Mom_ECHO_ID")
    ) %>%
    select(
      MomID,
      mom_echo_id,
      gestational_age_weeks,
      pregnancy_weight_change_lb,
      tpwtgsr_3,
      tpwtgsr_3a_lb
    )
}

arch_weight_gain_clean <- clean_weight_gain(arch_weight_gain, cross_walk)
march_weight_gain_clean <- clean_weight_gain(march_weight_gain, cross_walk)

# Clean MARCH GA data
march_ga_clean <- march_ga %>%
  transmute(
    march_id = as.character(march_id),
    child_id = as.character(child_id),
    GA_Birth = as.numeric(GA_Birth)
  ) %>%
  filter(
    !is.na(march_id),
    !is.na(child_id)
  ) %>%
  distinct(
    march_id,
    child_id,
    .keep_all = TRUE
  )

## 3.7 Merge datasets ------------------------------------------------------

arch_bc_model <- arch_bc_work %>%
  filter(
    Singleton == 1
  ) %>%
  left_join(
    UACE %>% select(arch_id, UR10),
    by = "arch_id"
  ) %>%
  left_join(
    arch_weight_gain_clean,
    by = c("arch_id" = "MomID")
  )

march_bc_model <- march_bc_work %>%
  filter(
    Singleton == 1
  ) %>%
  left_join(
    march_ur %>%
      select(march_id, child_id, UR20),
    by = c(
      "sampleid" = "child_id",
      "march_id" = "march_id"
    )
  ) %>%
  left_join(
    march_weight_gain_clean,
    by = c("march_id" = "MomID")
  ) %>%
  left_join(
    march_ga_clean,
    by = c(
      "march_id" = "march_id",
      "sampleid" = "child_id"
    )
  ) %>%
  mutate(
    # Track final gestational age source
    gestational_age_source = case_when(
      !is.na(GA_Birth) ~ "LMP_GA_Data",
      is.na(GA_Birth) & !is.na(gestational_age_weeks) ~ "TPWTGSR",
      TRUE ~ NA_character_
    ),
    
    # Use GA_Birth as the primary source, then TPWTGSR gestational age
    gestational_age_weeks = coalesce(
      GA_Birth,
      gestational_age_weeks
    )
  ) %>%
  select(
    -GA_Birth
  )

# 4. Create final analysis datasets ---------------------------------------

arch_data <- arch_bc_model %>%
  select(
    arch_id,
    MD_FINAL_ROUTE,
    delivery_binary,
    UR10,
    maternal_age,
    BMI,
    has_prior_live_birth,
    had_previous_children,
    previous_children_total,
    birth_order_group,
    is_second_birth,
    gestational_age_weeks,
    pregnancy_weight_change_lb
    # ,
    # RF_PREV_CSEC,
    # RF_NumbPREVCSEC,
    # RF_PPREG_DIAB,
    # RF_GEST_DIAB,
    # RF_PPREG_HYPER,
    # RF_GEST_HYPER,
    # RF_HYP_ECLAMP,
    # MOMRACHISP,
    # MOMEDUC,
    # MARITALSTATUS,
    # PAYSOURCE
  )

march_data <- march_bc_model %>%
  select(
    march_id,
    sampleid,
    MD_FINAL_ROUTE,
    delivery_binary,
    UR20,
    maternal_age,
    BMI,
    Singleton,
    has_prior_live_birth,
    had_previous_children,
    previous_children_total,
    birth_order_group,
    is_second_birth,
    gestational_age_weeks,
    pregnancy_weight_change_lb
    # ,
    # RF_PREV_CSEC,
    # RF_NumbPREVCSEC,
    # RF_PPREG_DIAB,
    # RF_GEST_DIAB,
    # RF_PPREG_HYPER,
    # RF_GEST_HYPER,
    # RF_HYP_ECLAMP,
    # MOMRACHISP,
    # MOMEDUC,
    # MARITALSTATUS,
    # PAYSOURCE
  )

# 5. Save processed datasets ---------------------------------------------

# Define output folder
output_dir <- "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/Data-Manager/Research/C-section/Data/Processed"

# Define output file path
output_file <- file.path(output_dir, "c_section_processed_data.xlsx")

# Save ARCH and MARCH datasets into different sheets
openxlsx::write.xlsx(
  list(
    ARCH = arch_data,
    MARCH = march_data
  ),
  file = output_file,
  overwrite = TRUE
)
