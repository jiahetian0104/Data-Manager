# Load packages (organized and deduplicated)
library(tidyverse)    # includes dplyr, tidyr, readr, stringr
library(readxl)

library(openxlsx) # save data set



# 0. CONFIGURATION - Centralized path management --------------------------


# Base paths - easy to modify for different environments
BASE_PATH <- "Z:/ECHO/CHARM/Data"

PATHS <- list(
  echo1 = file.path(BASE_PATH, "ECHO 1"),
  website = file.path(BASE_PATH, "Website"),
  code_derived = file.path(BASE_PATH, "Code Derived"),
  miscellaneous = file.path(BASE_PATH, "Miscellaneous"),
  ripple = file.path(BASE_PATH, "Ripple/Data Upload/2026")
)

# Helper function for constructing paths
get_path <- function(category, ...) {
  file.path(PATHS[[category]], ...)
}



# 1. HELPER FUNCTIONS -----------------------------------------------------


# clean ids by trimming whitespace
clean_id <- function(x) {
  x %>% as.character() %>% stringr::str_trim()
}

# normalize ECHO IDs to first n characters
normalize_echo_id <- function(x, n = 11) {
  x %>%
    clean_id() %>%
    stringr::str_sub(1, n)
}

# Extract rightmost n characters
substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

# Add leading zeros to make string length n
pad_zeros <- function(x, n) {
  ifelse(nchar(x) < n, 
         paste0(strrep("0", n - nchar(x)), x), 
         x)
}

# Calculate age from birthdate
calculate_age <- function(birthdate, reference_date = Sys.Date(), date_format = NULL) {
  # 
  if (!is.null(date_format)) {
    birthdate <- as.Date(birthdate, format = date_format)
  } else {
    # 
    birthdate <- as.Date(birthdate)
  }
  
  round(as.numeric(reference_date - birthdate) / 365.25, 3)
}

# Parse date from separate month/day/year columns
parse_date_from_parts <- function(month, day, year) {
  as.Date(paste(year, month, day, sep = "-"))
}

# Clean merged data by removing all-NA rows (except ID columns)
remove_empty_rows <- function(df, id_cols) {
  df %>% filter(!if_all(-all_of(id_cols), is.na))
}

# Pick the first non-missing and non-placeholder value by priority
pick_first_valid <- function(..., invalid = c("unknown", "other")) {
  vals <- list(...)
  
  # Standardize: trim + lower for checking invalid tokens
  vals_clean <- lapply(vals, function(v) {
    v_chr <- as.character(v)
    v_std <- str_trim(v_chr)
    v_low <- str_to_lower(v_std)
    
    # Treat invalid tokens as NA
    v_std[is.na(v_std) | v_std == "" | v_low %in% invalid] <- NA_character_
    v_std
  })
  
  # Return first non-NA by priority
  do.call(coalesce, vals_clean)
}

# Record which source provided the first valid value
pick_source_valid <- function(..., sources, invalid = c("unknown", "other")) {
  vals <- list(...)
  
  # Create a logical matrix: is this value valid?
  valid_mat <- lapply(vals, function(v) {
    v_chr <- as.character(v)
    v_std <- str_trim(v_chr)
    v_low <- str_to_lower(v_std)
    !(is.na(v_std) | v_std == "" | v_low %in% invalid)
  })
  valid_mat <- do.call(cbind, valid_mat)
  
  # First TRUE index per row
  idx <- apply(valid_mat, 1, function(r) {
    hit <- which(r)
    if (length(hit) == 0) NA_integer_ else hit[1]
  })
  
  ifelse(is.na(idx), "missing", sources[idx])
}

# 2. Load data ------------------------------------------------------------

setwd("/Users/jack/Desktop/Data Transfer")

# cross walk
crosswalk_raw <- read_excel(
  "global_crosswalk.xlsx",
  na = c("", "NA"),
  sheet = 2
)


# Filter and clean mother-child crosswalk
crosswalk_mothers <- crosswalk_raw %>%
  filter(weighted == 1) %>%
  select(
    child_id = ChildID,
    march_id = MomID,
    child_echo_id = Child_ECHO_ID,
    mom_echo_id = Mom_ECHO_ID
  ) 

# birth certificate
birth_cert <- read_csv(
  "MARCH_BC_FinalUpd.csv"
) %>%
  rename(child_id = sampleid)
# prenatal data
prenatal_1 <- read_csv(
  "PRENATAL_1_SURVEY_mixedformats.csv"
) %>%
  rename(march_id = SAMPLEID)

prenatal_2 <- read_csv(
  "PRENATAL_2_SURVEY_mixedformats.csv"
) %>%
  rename(march_id = SAMPLEID)
# redcap data
srs2_pre <- read_csv("20231202190220_42_ess_cnh_srs2_pre.csv")

cbcl_pre <- read_csv("20231202190220_42_ess_cnh_cbcl_pre.csv")

dem_child <- read_csv("20231202190220_42_ess_dem_dem_c.csv") %>%
  rename(child_echo_id = participantid)

dem_birth <- read_csv("20231202190220_42_ess_dem_dem_b.csv")

dem_caregiver <- read_csv("20231202190220_42_ess_dem_dem_cg.csv") %>%
  rename(child_echo_id = participantid)

cbmra <- read_csv("20231202190220_42_ess_hhx_cbmra.csv") %>%
  rename(child_echo_id = participantid)

mmra <- read_csv("20231202190220_42_ess_prg_mmra.csv") %>%
  rename(mom_echo_id = participantid)

hcexp <- read_csv("20231202190220_42_ess_bpe_hcexp_r.csv")

# Participant registration

registration_raw <- read_csv("ParticipantRegistration_Export.csv")

# Process mother registration
registration_mothers <- registration_raw %>%
  filter(
    ParticipantType == "P",
    str_starts(CohortParticipantId, "P")
  ) %>%
  rename(march_id = CohortParticipantId) %>%
  mutate(
    birth_date_mom = parse_date_from_parts(
      DateOfBirth_Month, 
      DateOfBirth_Day, 
      DateOfBirth_Year
    )
  )

# Process child registration
registration_children <- registration_raw %>%
  filter(
    ParticipantType == "C",
    str_sub(CohortParticipantId, 6, 6) == "M"
  ) %>%
  rename(child_id = CohortParticipantId) %>%
  mutate(
    birth_date_child = parse_date_from_parts(
      DateOfBirth_Month,
      DateOfBirth_Day,
      DateOfBirth_Year
    )
  )

# survey tracking
survey_track_mothers <- read_csv("march_mother_survey_track.csv")

survey_track_children <- read_csv("march_child_survey_track.csv") %>%
  rename(
    child_id = sampleid,
    march_id = march_id_fk
  )
# Geographic data
urban_data   <- read_csv("URBAN_CODE_DATA.csv")
urban_manual <- read_csv("UA_MANUAL_ZIP.csv")
urban_all    <- read_csv("UA_ALL.csv", na = c("", "NA"))

march_address <- read_csv("march_address_final.csv")

march_consent <- read_csv("march_all_contact.csv") %>%
  select(march_id = SAMPLEID, address)


# Sex
sex_coded <- read_csv("SEX_CODED.csv") %>%
  rename(child_id = SAMPLEID)

# Ripple
ripple_young <- read_csv("2025_6to35mo.csv") %>%
  rename(
    child_echo_id = globalId,
    child_id = customId
  ) %>%
  mutate(
    child_echo_id = str_sub(child_echo_id, 1, 11)
  )

ripple_old <- read_csv("2025_3to17yr.csv") %>%
  rename(
    child_echo_id = globalId,
    child_id = customId
  ) %>%
  mutate(
    child_echo_id = str_sub(child_echo_id, 1, 11)
  )

# Race
race_coded <- read_csv("MARCH_RACE_ALL.csv")

# County crosswalk
county_crosswalk <- read_excel("CountyCrosswalk.xlsx") %>%
  rename(resco = RESMCD) %>%
  mutate(resco = pad_zeros(resco, 2))

# Maternal age
maternal_age_birth <- read_csv("maternal_age_birth.csv")




# 2. LOAD CROSSWALK DATA -------------------------------------------------


# MARCH Participants
crosswalk_raw <- read_excel(
  get_path("miscellaneous", "Global Crosswalk/global_crosswalk.xlsx"),
  na = c("", "NA"),
  sheet = 2
)

# Filter and clean mother-child crosswalk
crosswalk_mothers <- crosswalk_raw %>%
  filter(weighted == 1) %>%
  select(
    child_id = ChildID,
    march_id = MomID,
    child_echo_id = Child_ECHO_ID,
    mom_echo_id = Mom_ECHO_ID
  ) 


# crosswalk_mothers <- crosswalk_mothers %>%
#   mutate(
#     march_id = clean_id(march_id),
#     child_id = clean_id(child_id),
#     child_echo_id = normalize_echo_id(child_echo_id),
#     mom_echo_id = clean_id(mom_echo_id)
#   )

# Extract ID vectors for filtering
mom_ids <- crosswalk_mothers$march_id
child_ids <- crosswalk_mothers$child_id


# 3. LOAD SOURCE DATA --------------------------------------------------------


# Birth certificate data
birth_cert <- read_csv(
  get_path("echo1", "MDHHS Data/Birth_Certificate/MARCH_BC_FinalUpd.csv")
) %>%
  rename(child_id = sampleid)

# Prenatal surveys
prenatal_1 <- read_csv(
  get_path("echo1", "SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_1/Data_Delivery/Prenatal_Survey1/PRENATAL_1_SURVEY_mixedformats.csv")
) %>%
  rename(march_id = SAMPLEID)

prenatal_2 <- read_csv(
  get_path("echo1", "SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_1/Data_Delivery/Prenatal_Survey2/PRENATAL_2_SURVEY_mixedformats.csv")
) %>%
  rename(march_id = SAMPLEID)

# RedCap data
srs2_pre <- read_csv(
  get_path("echo1", "RedCap/MARCH/20231202190220_42_ess_cnh_srs2_pre.csv")
)

cbcl_pre <- read_csv(
  get_path("echo1", "RedCap/MARCH/20231202190220_42_ess_cnh_cbcl_pre.csv")
)

dem_child <- read_csv(
  get_path("echo1", "RedCap/MARCH/20231202190220_42_ess_dem_dem_c.csv")
) %>%
  rename(child_echo_id = participantid)

dem_birth <- read_csv(
  get_path("echo1", "RedCap/MARCH/20231202190220_42_ess_dem_dem_b.csv")
) %>%
  rename(child_echo_id = participantid)

dem_caregiver <- read_csv(
  get_path("echo1", "RedCap/MARCH/20231202190220_42_ess_dem_dem_cg.csv")
) %>%
  rename(child_echo_id = participantid)

cbmra <- read_csv(
  get_path("echo1", "RedCap/MARCH/20231202190220_42_ess_hhx_cbmra.csv")
) %>%
  rename(child_echo_id = participantid)

mmra <- read_csv(
  get_path("echo1", "RedCap/MARCH/20231202190220_42_ess_prg_mmra.csv")
) %>%
  rename(mom_echo_id = participantid)

hcexp <- read_csv(
  get_path("echo1", "RedCap/MARCH/20231202190220_42_ess_bpe_hcexp_r.csv")
)

# Participant registration
registration_raw <- read_csv(
  get_path("echo1", "Participant Registration/ParticipantRegistration_Export.csv")
)

# Process mother registration
registration_mothers <- registration_raw %>%
  filter(
    ParticipantType == "P",
    str_starts(CohortParticipantId, "P")
  ) %>%
  rename(march_id = CohortParticipantId) %>%
  mutate(
    birth_date_mom = parse_date_from_parts(
      DateOfBirth_Month, 
      DateOfBirth_Day, 
      DateOfBirth_Year
    )
  )

# Process child registration
registration_children <- registration_raw %>%
  filter(
    ParticipantType == "C",
    str_sub(CohortParticipantId, 6, 6) == "M"
  ) %>%
  rename(child_id = CohortParticipantId) %>%
  mutate(
    birth_date_child = parse_date_from_parts(
      DateOfBirth_Month,
      DateOfBirth_Day,
      DateOfBirth_Year
    )
  )

# Survey tracking
survey_track_mothers <- read_csv(
  get_path("echo1", "CHARM Database/march_mother_survey_track.csv")
)

survey_track_children <- read_csv(
  get_path("echo1", "CHARM Database/march_child_survey_track.csv")
) %>%
  rename(
    child_id = sampleid,
    march_id = march_id_fk
  )

# Geographic data
urban_data <- read_csv(
  get_path("code_derived", "Geocoding/URBAN_CODE_DATA.csv")
)

urban_manual <- read_csv(
  get_path("code_derived", "Geocoding/UA_MANUAL_ZIP.csv")
)

urban_all <- read_csv(
  get_path("code_derived", "Geocoding/UA_ALL.csv"),
  na = c("", "NA")
)

march_address <- read_csv(
  get_path("echo1", "Contact Information/march_address_upd.csv")
)

march_consent <- read_csv(
  get_path("echo1", "CHARM Database/march_all_contact.csv")
) %>%
  select(march_id = SAMPLEID, address)

# Sex coding
sex_coded <- read_csv(
  get_path("code_derived", "SEX/SEX_CODED.csv")
) %>%
  rename(child_id = SAMPLEID)

# Ripple data
ripple_young <- read_csv(get_path("ripple", "2025_6to35mo.csv")) %>%
  rename(
    child_echo_id = globalId,
    child_id = customId
  ) %>%
  mutate(
    child_echo_id = str_sub(child_echo_id, 1, 11)
  )

ripple_old <- read_csv(get_path("ripple", "2025_3to17yr.csv")) %>%
  rename(
    child_echo_id = globalId,
    child_id = customId
  ) %>%
  mutate(
    child_echo_id = str_sub(child_echo_id, 1, 11)
  )


# Race/ethnicity
race_coded <- read_csv(
  get_path("code_derived", "Race_Ethnicity/MARCH_RACE_ALL.csv")
)

# County crosswalk
county_crosswalk <- read_excel(
  file.path( BASE_PATH,"Data Pulls/Armstrong/Side Projects/CountyCrosswalk.xlsx")
) %>%
  rename(resco = RESMCD) %>%
  mutate(resco = pad_zeros(resco, 2))

# Maternal age at birth
maternal_age_birth <- read_csv(
  get_path("code_derived", "Ages/maternal_age_birth.csv")
)


# 4. Child Age ------------------------------------------------------------

# CALCULATE CURRENT AGE


# Function to calculate age from Ripple data
calculate_ripple_age <- function(df, age_col_name) {
  df %>%
    select(child_id, birthday) %>%
    mutate(
      !!age_col_name := calculate_age(
        as.Date(birthday, format = "%m/%d/%Y")
      )
    ) %>%
    select(child_id, !!age_col_name)
}

# Calculate ages from different sources
age_ripple_young <- calculate_ripple_age(ripple_young, "age_ripple_young")
age_ripple_old <- calculate_ripple_age(ripple_old, "age_ripple_old")

age_birth_cert <- birth_cert %>%
  select(child_id, BXYEAR, BXMONTH, BXDAY) %>%
  mutate(
    birth_date = parse_date_from_parts(BXMONTH, BXDAY, BXYEAR),
    age_birth_cert = calculate_age(birth_date)
  ) %>%
  select(child_id, age_birth_cert)

age_registration <- registration_children %>%
  select(child_id, birth_date_child) %>%
  mutate(age_registration = calculate_age(birth_date_child)) %>%
  select(child_id, age_registration)

age_dem_child <- dem_child %>%
  select(child_echo_id, dem_c_1) %>%
  mutate(age_dem_child = calculate_age(dem_c_1)) %>%
  select(child_echo_id, age_dem_child)

age_cbmra <- cbmra %>%
  select(child_echo_id, cbmra_c1a) %>%
  mutate(age_cbmra = calculate_age(cbmra_c1a, date_format = "%m-%d-%Y")) %>%
  select(child_echo_id, age_cbmra)

age_survey_track <- survey_track_children %>%
  select(march_id, child_id, birth_date) %>%
  mutate(age_survey_track = calculate_age(birth_date, date_format = "%Y-%m-%d")) %>%
  select(march_id, child_id, age_survey_track)


# Merge all age sources
current_age_final <- crosswalk_mothers %>%
  left_join(age_birth_cert, by = "child_id") %>%
  left_join(age_registration, by = "child_id") %>%
  left_join(age_ripple_young, by = "child_id") %>%
  left_join(age_ripple_old, by = "child_id") %>%
  left_join(age_dem_child, by = "child_echo_id") %>%
  left_join(age_cbmra, by = "child_echo_id") %>%
  left_join(age_survey_track, by = c("march_id","child_id")) %>%
  mutate(
    # Prioritize sources (based on original code):
    # RIPPLE_Y > RIPPLE_O > BC > REG > DEM_C > CBMRA > ST_C
    curr_age = coalesce(
      age_ripple_young,    # CA_RIPPLE_Y (highest priority)
      age_ripple_old,      # CA_RIPPLE_O
      age_birth_cert,      # curr_ageBC
      age_registration,    # curr_ageREG
      age_dem_child,       # curr_ageDEM_C
      age_cbmra,          # curr_ageCBMRA
      age_survey_track    # curr_ageST_C (lowest priority)
    ),
    # Create age groups
    curr_age_group = case_when(
      curr_age < 1 ~ "<1 year",
      curr_age >= 1 & curr_age < 3 ~ "1-2 years",
      curr_age >= 3 & curr_age < 6 ~ "3-5 years",
      curr_age >= 6 & curr_age < 13 ~ "6-12 years",
      curr_age >= 13 ~ "13+ years",
      TRUE ~ NA_character_
    )
  ) %>%
  remove_empty_rows(c("march_id", "child_id")) %>%
  distinct(
    march_id, child_id, child_echo_id, mom_echo_id,
    curr_age, curr_age_group
  )

# There is one case for no current age: participant without child_id and child_echo_id

# Summary statistics
cat("Current Age Summary:\n")
summary(current_age_final$curr_age)
table(current_age_final$curr_age_group, useNA = "always")


# 5. Child Sex ------------------------------------------------------------

# 13 missings

# Create reusable sex recoding function
normalize_sex <- function(x) {
  x2 <- x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()
  
  case_when(
    x2 %in% c("1", "m", "male") ~ "M",
    x2 %in% c("2", "f", "female") ~ "F",
    x2 %in% c("other") ~ "Other",
    x2 %in% c("-8", "unknown", "", "na", "n/a") ~ NA_character_,
    TRUE ~ NA_character_
  )
}

# --- Source: SEX_CODED
sex_coded_clean <- sex_coded %>%
  transmute(
    march_id = clean_id(march_id),
    child_id = clean_id(child_id),
    sex_coded = normalize_sex(SEX_CODED)
  )

# --- Source: Birth Certificate
sex_birth_cert <- birth_cert %>%
  transmute(
    child_id = clean_id(child_id),
    sex_bc = normalize_sex(SEX)
  )

# --- Source: Registration
sex_registration <- registration_children %>%
  transmute(
    child_id = clean_id(child_id),
    sex_reg = normalize_sex(Sex)
  )

# --- Source: Survey track (RECODE!)
sex_survey_track <- survey_track_children %>%
  transmute(
    march_id = clean_id(march_id),
    child_id = clean_id(child_id),
    sex_track = normalize_sex(birth_sex)
  )

# --- Source: SRS2 / CBCL / CBMRA (normalize echo id!)
sex_srs2 <- srs2_pre %>%
  transmute(
    child_echo_id = normalize_echo_id(participantid),
    sex_srs2 = normalize_sex(srs2_pre_gender)
  )

sex_cbcl <- cbcl_pre %>%
  transmute(
    child_echo_id = normalize_echo_id(participantid),
    sex_cbcl = normalize_sex(cbcl_pre_gender)
  )

sex_cbmra <- cbmra %>%
  transmute(
    child_echo_id = normalize_echo_id(child_echo_id),
    sex_cbmra = normalize_sex(cbmra_c2)
  )

# --- Source: Ripple (normalize echo id!)
sex_ripple_young <- ripple_young %>%
  transmute(
    child_echo_id = normalize_echo_id(child_echo_id),
    sex_ripple_y = normalize_sex(sex)
  )

sex_ripple_old <- ripple_old %>%
  transmute(
    child_echo_id = normalize_echo_id(child_echo_id),
    sex_ripple_o = normalize_sex(sex)
  )

# --- Merge
sex_final <- crosswalk_mothers %>%
  left_join(sex_registration, by = "child_id") %>%
  left_join(sex_coded_clean, by = c("march_id", "child_id")) %>%
  left_join(sex_birth_cert, by = "child_id") %>%
  left_join(sex_survey_track, by = c("march_id", "child_id")) %>%
  left_join(sex_srs2, by = "child_echo_id") %>%
  left_join(sex_cbcl, by = "child_echo_id") %>%
  left_join(sex_cbmra, by = "child_echo_id") %>%
  left_join(sex_ripple_young, by = "child_echo_id") %>%
  left_join(sex_ripple_old, by = "child_echo_id") %>%
  mutate(
    # Priority: RIPPLE_O > RIPPLE_Y > SEX_CODED > BC > REG > CBMRA > TRACK > CBCL > SRS2
    sex_all = pick_first_valid(
      sex_ripple_o,
      sex_ripple_y,
      sex_coded,
      sex_bc,
      sex_reg,
      sex_cbmra,
      sex_track,
      sex_cbcl,
      sex_srs2,
      invalid = c("unknown","other")
    )
  ) %>%
  distinct(
    march_id, child_id, child_echo_id, mom_echo_id, sex_all
  ) %>%
  select(march_id, child_id, child_echo_id, sex_all)

# --- Audit missing
sex_missing_audit <- sex_final %>%
  left_join(
    sex_final %>% select(child_id, child_echo_id) ,
    by = "child_id"
  ) %>%
  mutate(missing = is.na(sex_all)) %>%
  filter(missing)

cat("\nSex Distribution:\n")
print(table(sex_final$sex_all, useNA = "always"))



# 6. Urban / Rural indicator ----------------------------------------------
# one missing

urban_all <- urban_all %>%
  mutate(
    march_id = clean_id(march_id),
    UA_CODE  = clean_id(UA_CODE)
  )


urban_final <- crosswalk_mothers %>%
  left_join(urban_all, by = "march_id") %>%
  mutate(
    # Standardize UA_CODE
    ua_code_clean = case_when(
      UA_CODE %in% c("R", "r") ~ "R",
      is.na(UA_CODE) | UA_CODE == "" ~ NA_character_,
      stringr::str_detect(UA_CODE, "^[0-9]+$") ~ stringr::str_pad(UA_CODE, width = 5, side = "left", pad = "0"),
      TRUE ~ NA_character_   # Non-numeric unexpected values -> NA
    ),
    # Classify urban/rural
    urban = case_when(
      ua_code_clean == "R" ~ "Rural",
      stringr::str_detect(ua_code_clean, "^[0-9]{5}$") ~ "Urban",
      TRUE ~ NA_character_
    ),
    # Flag join miss explicitly
    join_missed = is.na(UA_CODE)
  ) %>%
  distinct(
    march_id, child_id, child_echo_id, mom_echo_id, UA_CODE, ua_code_clean, urban, join_missed
  ) 

cat("\nUrban/Rural Distribution:\n")
print(table(urban_final$urban, useNA = "always"))
prop.table(table(urban_final$urban, useNA = "always"))


# 7. Geographic Heatmap ---------------------------------------------------

# 31 missing

# Extract ZIP codes from different sources
zip_birth_cert <- birth_cert %>%
  select(march_id, child_id, zip_bc = MOMZIP, resco = RESCO) %>%
  mutate(
    zip_bc = stringr::str_extract(as.character(zip_bc), "\\d{5}")
  )

zip_urban_data <- urban_data %>%
  transmute(
    march_id,
    zip_ua = stringr::str_extract(OUTPUT_ADDRESS, "\\d{5}(?=\\D*$)") %>%  # last 5 digits near end
      stringr::str_sub(1, 5)
  )


zip_manual <- urban_manual %>%
  transmute(
    march_id, child_id,
    zip_manual = stringr::str_extract(as.character(ZIP), "\\d{5}") %>%
      stringr::str_pad(5, "left", "0")
  )

# Merge all ZIP code sources
zip_final <- crosswalk_mothers %>%
  left_join(zip_birth_cert, by = c("march_id", "child_id")) %>%
  left_join(zip_urban_data, by = "march_id") %>%
  left_join(zip_manual, by = c("march_id", "child_id")) %>%
  mutate(
    zip_final  = coalesce(zip_bc, zip_ua, zip_manual),
    zip_source = case_when(
      !is.na(zip_bc) ~ "birth_cert",
      is.na(zip_bc) & !is.na(zip_ua) ~ "urban_data",
      is.na(zip_bc) & is.na(zip_ua) & !is.na(zip_manual) ~ "manual_entry",
      TRUE ~ "missing"
    )
  ) %>%
  distinct(march_id, child_id, child_echo_id, mom_echo_id, zip_final, zip_source)

# Summary statistics
cat("\nZIP Code Distribution:\n")
cat("Total records:", nrow(zip_final), "\n")
cat("Records with ZIP:", sum(!is.na(zip_final$zip_final)), "\n")
cat("Missing ZIP:", sum(is.na(zip_final$zip_final)), "\n\n")

# Show top ZIP codes
cat("Top 10 ZIP Codes:\n")
print(head(sort(table(zip_final$zip_final, useNA = "ifany"), decreasing = TRUE), 10))


# 8. COUNTY AND PROSPERITY REGION PROCESSING ------------------------------

# 156 missing

county_data <- zip_birth_cert %>%
  transmute(
    march_id, child_id,
    resco = stringr::str_extract(as.character(resco), "\\d+") %>%
      stringr::str_pad(2, "left", "0") # Add leading zero to single-digit county codes
  )


# Merge with county crosswalk and crosswalk_mothers
county_final <- crosswalk_mothers %>%
  left_join(county_data, by = c("march_id", "child_id")) %>%
  left_join(county_crosswalk, by = "resco") %>%
  # Remove rows where all columns except march_id are NA
  filter(!if_all(-march_id, is.na)) %>%
  distinct(march_id, child_id, child_echo_id, mom_echo_id, resco, county = COUNTY, prosperity_region = ProsperityRegion)

# Summary statistics
cat("\n\nCounty Distribution:\n")
print(table(county_final$county, useNA = "always"))

cat("\n\nProsperity Region Distribution:\n")
print(table(county_final$prosperity_region, useNA = "always"))

# Optional: Check for missing county mappings
missing_counties <- county_final %>%
  filter(!is.na(resco) & is.na(county))

if (nrow(missing_counties) > 0) {
  cat("\n\nWarning: Found", nrow(missing_counties), 
      "records with RESCO but no county mapping:\n")
  print(unique(missing_counties$resco))
}


# 9. Prosperity Regions (Michigan Medicaid Health Plans) ------------------

# missing 156

# Source: https://www.michigan.gov/mdhhs/-/media/Project/Websites/mdhhs/Assistance-Programs/Medicaid-BPHASA/Other-Prov-Specific-Page-Docs/MHP-MAP-County-with-Plan-03082024.pdf

# Define Prosperity Regions (Michigan Medicaid Health Plans)
prosperity_regions <- list(
  "Region 1" = c("Gogebic", "Ontonagon", "Houghton", "Keweenaw", "Baraga", 
                 "Iron", "Marquette", "Dickinson", "Menominee", "Delta", 
                 "Alger", "Schoolcraft", "Luce", "Mackinac", "Chippewa"),
  
  "Region 2" = c("Emmet", "Charlevoix", "Antrim", "Leelanau", "Kalkaska", 
                 "Grand Traverse", "Benzie", "Missaukee", "Wexford", "Manistee"),
  
  "Region 3" = c("Cheboygan", "Presque Isle", "Otsego", "Montmorency", "Alpena", 
                 "Crawford", "Oscoda", "Alcona", "Roscommon", "Ogemaw", "Iosco"),
  
  "Region 4" = c("Allegan", "Barry", "Ottawa", "Kent", "Ionia", "Montcalm", 
                 "Muskegon", "Oceana", "Newaygo", "Mecosta", "Osceola", 
                 "Lake", "Mason"),
  
  "Region 5" = c("Clare", "Gladwin", "Arenac", "Isabella", "Midland", 
                 "Bay", "Saginaw", "Gratiot"),
  
  "Region 6" = c("Huron", "Tuscola", "Sanilac", "Shiawassee", "Genesee", 
                 "Lapeer", "St. Clair"),
  
  "Region 7" = c("Clinton", "Eaton", "Ingham"),
  
  "Region 8" = c("Van Buren", "Kalamazoo", "Calhoun", "Berrien", "Cass", 
                 "St. Joseph", "Branch"),
  
  "Region 9" = c("Livingston", "Jackson", "Washtenaw", "Hillsdale", 
                 "Lenawee", "Monroe"),
  
  "Region 10" = c("Macomb", "Oakland", "Wayne")
)

# Extract county code from birth certificate
medicaid_pr_data <- birth_cert %>%
  select(march_id, child_id, resco = RESCO) %>%
  mutate(
    # Standardize county code with leading zero
    resco = str_pad(as.character(resco), width = 2, side = "left", pad = "0")
  )


# ---- 1) Canonical region labels (from MDHHS wording)
region_label_map <- tibble::tibble(
  region_code = as.character(1:10),
  region_label = c(
    "Upper Peninsula Prosperity Alliance",
    "Northwest Prosperity Region",
    "Northeast Prosperity Region",
    "West Michigan Prosperity Region",
    "East Central Michigan Prosperity Region",
    "East Michigan Prosperity Region",
    "South Central Prosperity Region",
    "Southwest Prosperity Region",
    "Southeast Michigan Prosperity Region",
    "Detroit Metro Prosperity Region"
  )
)

# ---- 2) Normalize county names for matching
normalize_county <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_replace_all("\\s+", " ") %>%
    str_replace("^St\\.?\\s+", "St. ") %>%  # keep St. prefix consistent
    str_to_title()
}

# ---- 3) Your prosperity region county lists -> long lookup table
prosperity_lookup <- imap_dfr(prosperity_regions, ~{
  tibble::tibble(
    region_name_key = .y,                       # e.g., "Region 1"
    county_calc = normalize_county(.x)
  )
}) %>%
  mutate(
    region_code_calc = str_extract(region_name_key, "\\d+")
  ) %>%
  select(region_code_calc, county_calc) %>%
  distinct()

# ---- 4) Extract RESCO from birth certificate (clean)
medicaid_pr_data <- birth_cert %>%
  transmute(
    march_id,
    child_id,
    resco = str_extract(as.character(RESCO), "\\d+") %>% str_pad(2, "left", "0")
  )

# ---- 5) Merge: crosswalk -> RESCO -> county_crosswalk
medicaid_pr_final <- crosswalk_mothers %>%
  left_join(medicaid_pr_data, by = c("march_id", "child_id")) %>%
  left_join(county_crosswalk, by = "resco") %>%
  mutate(
    county_norm = normalize_county(COUNTY),
    # Parse a region code from county_crosswalk$ProsperityRegion if present
    region_code_crosswalk = str_extract(as.character(ProsperityRegion), "\\d+")
  ) %>%
  # Add computed region based on your lists
  left_join(prosperity_lookup, by = c("county_norm" = "county_calc")) %>%
  left_join(region_label_map, by = c("region_code_calc" = "region_code")) %>%
  distinct(
    march_id, child_id, child_echo_id, mom_echo_id, resco,
    county = COUNTY,
    prosperity_region_raw = ProsperityRegion,
    region_code_crosswalk,
    region_code_calc,
    region_label
  )



# ---- 6) QC summaries
cat("\nCrosswalk region code distribution:\n")
print(table(medicaid_pr_final$region_code_crosswalk, useNA = "always"))

cat("\nCalculated region code distribution:\n")
print(table(medicaid_pr_final$region_code_calc, useNA = "always"))

table(medicaid_pr_final$region_label, useNA = "always")

# Counties not covered by your lists (should be 0 if lists complete)
uncovered_counties <- medicaid_pr_final %>%
  filter(!is.na(county) & is.na(region_code_calc)) %>%
  distinct(county) %>%
  arrange(county)

cat("\nUncovered counties (not found in prosperity_regions lists):\n")
print(uncovered_counties)

# Mismatches between county_crosswalk and your calculated mapping
mismatch <- medicaid_pr_final %>%
  filter(
    !is.na(region_code_crosswalk),
    !is.na(region_code_calc),
    region_code_crosswalk != region_code_calc
  ) %>%
  distinct(county, resco, prosperity_region_raw, region_code_crosswalk, region_code_calc) %>%
  arrange(county)

cat("\nMismatches between county_crosswalk and calculated mapping:\n")
print(mismatch)




# 10. Child Race / Ethnicity ---------------------------------------------

# Race: 19 missings 
# Ethnicity: 15 missings

recode_ethnicity <- function(x) {
  x_chr <- as.character(x)
  x2 <- x_chr %>%
    str_trim() %>%
    str_to_lower()
  
  case_when(
    # Preserve true NA
    is.na(x) ~ NA_character_,
    
    # Hispanic
    x2 %in% c("1", "hispanic", "hispanic/latino(a)", 
              "hispanic or latino", "latino") ~ 
      "Hispanic or Latino",
    
    # Not Hispanic
    x2 %in% c("0", "not hispanic/latino(a)", 
              "not hispanic or latino", "non-hispanic") ~ 
      "Not Hispanic or Latino",
    
    # Unknown patterns (more robust)
    str_detect(x2, "don't know|dont know|prefer not|unknown|not reported|-8") ~ 
      "Unknown",
    
    # Blank string treated as NA
    x2 == "" ~ NA_character_,
    
    # Fallback
    TRUE ~ NA_character_
  )
}

recode_race_source <- function(x, source = c("registration", "birth_cert", "ripple", "race_coded")) {
  source <- match.arg(source)
  
  x_chr <- as.character(x) %>% str_trim()
  x_low <- str_to_lower(x_chr)
  
  # Preserve NA (keep NA as NA)
  ifelse_na <- function(out) {
    ifelse(is.na(x), NA_character_, out)
  }
  
  # ---- Source 1: Registration (coded numbers)
  if (source == "registration") {
    out <- case_when(
      x_chr == "1"  ~ "White",
      x_chr == "2"  ~ "Black or African American",
      x_chr == "3"  ~ "Asian",
      x_chr == "4"  ~ "Native Hawaiian or Other Pacific Islander",
      x_chr == "5"  ~ "American Indian or Alaska Native",
      x_chr == "6"  ~ "Multiple",
      x_chr == "-8" ~ "Unknown", # keep unknown codes as Unknown
      x_chr == "" ~ NA_character_,
      TRUE ~  NA_character_  
    )
    return(ifelse_na(out))
  }
  
  # ---- Source 2: Birth Certificate (RACECHILD codes)
  if (source == "birth_cert") {
    out <- case_when(
      x_chr == "1" ~ "White",
      x_chr == "2" ~ "Black or African American",
      x_chr == "3" ~ "American Indian or Alaska Native",
      x_chr %in% c("0","4","5","6","9","11","13", "A", "K") ~ "Asian", # Other Asian/Chinese/Japanese/Filipino/Asian Indian/Korean/Vietnamese
      x_chr %in% c("7","8","10","12", "P") ~ "Native Hawaiian or Other Pacific Islander", # Hawaiian/OtherPI/Guaman/Samoan
      x_chr %in% c("14","15", "O") ~ "Other",
      x_chr %in% c("21","22","23","24","20", "M") ~ "Multiple",
      x_chr == "99" ~ "Unknown", # keep unknown codes as Unknown
      x_chr == "" ~ NA_character_,
      TRUE ~  NA_character_
    )
    return(ifelse_na(out))
  }
  
  # ---- Source 3: Ripple (already text)
  # Your tables show values like:
  # "White", "Black or African American", "Asian",
  # "American Indian or Alaska Native", "Native Hawaiian or Pacific Islander",
  # "More than one race", "Other Race", "Don't Know/Prefer not to answer"
  if (source == "ripple") {
    out <- case_when(
      str_detect(x_low, "^white$") ~ "White",
      str_detect(x_low, "^black") ~ "Black or African American",
      str_detect(x_low, "american indian|alaska") ~ "American Indian or Alaska Native",
      str_detect(x_low, "^asian$") ~ "Asian",
      str_detect(x_low, "native hawaiian|pacific") ~ "Native Hawaiian or Other Pacific Islander",
      str_detect(x_low, "more than one race|two or more|multiple") ~ "Multiple",
      str_detect(x_low, "other") ~ "Other",
      str_detect(x_low, "don't know|dont know|prefer not|unknown|not reported") ~ "Unknown",
      x_chr == "" ~ NA_character_,
      TRUE ~ NA_character_
    )
    return(ifelse_na(out))
  }
  
  # ---- Source 4: race_coded (text, sometimes multi like "White & Black ...")
  if (source == "race_coded") {
    # Multi logic (simplified):
    # - Treat any '&' as multiple
    # - Treat "white and other asian" as multiple (special case in this dataset)
    # - Do NOT treat "black and african american" as multiple (single category wording)
    is_multi <- str_detect(x_low, "&") |
      str_detect(x_low, "^white\\s+and\\s+other\\s+asian$") |
      str_detect(x_low, "^white\\s+and\\s+other\\s+asian\\b")
    
    out <- case_when(
      is_multi ~ "Multiple",
      
      # Handle the single-category wording explicitly
      str_detect(x_low, "^black\\s+and\\s+african\\s+american$") ~ "Black or African American",
      
      # Single-race categories
      str_detect(x_low, "^white$") ~ "White",
      str_detect(x_low, "^black") ~ "Black or African American",
      str_detect(x_low, "american indian|alaska") ~ "American Indian or Alaska Native",
      str_detect(x_low, "native hawaiian|pacific") ~ "Native Hawaiian or Other Pacific Islander",
      
      # any of these should be collapsed into Asian group
      str_detect(x_low, "asian|chinese|japanese|korean|filipino|vietnamese|asian indian|other asian") ~ "Asian",
      
      str_detect(x_low, "^other$") ~ "Other",
      str_detect(x_low, "don't know|dont know|unknown|not reported") ~ "Unknown",
      x_chr == "" ~ NA_character_,
      TRUE ~ NA_character_
    )
    return(ifelse_na(out))
  }
  
  # Should never reach here
  return(ifelse_na(NA_character_))
}


# Source 1: Registration (child_id) - numeric codes only
race_reg <- registration_children %>%
  transmute(
    child_id = clean_id(child_id),
    # race_raw = as.character(Race),            # keep raw for QC
    race_reg = recode_race_source(Race, source = "registration"),
    # eth_raw  = as.character(Ethnicity),       # optional: keep raw ethnicity too
    eth_reg  = recode_ethnicity(Ethnicity)
  )

# table(race_reg$race_raw, race_reg$race_reg, useNA = "ifany")
# table(race_reg$eth_raw, race_reg$eth_reg, useNA = "ifany")


# # Show unexpected raw values that became NA
# race_reg %>%
#   filter(!is.na(race_raw) & is.na(race_reg)) %>%
#   count(race_raw, sort = TRUE)

# Source 2: Birth certificate (child_id) - BC code system only
race_bc <- birth_cert %>%
  transmute(
    march_id = clean_id(march_id),
    child_id = clean_id(child_id),
    # race_raw = as.character(RACECHILD), 
    race_bc  = recode_race_source(RACECHILD, source = "birth_cert")
  )

# table(race_bc$race_raw, race_bc$race_bc, useNA = "ifany")

# Source 3: Ripple (child_echo_id) - text only
race_ripple_y <- ripple_young %>%
  transmute(
    child_echo_id = normalize_echo_id(child_echo_id),
    # race_raw = as.character(race),
    race_rip_y = recode_race_source(race, source = "ripple"),
    # eth_raw  = as.character(ethnicity), 
    eth_rip_y  = recode_ethnicity(ethnicity)
  )

# table(race_ripple_y$race_raw, race_ripple_y$race_rip_y, useNA = "ifany")
# table(race_ripple_y$eth_raw, race_ripple_y$eth_rip_y, useNA = "ifany")

race_ripple_o <- ripple_old %>%
  transmute(
    child_echo_id = normalize_echo_id(child_echo_id),
    # race_raw = as.character(race),
    race_rip_o = recode_race_source(race, source = "ripple"),
    # eth_raw  = as.character(ethnicity), 
    eth_rip_o  = recode_ethnicity(ethnicity)
  )

# table(race_ripple_o$race_raw, race_ripple_o$race_rip_o, useNA = "ifany")
# table(race_ripple_o$eth_raw, race_ripple_o$eth_rip_o, useNA = "ifany")

# Source 4: CHARM derived (race_coded) - multi -> Multiple
race_coded_child <- race_coded %>%
  transmute(
    march_id = clean_id(march_id),
    child_id = clean_id(child_id),
    # race_raw = as.character(Child_Race),
    race_coded = recode_race_source(Child_Race, source = "race_coded"),
    # eth_raw  = as.character(Child_Ethnicity), 
    eth_coded  = recode_ethnicity(Child_Ethnicity)
  )

# table(race_coded_child$race_raw, race_coded_child$race_coded, useNA = "ifany")
# table(race_coded_child$eth_raw, race_coded_child$eth_coded, useNA = "ifany")

# Source 5: dem_child 

race_dem_child <- dem_child %>%
  transmute(
    child_echo_id = child_echo_id,
    
    # Hispanic indicator from DEM_B (Yes/No)
    eth_dem_child = case_when(
      dem_c_3 == "1" ~ "Hispanic or Latino",
      dem_c_3 == "2" ~ "Not Hispanic or Latino",
      dem_c_3 == "-7" ~ NA_character_,
      dem_c_3 == "-8" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    # Race checkboxes (top-level)
    chk_white = dem_c_4___1 == "1",
    chk_black = dem_c_4___2 == "1",
    chk_aian  = dem_c_4___3 == "1",
    chk_asian_indian = dem_c_4___4 == "1",
    chk_other_asian = dem_c_4___5 == "1",
    chk_nhpi  = dem_c_4___6 == "1",
    chk_other = dem_c_4___7 == "1",
    chk_na    = dem_c_4____7 == "1",
    chk_unknown    = dem_c_4____8 == "1",
    
    # More detailed Asian subtypes (count as Asian)
    # 1 Chinese, 2 Filipino, 3 Japanese, 4 Korean, 5 Vietnamese, 6 other, -8 Unknown Asian
    chk_asian_sub = (dem_c_4_asian_type___1 == "1") |
      (dem_c_4_asian_type___2 == "1") |
      (dem_c_4_asian_type___3 == "1") |
      (dem_c_4_asian_type___4 == "1") |
      (dem_c_4_asian_type___5 == "1") |
      (dem_c_4_asian_type___6 == "1") |
      (dem_c_4_asian_type____8 == "1")|
      (!is.na(dem_c_4_asian_sp) & dem_c_4_asian_sp != ""),
    
    # More detailed NHPI subtypes (count as NHPI)
    chk_nhpi_sub = (dem_c_4_hawaii_type___1 == "1") |
      (dem_c_4_hawaii_type___2 == "1") |
      (dem_c_4_hawaii_type___3 == "1") |
      (dem_c_4_hawaii_type___4 == "1") |
      (dem_c_4_hawaii_type____8 == "1")|
      (!is.na(dem_c_4_hawaii_sp) & dem_c_4_hawaii_sp != ""),
  ) %>%
  mutate(
    # Collapse Asian + subtypes into one flag
    chk_asian_any = chk_asian_indian | chk_other_asian | chk_asian_sub,
    chk_nhpi_any  = chk_nhpi | chk_nhpi_sub,
    
    # Count how many race groups were selected
    n_selected = (chk_white %>% as.integer()) +
      (chk_black %>% as.integer()) +
      (chk_aian  %>% as.integer()) +
      (chk_asian_any %>% as.integer()) +
      (chk_nhpi_any  %>% as.integer()) +
      (chk_other %>% as.integer()),
    
    # Standardized mother race from DEM_B
    race_dem_child = case_when(
      chk_unknown ~ "Unknown",
      n_selected >= 2 ~ "Multiple",
      chk_white ~ "White",
      chk_black ~ "Black or African American",
      chk_aian  ~ "American Indian or Alaska Native",
      chk_asian_any ~ "Asian",
      chk_nhpi_any  ~ "Native Hawaiian or Other Pacific Islander",
      chk_other ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  select(child_echo_id, race_dem_child, eth_dem_child)



# Merge: Use crosswalk_mothers as master



race_eth_final <- crosswalk_mothers %>%
  # child_id sources
  left_join(race_reg, by = "child_id") %>%
  left_join(race_bc,  by = c("march_id","child_id")) %>%
  left_join(race_coded_child, by = c("march_id","child_id")) %>%
  # child_echo_id sources
  left_join(race_ripple_y, by = "child_echo_id") %>%
  left_join(race_ripple_o, by = "child_echo_id") %>%
  left_join(race_dem_child, by = "child_echo_id") %>%
  mutate(
    # Race: skip NA / Unknown / Other and then move down the priority list
    race_child = pick_first_valid(
      race_coded,
      race_rip_o,
      race_rip_y,
      race_bc,
      race_reg,
      race_dem_child,
      invalid = c("unknown", "other")
    ),
    race_source = pick_source_valid(
      race_coded,
      race_rip_o,
      race_rip_y,
      race_bc,
      race_reg,
      race_dem_child,
      sources = c("race_coded", "ripple_old", "ripple_young", "birth_cert", "registration","dem_child"),
      invalid = c("unknown", "other")
    ),
    
    # Ethnicity: skip NA / Unknown / Other (Other rarely appears, but keep consistent)
    ethnicity_child = pick_first_valid(
      eth_coded,
      eth_rip_o,
      eth_rip_y,
      eth_reg,
      eth_dem_child,
      invalid = c("unknown", "other")
    ),
    ethnicity_source = pick_source_valid(
      eth_coded,
      eth_rip_o,
      eth_rip_y,
      eth_reg,
      eth_dem_child,
      sources = c("race_coded", "ripple_old", "ripple_young", "registration"),
      invalid = c("unknown", "other")
    )
  ) %>%
  select(
    march_id, child_id, mom_echo_id, child_echo_id,
    race_child, ethnicity_child,
    race_source, ethnicity_source,
    # keep recoded components for QC
    race_coded, race_rip_o, race_rip_y, race_bc, race_reg, race_dem_child,
    eth_coded,  eth_rip_o,  eth_rip_y,  eth_reg, eth_dem_child
  )  %>%
  distinct(march_id, child_id, mom_echo_id, child_echo_id,
           race_child, ethnicity_child,
           race_source, ethnicity_source)


# summaries
cat("\nRace distribution:\n")
print(table(race_eth_final$race_child, useNA = "always"))

cat("\nEthnicity distribution:\n")
print(table(race_eth_final$ethnicity_child, useNA = "always"))

cat("\nRace source contribution:\n")
print(table(race_eth_final$race_source, useNA = "always"))

cat("\nEthnicity source contribution:\n")
print(table(race_eth_final$ethnicity_source, useNA = "always"))


# 11. Mom Race / Ethnicity ------------------------------------------------


# Race: 12 missings
# Ethnicity: 10 missings

# --- recode_ethnicity function can be used for Mom Ethnicity 

# SOURCE A: DEM Birth (dem_birth) - derive mother race from checkbox fields

dem_birth_mom_race <- dem_birth %>%
  transmute(
    participantid = participantid,
    
    # Hispanic indicator from DEM_B (Yes/No)
    eth_dem_birth = case_when(
      dem_b_a3 == "1" ~ "Hispanic or Latina",
      dem_b_a3 == "2" ~ "Not Hispanic or Latina",
      dem_b_a3 == "-7" ~ NA_character_,
      dem_b_a3 == "-8" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    # Race checkboxes (top-level)
    chk_white = dem_b_a4___1 == "1",
    chk_black = dem_b_a4___2 == "1",
    chk_aian  = dem_b_a4___3 == "1",
    chk_asian_indian = dem_b_a4___4 == "1",
    chk_other_asian = dem_b_a4___5 == "1",
    chk_nhpi  = dem_b_a4___6 == "1",
    chk_other = dem_b_a4___7 == "1",
    chk_na    = dem_b_a4____7 == "1",
    chk_unknown    = dem_b_a4____8 == "1",
    
    # More detailed Asian subtypes (count as Asian)
    # 1 Chinese, 2 Filipino, 3 Japanese, 4 Korean, 5 Vietnamese, 6 other, -8 Unknown Asian
    chk_asian_sub = (dem_b_a4_asian_type___1 == "1") |
      (dem_b_a4_asian_type___2 == "1") |
      (dem_b_a4_asian_type___3 == "1") |
      (dem_b_a4_asian_type___4 == "1") |
      (dem_b_a4_asian_type___5 == "1") |
      (dem_b_a4_asian_type___6 == "1") |
      (dem_b_a4_asian_type____8 == "1")|
      (!is.na(dem_b_a4_asian_sp) & dem_b_a4_asian_sp != ""),
    
    # More detailed NHPI subtypes (count as NHPI)
    chk_nhpi_sub = (dem_b_a4_hawaii_type___1 == "1") |
      (dem_b_a4_hawaii_type___2 == "1") |
      (dem_b_a4_hawaii_type___3 == "1") |
      (dem_b_a4_hawaii_type___4 == "1") |
      (dem_b_a4_hawaii_type____8 == "1")|
      (!is.na(dem_b_a4_hawaii_sp) & dem_b_a4_hawaii_sp != ""),
  ) %>%
  mutate(
    # Collapse Asian + subtypes into one flag
    chk_asian_any = chk_asian_indian | chk_other_asian | chk_asian_sub,
    chk_nhpi_any  = chk_nhpi | chk_nhpi_sub,
    
    # Count how many race groups were selected
    n_selected = (chk_white %>% as.integer()) +
      (chk_black %>% as.integer()) +
      (chk_aian  %>% as.integer()) +
      (chk_asian_any %>% as.integer()) +
      (chk_nhpi_any  %>% as.integer()) +
      (chk_other %>% as.integer()),
    
    # Standardized mother race from DEM_B
    race_dem_birth = case_when(
      chk_unknown ~ "Unknown",
      n_selected >= 2 ~ "Multiple",
      chk_white ~ "White",
      chk_black ~ "Black or African American",
      chk_aian  ~ "American Indian or Alaska Native",
      chk_asian_any ~ "Asian",
      chk_nhpi_any  ~ "Native Hawaiian or Other Pacific Islander",
      chk_other ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  select(participantid, race_dem_birth, eth_dem_birth)

# Split DEM_B-derived race/ethnicity table into mom-key vs child-key

# Helper: detect participantid ending with "0"
is_mom_key <- function(id) {
  id_chr <- as.character(id) %>% str_trim()
  !is.na(id_chr) & str_detect(id_chr, "0$")
}

# rename race_dem_birth and eth_dem_birth for later left-join
dem_birth_mom_race_mom_key <- dem_birth_mom_race %>%
  filter(is_mom_key(participantid)) %>%
  transmute(
    mom_echo_id = participantid,
    race_dem_birth_mom = race_dem_birth,
    eth_dem_birth_mom  = eth_dem_birth
  )

dem_birth_mom_race_child_key <- dem_birth_mom_race %>%
  filter(!is_mom_key(participantid)) %>%
  transmute(
    child_echo_id = participantid,
    race_dem_birth_child = race_dem_birth,
    eth_dem_birth_child  = eth_dem_birth
  )

# QC: quick counts
cat("DEM_B mom-key rows:", nrow(dem_birth_mom_race_mom_key), "\n")
cat("DEM_B child-key rows:", nrow(dem_birth_mom_race_child_key), "\n")

# SOURCE B: Birth certificate (birth_cert) - mother race/ethnicity
mom_bc <- birth_cert %>%
  transmute(
    march_id = clean_id(march_id),
    child_id = clean_id(child_id),
    race_bc_mom = recode_race_source(MOMRACE, source = "birth_cert"),
    eth_bc_mom  = case_when(
      MOMHISP == "1" ~ "Hispanic or Latina",
      MOMHISP == "2" ~ "Not Hispanic or Latina",
      MOMHISP == "9" ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# SOURCE C: Registration (registration_mothers) - mother race/ethnicity
mom_reg <- registration_mothers %>%
  transmute(
    march_id = clean_id(march_id),
    race_reg_mom = recode_race_source(Race, source = "registration"),
    eth_reg_mom  = case_when(
      Ethnicity == "1" ~ "Hispanic or Latina",
      Ethnicity == "0" ~ "Not Hispanic or Latina",
      Ethnicity == "-8" ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# SOURCE D: CHARM derived (race_coded) - mother race/ethnicity
mom_coded <- race_coded %>%
  transmute(
    march_id = clean_id(march_id),
    child_id = clean_id(child_id),
    # race_raw = as.character(Mother_Race),
    race_coded_mom = recode_race_source(Mother_Race, source = "race_coded"),
    # eth_raw  = as.character(Mother_Ethnicity),
    eth_coded_mom  = case_when(
      Mother_Ethnicity == "Not Hispanic/Latina" ~ "Not Hispanic or Latina",
      Mother_Ethnicity == "Hispanic/Latina" ~ "Hispanic or Latina",
      TRUE ~ NA_character_)
  )


# table(mom_coded$race_raw, mom_coded$race_coded_mom, useNA = "ifany")
# table(mom_coded$eth_raw, mom_coded$eth_coded_mom, useNA = "ifany")


# MERGE + PRIORITIZE (skip NA/Unknown/Other by priority)
# Priority (Race): coded > BC > reg > DEM_B
# Priority (Ethnicity): coded > BC > reg > DEM_B

mother_race_eth_final <- crosswalk_mothers %>%
  # Join DEM_B using mom_echo_id and child_echo_id separately
  left_join(dem_birth_mom_race_mom_key,   by = "mom_echo_id") %>%
  left_join(dem_birth_mom_race_child_key, by = "child_echo_id") %>%
  # Join BC and coded by march_id + child_id
  left_join(mom_bc, by = c("march_id", "child_id")) %>%
  left_join(mom_coded, by = c("march_id", "child_id")) %>%
  # Join registration by march_id
  left_join(mom_reg, by = "march_id") %>%
  mutate(
    race_dem_birth = coalesce(race_dem_birth_mom, race_dem_birth_child),
    eth_dem_birth  = coalesce(eth_dem_birth_mom,  eth_dem_birth_child),
    race_mom = pick_first_valid(
      race_coded_mom,
      race_bc_mom,
      race_reg_mom,
      race_dem_birth,
      invalid = c("unknown", "other")
    ),
    race_source_mom = pick_source_valid(
      race_coded_mom,
      race_bc_mom,
      race_reg_mom,
      race_dem_birth,
      sources = c("race_coded", "birth_cert", "registration", "dem_birth"),
      invalid = c("unknown", "other")
    ),
    ethnicity_mom = pick_first_valid(
      eth_coded_mom,
      eth_bc_mom,
      eth_reg_mom,
      eth_dem_birth,
      invalid = c("unknown", "other")
    ),
    ethnicity_source_mom = pick_source_valid(
      eth_coded_mom,
      eth_bc_mom,
      eth_reg_mom,
      eth_dem_birth,
      sources = c("race_coded", "birth_cert", "registration", "dem_birth"),
      invalid = c("unknown", "other")
    )
  ) %>%
  select(
    march_id, child_id, child_echo_id, mom_echo_id,
    race_mom, ethnicity_mom,
    race_source_mom, ethnicity_source_mom,
    # keep source columns for QC
    race_coded_mom, race_bc_mom, race_reg_mom, race_dem_birth,
    eth_coded_mom,  eth_bc_mom,  eth_reg_mom,  eth_dem_birth
  )

# Check if any mom has inconsistent race across children
mother_race_eth_final %>%
  group_by(march_id, mom_echo_id) %>%
  summarise(
    n_race = n_distinct(race_mom, na.rm = TRUE),
    n_eth  = n_distinct(ethnicity_mom, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_race > 1 | n_eth > 1)

# the result above shows there is no conflict
mother_race_eth_mom_level <- mother_race_eth_final %>%
  distinct(
    march_id, mom_echo_id,
    race_mom,
    ethnicity_mom,
    race_source_mom,
    ethnicity_source_mom
  )

# QC tables
table(mother_race_eth_mom_level$race_mom, useNA = "always")
table(mother_race_eth_mom_level$ethnicity_mom, useNA = "always")
table(mother_race_eth_mom_level$race_source_mom, useNA = "always")
table(mother_race_eth_mom_level$ethnicity_source_mom, useNA = "always")


# 12. MATERNAL AGE AT BIRTH ----------------------------------------------------

# Priority (same as colleague): Ripple Old > Ripple Young > BC MOMAGE >
#                               maternal_age_birth (code derived) > REG_MA > DEM_B
# Notes: Output is child-level, can collapse to mother-level later if needed.

# ---- Helper: parse date safely (expects Date or YYYY-MM-DD / mm/dd/yyyy etc.)
safe_as_date <- function(x, format = NULL) {
  if (inherits(x, "Date")) return(x)
  if (is.null(format)) {
    as.Date(x)
  } else {
    as.Date(x, format = format)
  }
}

# ---- Helper: compute age in years (rounded)
age_years <- function(later_date, earlier_date, digits = 2) {
  round(as.numeric(later_date - earlier_date) / 365.25, digits = digits)
}

# 1) DOB sources (child DOB from Ripple; mom DOB from registration/BC)


# Child DOB from Ripple (uses child_echo_id; birthday = mm/dd/yyyy)
child_dob_ripple_old <- ripple_old %>%
  transmute(
    child_echo_id = child_echo_id,
    child_dob_rip_o = safe_as_date(birthday, format = "%m/%d/%Y")
  )

child_dob_ripple_young <- ripple_young %>%
  transmute(
    child_echo_id = child_echo_id,
    child_dob_rip_y = safe_as_date(birthday, format = "%m/%d/%Y")
  )

# Mom DOB and Child DOB from Birth Certificate (march_id + child_id)
mom_dob_birth_cert <- birth_cert %>%
  transmute(
    march_id = march_id,
    child_id = child_id,
    mom_age_bc = suppressWarnings(as.numeric(MOMAGE)),  # already years in BC
    # mom's birthday
    mom_dob_bc = safe_as_date(
      paste(MOMBXYR, MOMBXMO, MOMBXDAY, sep = "-"),
      format = "%Y-%m-%d"
    ),
    # child's birthday
    child_dob_bc = safe_as_date(
      paste(BXYEAR, BXMONTH, BXDAY, sep = "-"),
      format = "%Y-%m-%d"
    )
  )

# Mom DOB from Registration (mother level) and Child DOB from Registration (child level)
# registration_mothers has birth_date_mom; registration_children has birth_date_child
mom_dob_registration <- registration_mothers %>%
  transmute(
    march_id = march_id,
    # mom's birthday
    mom_dob_reg = safe_as_date(birth_date_mom)
  )

child_dob_registration <- registration_children %>%
  transmute(
    child_id = child_id,
    # child's birthday
    child_dob_reg = safe_as_date(birth_date_child)
  )

# Maternal age from Registration-derived DOBs (REG_MA = child_dob - mom_dob)
mat_age_from_registration <- crosswalk_mothers %>%
  select(march_id, child_id, child_echo_id, mom_echo_id) %>%
  left_join(mom_dob_registration, by = "march_id") %>%
  left_join(child_dob_registration, by = "child_id") %>%
  mutate(
    mat_age_reg = ifelse(
      !is.na(mom_dob_reg) & !is.na(child_dob_reg),
      age_years(child_dob_reg, mom_dob_reg, digits = 2),
      NA_real_
    )
  ) %>%
  select(march_id, child_id, child_echo_id, mom_echo_id, mat_age_reg, mom_dob_reg, child_dob_reg)

# Maternal age from DEM_B (dem_b_a1 is mom DOB; need child DOB to compute)
# dem_b_a1 may already be a date string; 
cutoff_date <- as.Date("2018-01-01")

dem_birth_mom_dob_raw <- dem_birth %>%
  transmute(
    participantid = participantid,
    # mom's birthday reported in DEM_B
    mom_dob_dem_b = safe_as_date(dem_b_a1)
  ) %>%
  # Remove implausible "mom DOB" (too recent -> likely not mom DOB)
  filter(is.na(mom_dob_dem_b) | mom_dob_dem_b <= cutoff_date)

# Split into mom-key vs child-key
dem_birth_mom_dob_mom_key <- dem_birth_mom_dob_raw %>%
  filter(is_mom_key(participantid)) %>%
  transmute(
    mom_echo_id = participantid,
    mom_dob_dem_b
  )

dem_birth_mom_dob_child_key <- dem_birth_mom_dob_raw %>%
  filter(!is_mom_key(participantid)) %>%
  transmute(
    child_echo_id = participantid,
    mom_dob_dem_b
  )


# 2) Merge all sources on crosswalk_mothers (master)


maternal_age_at_birth_all <- crosswalk_mothers %>%
  select(march_id, child_id, child_echo_id, mom_echo_id) %>%
  # bring in registration-based maternal age + DOBs
  left_join(mat_age_from_registration, by = c("march_id", "child_id", "child_echo_id", "mom_echo_id")) %>%
  # bring in BC mom age and mom DOB
  left_join(mom_dob_birth_cert, by = c("march_id", "child_id")) %>%
  # bring in code-derived maternal_age_birth (march_id-level)
  left_join(
    maternal_age_birth %>%
      transmute(march_id = march_id, mat_age_code = maternal_age_birth),
    by = "march_id"
  ) %>%
  # child DOB from Ripple
  left_join(child_dob_ripple_young, by = "child_echo_id") %>%
  left_join(child_dob_ripple_old,   by = "child_echo_id") %>%
  # Add DEM_B mom DOB by mom_echo_id and child_echo_id separately
  left_join(dem_birth_mom_dob_mom_key,   by = "mom_echo_id") %>%
  left_join(dem_birth_mom_dob_child_key, by = "child_echo_id") %>%
  mutate(
    mom_dob_dem_b = coalesce(mom_dob_dem_b.x, mom_dob_dem_b.y),
    # pick best mom DOB: registration first, then BC mom DOB, then dem_birth information
    mom_dob_temp_final = coalesce(mom_dob_reg, mom_dob_bc, mom_dob_dem_b),
    
    # maternal age based on Ripple child DOB - mom DOB
    mat_age_rip_o = ifelse(
      !is.na(child_dob_rip_o) & !is.na(mom_dob_temp_final),
      age_years(child_dob_rip_o, mom_dob_temp_final, digits = 2),
      NA_real_
    ),
    mat_age_rip_y = ifelse(
      !is.na(child_dob_rip_y) & !is.na(mom_dob_temp_final),
      age_years(child_dob_rip_y, mom_dob_temp_final, digits = 2),
      NA_real_
    )
  ) %>%
  mutate(
    # Priority (same as colleague):
    # RIP_O > RIP_Y > BC MOMAGE > code-derived > REG_MA > DEM_B
    mat_age_birth = coalesce(
      mat_age_rip_o,
      mat_age_rip_y,
      mom_age_bc,
      mat_age_code,
      mat_age_reg
    ),
    mat_age_source = case_when(
      !is.na(mat_age_rip_o) ~ "ripple_old",
      is.na(mat_age_rip_o) & !is.na(mat_age_rip_y) ~ "ripple_young",
      is.na(mat_age_rip_o) & is.na(mat_age_rip_y) & !is.na(mom_age_bc) ~ "birth_cert_momage",
      is.na(mat_age_rip_o) & is.na(mat_age_rip_y) & is.na(mom_age_bc) & !is.na(mat_age_code) ~ "code_derived",
      is.na(mat_age_rip_o) & is.na(mat_age_rip_y) & is.na(mom_age_bc) & is.na(mat_age_code) & !is.na(mat_age_reg) ~ "registration_dob",
      TRUE ~ "missing"
    )
  ) %>%
  distinct(march_id, child_id, mom_echo_id, child_echo_id, mat_age_birth, mat_age_source)

maternal_age_at_birth_final <- maternal_age_at_birth_all %>%
  mutate(
    age_group = cut(
      mat_age_birth,
      breaks = c(0, 19.99, 24.99, Inf),
      labels = c("Under20", "yrs20_24", "Over25")
    )
  )

# Summary stats
table(maternal_age_at_birth_final$age_group, useNA = "always")

ma_stats <- tibble(
  Median = median(maternal_age_at_birth_final$mat_age_birth, na.rm = TRUE),
  Mean   = mean(maternal_age_at_birth_final$mat_age_birth, na.rm = TRUE),
  SD     = sd(maternal_age_at_birth_final$mat_age_birth, na.rm = TRUE),
  Min    = min(maternal_age_at_birth_final$mat_age_birth, na.rm = TRUE),
  Max    = max(maternal_age_at_birth_final$mat_age_birth, na.rm = TRUE),
  PercentUnder20   = 100 * mean(maternal_age_at_birth_final$age_group == "Under20", na.rm = TRUE),
  Percent20_24yrs   = 100 * mean(maternal_age_at_birth_final$age_group == "yrs20_24", na.rm = TRUE),
  PercentOver25     = 100 * mean(maternal_age_at_birth_final$age_group == "Over25", na.rm = TRUE)
)

print(ma_stats)

hist(maternal_age_at_birth_final$mat_age_birth, breaks = 20)
abline(v = mean(maternal_age_at_birth_final$mat_age_birth, na.rm = TRUE), lty = 2, col = "red")


# 13. PARITY + PARITY CATEGORY --------------------------------------------

# PARITY (number of pregnancies) + PARITY CATEGORY (parous)
# Priority: MMRA > Registration

# ---- Helper: clean numeric with special missing codes
clean_num <- function(x, na_codes = c("-8", "-7", "999", "99", "")) {
  x_chr <- as.character(x) %>% str_trim()
  x_chr[x_chr %in% na_codes] <- NA_character_
  suppressWarnings(as.numeric(x_chr))
}


# Source 1: Registration (child_id) - PregnancyNumber

parity_registration <- registration_children %>%
  transmute(
    child_id = child_id,
    parity_reg = clean_num(PregnancyNumber)
  )


# Source 2: MMRA (mom_echo_id) - mmra_b3c (number of pregnancies)

parity_mmra <- mmra %>%
  transmute(
    mom_echo_id = mom_echo_id,
    parity_mmra = clean_num(mmra_b3c)
  )


# Merge using crosswalk_mothers as master
parity_final <- crosswalk_mothers %>%
  left_join(parity_mmra, by = "mom_echo_id") %>%
  left_join(parity_registration, by = "child_id") %>%
  mutate(
    # Priority: MMRA > Registration
    parity = coalesce(parity_mmra, parity_reg),
    
    # Categorize parity
    parous = case_when(
      is.na(parity) ~ NA_character_,
      parity == 0 ~ "Nulliparous",
      parity == 1 ~ "Primiparous",
      parity >= 2 ~ "Multiparous",
      TRUE ~ NA_character_
    ),
    
    # QC: record source
    parity_source = case_when(
      !is.na(parity_mmra) ~ "mmra",
      is.na(parity_mmra) & !is.na(parity_reg) ~ "registration",
      TRUE ~ "missing"
    )
  ) %>%
  select(
    march_id, child_id, child_echo_id, mom_echo_id,
    parity, parous, parity_source,
    # keep source columns for QC
    parity_mmra, parity_reg
  ) %>%
  remove_empty_rows(c("march_id", "child_id", "child_echo_id", "mom_echo_id")) %>%
  distinct(march_id, child_id, mom_echo_id, child_echo_id, parity, parous, parity_source)


# Stats

table(parity_final$parity, useNA = "always")
table(parity_final$parous, useNA = "always")
table(parity_final$parity_source, useNA = "always")


# 14. Mom's Education -----------------------------------------------------

# missing: 49

# Helper: recode maternal education from different sources

recode_mom_educ <- function(x, source = c("birth_cert", "dem_birth")) {
  source <- match.arg(source)
  x_chr <- as.character(x) %>% str_trim()
  
  # Preserve NA
  if (all(is.na(x_chr))) return(rep(NA_character_, length(x_chr)))
  
  if (source == "birth_cert") {
    return(case_when(
      is.na(x_chr) ~ NA_character_,
      x_chr == "1" ~ "<9th",
      x_chr == "2" ~ "9-12th (no diploma)",
      x_chr == "3" ~ "HS or GED",
      x_chr == "4" ~ "Some College/No Degree",
      x_chr == "5" ~ "Associates",
      x_chr == "6" ~ "Bachelors",
      x_chr == "7" ~ "Masters",
      x_chr == "8" ~ "Doctoral or Professional Degree",
      x_chr == "9" ~ NA_character_, 
      TRUE ~ NA_character_
    ))
  }
  
  # dem_birth (DEM_B: dem_b_a2)
  case_when(
    is.na(x_chr) ~ NA_character_,
    x_chr == "1"  ~ "No Schooling",
    x_chr == "2"  ~ "<9th",
    x_chr == "3"  ~ "9-12th (no diploma)",
    x_chr %in% c("4","5")  ~ "HS or GED", # 4 - "HS Diploma", 5 - "GED"
    x_chr == "6"  ~ "Some College/No Degree",
    x_chr == "7"  ~ "Associates",
    x_chr == "8"  ~ "Bachelors",
    x_chr == "9"  ~ "Masters",
    x_chr == "10" ~ "Doctoral or Professional Degree",
    x_chr == "-7" ~ NA_character_,
    x_chr == "-8" ~ "Unknown",
    TRUE ~ NA_character_
  )
}


# Source 1: Birth certificate maternal education (march_id + child_id)

mom_educ_bc <- birth_cert %>%
  transmute(
    march_id,
    child_id,
    mom_educ_bc = recode_mom_educ(MOMEDUC, source = "birth_cert")
  )

# Source 2: DEM_B maternal education (participantid could be mom or child key)

dem_birth_educ <- dem_birth %>%
  transmute(
    participantid = participantid,
    mom_educ_dem  = recode_mom_educ(dem_b_a2, source = "dem_birth")
  )

dem_birth_educ_mom_key <- dem_birth_educ %>%
  filter(is_mom_key(participantid)) %>%
  transmute(
    mom_echo_id = participantid,
    mom_educ_dem_mom = mom_educ_dem
  )

dem_birth_educ_child_key <- dem_birth_educ %>%
  filter(!is_mom_key(participantid)) %>%
  transmute(
    child_echo_id = participantid,
    mom_educ_dem_child = mom_educ_dem
  )


# Merge + prioritize
mom_educ_final <- crosswalk_mothers %>%
  left_join(mom_educ_bc, by = c("march_id", "child_id")) %>%
  left_join(dem_birth_educ_mom_key,   by = "mom_echo_id") %>%
  left_join(dem_birth_educ_child_key, by = "child_echo_id") %>%
  mutate(
    mom_educ_dem = coalesce(mom_educ_dem_mom, mom_educ_dem_child),
    
    # Priority: BC > DEM_B (but skip Unknown if you want)
    mom_educ_priority = coalesce(
      mom_educ_bc,
      mom_educ_dem
    ),
    
    mom_educ_source = case_when(
      !is.na(mom_educ_bc) ~ "birth_cert",
      is.na(mom_educ_bc) & !is.na(mom_educ_dem) ~ "dem_birth",
      TRUE ~ "missing"
    )
  ) %>%
  select(
    march_id, child_id, child_echo_id, mom_echo_id,
    mom_educ_priority, mom_educ_source,
    # keep QC cols
    mom_educ_bc, mom_educ_dem
  ) %>%
  remove_empty_rows(c("march_id", "child_id", "child_echo_id", "mom_echo_id")) %>%
  distinct(march_id, child_id, mom_echo_id, child_echo_id, mom_educ_priority, mom_educ_source)

# Stats
table(mom_educ_final$mom_educ_priority, useNA = "always")
table(mom_educ_final$mom_educ_source, useNA = "always")


# 15. Relationship Status at Birth ----------------------------------------

# 11 missings

# Recode marital status by source

recode_marital_source <- function(x, source = c("birth_cert", "prenatal_1", "dem_birth", "dem_caregiver")) {
  source <- match.arg(source)
  x_chr <- as.character(x) %>% str_trim()
  
  # Preserve NA
  out <- case_when(
    is.na(x_chr) | x_chr == "" ~ NA_character_,
    TRUE ~ x_chr
  )
  
  if (source == "birth_cert") {
    # BC MARITALSTATUS: 1 Never Married, 2 Married, 3 Divorced/Widowed, 4 Married (per colleague)
    return(case_when(
      is.na(out) ~ NA_character_,
      out == "1" ~ "Never Married",
      out == "2" ~ "Married",
      out == "3" ~ "Divorced/Widowed",
      out == "4" ~ "Married",
      TRUE ~ NA_character_
    ))
  }
  
  if (source == "prenatal_1") {
    # PN1 MARIT_STATUS: 1 Married, 2 Living w/ Partner, 3 Divorced, 4 Separated, 5 Widowed, 6 Never Married, 98 Don't Know
    return(case_when(
      is.na(out) ~ NA_character_,
      out == "1"  ~ "Married",
      out == "2"  ~ "Living w/ Partner",
      out == "3"  ~ "Divorced",
      out == "4"  ~ "Separated",
      out == "5"  ~ "Widowed",
      out == "6"  ~ "Never Married",
      out == "98"  ~ "Unknown",
      out == "99"  ~ NA_character_, # Refuse
      TRUE ~ NA_character_
    ))
  }
  
  # DEM_B / DEM_CG: 1-2 Married, 3-4 Living w/ Partner, 5 Widowed, 6 Separated, 7 Divorced, 8 Never Married, -7 Prefer not, -8 Don't know
  case_when(
    is.na(out) ~ NA_character_,
    out %in% c("1","2") ~ "Married",
    out %in% c("3","4") ~ "Living w/ Partner",
    out == "5" ~ "Widowed",
    out == "6" ~ "Separated",
    out == "7" ~ "Divorced",
    out == "8" ~ "Never Married",
    out == "-7" ~ NA_character_, # "Prefer Not to Answer"
    out == "-8" ~ "Unknown",
    TRUE ~ NA_character_
  )
}


# Collapse to 3-level marital status used on website
# colleague logic:
#   Divorced / Widowed / Separated -> "Divorced/Widowed/Separated"
#   Living w/ Partner -> "Never Married"
#   Married -> "Married"
#   Never Married -> "Never Married"

collapse_marital_gen <- function(x) {
  x2 <- as.character(x) %>% str_trim()
  case_when(
    is.na(x2) ~ NA_character_,
    x2 %in% c("Divorced", "Widowed", "Separated", "Divorced/Widowed") ~ "Divorced/Widowed/Separated",
    x2 == "Married" ~ "Married",
    x2 %in% c("Never Married", "Living w/ Partner") ~ "Never Married",
    x2 %in% c("Unknown", "Prefer Not to Answer") ~ x2,
    TRUE ~ NA_character_
  )
}

# Pick first valid value given priority, skipping invalid values
# invalid: NA + "Unknown" + "Prefer Not to Answer" (and optionally "Other" if you want)

# Source extracts

marital_bc <- birth_cert %>%
  transmute(
    march_id,
    child_id,
    marital_bc = recode_marital_source(MARITALSTATUS, source = "birth_cert")
  )

marital_pn1 <- prenatal_1 %>%
  transmute(
    march_id,
    marital_pn1 = recode_marital_source(MARIT_STATUS, source = "prenatal_1")
  )

# dem_caregiver: child_echo_id + visit (keep visit for QC; you may want most recent later)
marital_dem_cg <- dem_caregiver %>%
  transmute(
    child_echo_id,
    visitname,
    marital_dem_cg = recode_marital_source(dem_cg_a6, source = "dem_caregiver")
  )

marital_dem_birth_raw <- dem_birth %>%
  transmute(
    participantid = participantid,
    visitname = visitname,
    marital_dem_birth = recode_marital_source(dem_b_a6, source = "dem_birth")
  )

# Mom key (participantid ends with 0) -> mom_echo_id
marital_dem_birth_mom_key <- marital_dem_birth_raw %>%
  filter(is_mom_key(participantid)) %>%
  transmute(
    mom_echo_id = participantid,
    visitname,
    marital_dem_birth_mom = marital_dem_birth
  )

# Child key (participantid does NOT end with 0) -> child_echo_id
marital_dem_birth_child_key <- marital_dem_birth_raw %>%
  filter(!is_mom_key(participantid)) %>%
  transmute(
    child_echo_id = participantid,
    visitname,
    marital_dem_birth_child = marital_dem_birth
  )

# Merge with priority: BC > PN1 > DEM_CG > DEM_B
# and skip Unknown/Prefer Not to Answer when choosing final value

marital_final_raw <- crosswalk_mothers %>%
  left_join(marital_bc,  by = c("march_id", "child_id")) %>%
  left_join(marital_pn1, by = "march_id") %>%
  left_join(marital_dem_cg,   by = "child_echo_id") %>%
  left_join(marital_dem_birth_mom_key,   by = "mom_echo_id") %>%
  left_join(marital_dem_birth_child_key, by = "child_echo_id") %>%
  mutate(
    marital_dem_birth = pick_first_valid(marital_dem_birth_mom, marital_dem_birth_child, invalid = c("unknown")),
    marital_detail = pick_first_valid(
      marital_bc,
      marital_pn1,
      marital_dem_cg,
      marital_dem_birth,
      invalid = c("unknown")
    ),
    marital_source = pick_source_valid(
      marital_bc,
      marital_pn1,
      marital_dem_cg,
      marital_dem_birth,
      sources = c("birth_cert", "prenatal_1", "dem_caregiver", "dem_birth"),
      invalid = c("unknown")
    ),
    marital_gen = collapse_marital_gen(marital_detail)
  ) %>%
  select(
    march_id, child_id, child_echo_id, mom_echo_id,
    marital_detail, marital_gen, marital_source,
    # QC columns
    marital_bc, marital_pn1, marital_dem_cg, marital_dem_birth,
    visitname.x, visitname.y
  ) %>%
  remove_empty_rows(c("march_id", "child_id", "child_echo_id", "mom_echo_id"))

source_rank <- c(
  "birth_cert"   = 1,
  "prenatal_1"   = 2,
  "dem_caregiver"= 3,
  "dem_birth"    = 4,
  "missing"      = 99
)

marital_final <- marital_final_raw %>%
  mutate(
    # Normalize blanks
    marital_detail = na_if(str_trim(marital_detail), ""),
    marital_gen    = na_if(str_trim(marital_gen), ""),
    marital_source = na_if(str_trim(marital_source), ""),
    # Rank rows within each key: prefer non-missing and non-unknown
    flag_good = !is.na(marital_detail) & !str_to_lower(marital_detail) %in% c("unknown"),
    src_rank  = unname(source_rank[marital_source]) %>% replace_na(99)
  ) %>%
  group_by(march_id, child_id, child_echo_id, mom_echo_id) %>%
  arrange(
    desc(flag_good),   # keep informative rows first
    src_rank,          # prefer higher-priority source (smaller rank)
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(march_id, child_id, child_echo_id, mom_echo_id, 
         marital_detail, marital_gen, marital_source)

# QC: check if any duplicates remain (should be 0)
dup_n <- marital_final_raw %>%
  count(march_id, child_id, child_echo_id, mom_echo_id) %>%
  filter(n > 1) %>%
  nrow()
cat("Remaining duplicate key groups:", dup_n, "\n")

# Stats
table(marital_final$marital_detail, useNA = "always")
table(marital_final$marital_gen, useNA = "always")
table(marital_final$marital_source, useNA = "always")


# 15. Insurance Coverage at Birth -----------------------------------------

# 15 missings

# Recode insurance (PN1 yes/no)
recode_ins_yesno_pn1 <- function(x) {
  x_chr <- as.character(x) %>% str_trim()
  case_when(
    is.na(x_chr) ~ NA_character_,
    x_chr == "1"  ~ "Yes",
    x_chr == "5"  ~ "No",
    x_chr == "98" ~ "Unknown",
    x_chr == "" ~ NA_character_,
    TRUE ~ NA_character_
  )
}


# Recode insurance type (Birth certificate pay source)
recode_ins_type_bc <- function(x) {
  x_chr <- as.character(x) %>% str_trim()
  case_when(
    is.na(x_chr) ~ NA_character_,
    x_chr == "1" ~ "PrivateIns",
    x_chr == "2" ~ "Medicaid",
    x_chr == "3" ~ "Self-Pay",
    x_chr == "4" ~ "IndianHS",
    x_chr == "5" ~ "CHAMPUS/TRICARE",
    x_chr == "6" ~ "OtherGovIns",
    x_chr == "8" ~ "Other",
    x_chr == "9" ~ "Unknown",
    x_chr == "" ~ NA_character_,
    TRUE ~ NA_character_
  )
}

# Convert BC insurance type -> Yes/No (optional)
bc_type_to_yesno <- function(x_type) {
  x2 <- as.character(x_type) %>% str_trim()
  case_when(
    is.na(x2) ~ NA_character_,
    x2 %in% c("PrivateIns","Medicaid","IndianHS","CHAMPUS/TRICARE","OtherGovIns","Other") ~ "Yes",
    x2 == "Self-Pay" ~ "No",
    x2 == "Unknown" ~ "Unknown",
    TRUE ~ NA_character_
  )
}

# Source 1: PN1 (prenatal_1) - yes/no insurance
ins_pn1 <- prenatal_1 %>%
  transmute(
    march_id,
    ins_pn1 = recode_ins_yesno_pn1(INSURANCE)
  )

# Source 2: Birth certificate - insurance type + derived yes/no
ins_bc <- birth_cert %>%
  transmute(
    march_id,
    child_id,
    ins_type_bc = recode_ins_type_bc(PAYSOURCE),
    ins_bc_yesno = bc_type_to_yesno(ins_type_bc)
  )

ins_final_raw <- crosswalk_mothers %>%
  left_join(ins_pn1, by = "march_id") %>%
  left_join(ins_bc,  by = c("march_id","child_id")) %>%
  mutate(
    # If PN1 is Unknown or NA, fall back to BC yes/no
    ins_final = coalesce(
      ifelse(!is.na(ins_pn1) & str_to_lower(ins_pn1) != "unknown", ins_pn1, NA_character_),
      ifelse(!is.na(ins_bc_yesno) & str_to_lower(ins_bc_yesno) != "unknown", ins_bc_yesno, NA_character_)
    ),
    ins_source = case_when(
      !is.na(ins_pn1) & str_to_lower(ins_pn1) != "unknown" ~ "prenatal_1",
      (is.na(ins_pn1) | str_to_lower(ins_pn1) == "unknown") &
        !is.na(ins_bc_yesno) & str_to_lower(ins_bc_yesno) != "unknown" ~ "birth_cert",
      TRUE ~ "missing"
    )
  ) %>%
  select(
    march_id, child_id, child_echo_id, mom_echo_id,
    ins_final, ins_source,
    # QC columns
    ins_pn1, ins_type_bc, ins_bc_yesno
  ) %>%
  remove_empty_rows(c("march_id", "child_id", "child_echo_id", "mom_echo_id"))

source_rank <- c(
  "birth_cert"   = 1,
  "prenatal_1"   = 2,
  "missing"      = 99
)

ins_final <- ins_final_raw %>%
  mutate(
    # Normalize blanks
    ins_final = na_if(str_trim(ins_final), ""),
    ins_source    = na_if(str_trim(ins_source), ""),
    # Rank rows within each key: prefer non-missing and non-unknown
    flag_good = !is.na(ins_final) & !str_to_lower(ins_final) %in% c("unknown"),
    src_rank  = unname(source_rank[ins_source]) %>% replace_na(99)
  ) %>%
  group_by(march_id, child_id, child_echo_id, mom_echo_id) %>%
  arrange(
    desc(flag_good),   # keep informative rows first
    src_rank,          # prefer higher-priority source (smaller rank)
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(march_id, child_id, child_echo_id, mom_echo_id,,
         ins_final, ins_source)

# Stats
table(ins_final$ins_final, useNA = "always")
table(ins_final$ins_source, useNA = "always")
table(ins_final$ins_type_bc, useNA = "ifany")



# 16. Save Date -----------------------------------------------------------


# 1) Prepare sheet list

sheet_list <- list(
  "Child Sex" = sex_final,
  "Urban Rural" = urban_final,
  "ZIP" = zip_final,
  "County" = county_final,
  "Prosperity Regions" = medicaid_pr_final,
  "Child Race and Ethnicity" = race_eth_final,
  "Mom Race and Ethnicity" = mother_race_eth_mom_level,
  "Maternal Age at Birth" = maternal_age_at_birth_final,
  "Parity" = parity_final,
  "Maternal Education Level" = mom_educ_final,
  "Relationship Status at Birth" = marital_final,
  "Insurance Coverage at Birth" = ins_final
)


# 2) Create workbook and write sheets

wb <- createWorkbook()

for (nm in names(sheet_list)) {
  addWorksheet(wb, nm)
  writeDataTable(
    wb, sheet = nm, x = sheet_list[[nm]],
    withFilter = TRUE, tableStyle = "TableStyleLight9"
  )
  freezePane(wb, sheet = nm, firstRow = TRUE)
  setColWidths(wb, sheet = nm, cols = 1:200, widths = "auto")
}

# 3) Notes sheet (at the end)

addWorksheet(wb, "Notes")

notes_text <- c(
  "Notes",
  "",
  "1. Avery Armstrong prepared the initial code draft.",
  "2. Jiahe Tian reviewed and revised the code.",
  "3. Blanks represent NA values.",
  "4. Data creation date: 2026 March. 2."
)

writeData(wb, "Notes", x = notes_text, startCol = 1, startRow = 1)

# Make the title a bit nicer
title_style <- createStyle(textDecoration = "bold", fontSize = 14)
addStyle(wb, "Notes", style = title_style, rows = 1, cols = 1, gridExpand = TRUE)

setColWidths(wb, "Notes", cols = 1, widths = 80)

# -----------------------------
# 4) Save
# -----------------------------
out_file <- "CHARM_Demographics_Derived_2026-02-25.xlsx"
saveWorkbook(wb, file = out_file, overwrite = TRUE)

cat("Saved to:", normalizePath(out_file), "\n")
