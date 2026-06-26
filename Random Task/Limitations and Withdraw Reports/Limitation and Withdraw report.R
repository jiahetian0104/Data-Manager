library(tidyverse)

#!/usr/bin/env Rscript
token <- "D199C7FD4EF4A61F17AB8DEFB6AFB1F2"
url <- "https://echoredcap.org/api/"



# 2. Limits ---------------------------------------------------------------



formData <- list("token"=token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'fields[0]'='record_id',
                 'fields[1]'='participantid',
                 'forms[0]'='adm_limits',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)

limited_df <- result %>%
  filter(adm_limits_complete == 2)

empty_columns <- limited_df %>%
  select(where(~ all(is.na(.)))) %>%
  names()

empty_columns

clean_df <- limited_df %>%
  select(-any_of(empty_columns))

colnames(clean_df)


is_checked <- function(x) {
  x %in% c(1, "1", TRUE)
}


limits_clean <- clean_df %>%
  mutate(
    # ---- ID fields ----
    record_id = as.character(record_id),
    limits_pregid = as.character(limits_pregid),
    
    # ---- Date fields ----
    limits_formdt = as.Date(limits_formdt),
    limits_effective_date = as.Date(limits_1),
    
    # ---- Numeric fields ----
    limits_gainwksedd = as.integer(limits_gainwksedd),
    limits_ageinmos = as.integer(limits_ageinmos),
    limits_ageinyears = as.integer(limits_ageinyears),
    limits_dcf_ver = as.numeric(limits_dcf_ver),
    
    # ---- Respondent ----
    respondent = case_when(
      limits_respondent == 1 | limits_respondent == "1" ~ "Participant",
      limits_respondent == 2 | limits_respondent == "2" ~ "Biological Mother",
      limits_respondent == 3 | limits_respondent == "3" ~ "Biological Father",
      limits_respondent == 4 | limits_respondent == "4" ~ "Other Respondent",
      TRUE ~ NA_character_
    ),
    
    # ---- Data limitation yes/no ----
    data_limit_imposed = case_when(
      limits_2 == 1 | limits_2 == "1" ~ "Yes",
      limits_2 == 2 | limits_2 == "2" ~ "No",
      TRUE ~ NA_character_
    ),
    
    # ---- Biospecimen limitation yes/no ----
    biospecimen_limit_imposed = case_when(
      limits_3 == 1 | limits_3 == "1" ~ "Yes",
      limits_3 == 2 | limits_3 == "2" ~ "No",
      TRUE ~ NA_character_
    ),
    
    # ---- Data limitation checkbox indicators ----
    data_limit_no_address_research = is_checked(limits_2a___1),
    data_limit_other = is_checked(limits_2a___2),
    
    # ---- Biospecimen limitation checkbox indicators ----
    # limits_3a fields are retired, but keep them for historical records
    bio_limit_no_drug_testing = is_checked(limits_3a___1),
    
    bio_limit_no_genetic_research = case_when(
      is_checked(limits_3a___2) | is_checked(limits_3a_r1___2) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    bio_limit_no_genome_sequencing = is_checked(limits_3a_r1___4),
    
    bio_limit_other = case_when(
      is_checked(limits_3a___3) | is_checked(limits_3a_r1___3) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # ---- Form status ----
    form_status = case_when(
      adm_limits_complete == 0 | adm_limits_complete == "0" ~ "Incomplete",
      adm_limits_complete == 1 | adm_limits_complete == "1" ~ "Unverified",
      adm_limits_complete == 2 | adm_limits_complete == "2" ~ "Complete",
      TRUE ~ NA_character_
    ),
    
    # ---- Overall limitation flag ----
    any_limitation = case_when(
      data_limit_imposed == "Yes" | biospecimen_limit_imposed == "Yes" ~ TRUE,
      data_limit_imposed == "No" & biospecimen_limit_imposed == "No" ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  rowwise() %>%
  mutate(
    # ---- Human-readable data limitation summary ----
    data_limit_summary = paste(
      c(
        if (data_limit_no_address_research) {
          "Identifying address information may not be used for research purposes"
        },
        if (data_limit_other) {
          "Other data limitation"
        }
      ),
      collapse = "; "
    ),
    
    # ---- Human-readable biospecimen limitation summary ----
    biospecimen_limit_summary = paste(
      c(
        if (bio_limit_no_drug_testing) {
          "Biospecimens may not be used for drug testing"
        },
        if (bio_limit_no_genetic_research) {
          "Biospecimens may not be used for genetic research"
        },
        if (bio_limit_no_genome_sequencing) {
          "Biospecimens may not be used for genome sequencing"
        },
        if (bio_limit_other) {
          paste0("Other: ", limits_3aoth)
        }
      ),
      collapse = "; "
    )
  ) %>%
  ungroup() %>%
  mutate(
    data_limit_summary = na_if(data_limit_summary, ""),
    biospecimen_limit_summary = na_if(biospecimen_limit_summary, "")
  ) %>%
  select(
    # ---- IDs and event ----
    record_id,
    redcap_event_name,
    limits_pregid,
    
    # ---- Dates and age ----
    limits_formdt,
    limits_effective_date,
    limits_gainwksedd,
    limits_ageinmos,
    limits_ageinyears,
    
    # ---- Respondent ----
    respondent,
    limits_otherresp,
    
    # ---- Limitation flags ----
    any_limitation,
    
    data_limit_imposed,
    data_limit_no_address_research,
    data_limit_other,
    data_limit_summary,
    
    biospecimen_limit_imposed,
    bio_limit_no_drug_testing,
    bio_limit_no_genetic_research,
    bio_limit_no_genome_sequencing,
    bio_limit_other,
    limits_3aoth,
    biospecimen_limit_summary,
    
    # ---- Metadata ----
    limits_dcf_ver,
    limits_version,
    limits_device,
    form_status,
    
    # ---- Raw variables for QC ----
    limits_1,
    limits_2,
    limits_2a___1,
    limits_2a___2,
    limits_3,
    limits_3a___1,
    limits_3a___2,
    limits_3a___3,
    limits_3a_r1___2,
    limits_3a_r1___4,
    limits_3a_r1___3,
    adm_limits_complete
  )


limits_export <- limits_clean %>%
  select(
    `Record ID` = record_id,
    `Event Name` = redcap_event_name,
    `Form Completed` = limits_formdt,
    `Effective Date of Limitations` = limits_effective_date,
    `Any Limitation` = any_limitation,
    `Data Limitation Imposed` = data_limit_imposed,
    `Identifying Address Information May Not Be Used for Research` = data_limit_no_address_research,
    `Other Data Limitation` = data_limit_other,
    `Data Limitation Summary` = data_limit_summary,
    `Biospecimen Limitation Imposed` = biospecimen_limit_imposed,
    `Biospecimens May Not Be Used for Drug Testing` = bio_limit_no_drug_testing,
    `Biospecimens May Not Be Used for Genetic Research` = bio_limit_no_genetic_research,
    `Biospecimens May Not Be Used for Genome Sequencing` = bio_limit_no_genome_sequencing,
    `Other Biospecimen Limitation` = bio_limit_other,
    `Other Biospecimen Limitation Detail` = limits_3aoth,
    `Biospecimen Limitation Summary` = biospecimen_limit_summary
  )


library(openxlsx)

output_path <- file.path(
  "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/Data-Manager/Random Task/Limitations and Withdraw Reports",
  "Redcap_Consent_Assent_Limitations_Cleaned.xlsx"
)

write.xlsx(
  limits_export,
  file = output_path,
  overwrite = TRUE
)



# 3. Withdraw -------------------------------------------------------------

#!/usr/bin/env Rscript
token <- "D199C7FD4EF4A61F17AB8DEFB6AFB1F2"
url <- "https://echoredcap.org/api/"
formData <- list("token"=token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 'fields[0]'='record_id',
                 'forms[0]'='adm_wthd',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)


# Helper function: safely convert date fields
parse_date_safe <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  } else {
    return(as.Date(parse_date_time(x, orders = c("ymd", "mdy"))))
  }
}

withdrawal_clean <- result %>%
  filter(adm_wthd_complete == 2 | adm_wthd_complete == "2") %>%
  mutate(
    # ---- ID fields ----
    record_id = as.character(record_id),
    
    # ---- Date fields ----
    withdrawal_or_discontinued_date = parse_date_safe(wthd_1),
    
    # ---- 1.a. Participant status ----
    participant_status = case_when(
      wthd_1a == 1 | wthd_1a == "1" ~ "Withdraws",
      wthd_1a == 2 | wthd_1a == "2" ~ "Can no longer be contacted by site",
      TRUE ~ NA_character_
    ),
    
    # ---- 1.a.1. Reason site can no longer contact participant ----
    lost_contact_reason = case_when(
      wthd_1a1 == 1 | wthd_1a1 == "1" ~ "Lost contact with participant after repeated efforts to reach them",
      wthd_1a1 == 2 | wthd_1a1 == "2" ~ "Unable to obtain parental permission after birth",
      wthd_1a1 == 3 | wthd_1a1 == "3" ~ "Unable to obtain parental permission after change in guardianship",
      wthd_1a1 == 4 | wthd_1a1 == "4" ~ "Permanent relocation outside of the U.S.",
      wthd_1a1 == 5 | wthd_1a1 == "5" ~ "Other, specify reason",
      TRUE ~ NA_character_
    ),
    
    # ---- 2. Circumstance for withdrawal ----
    circumstance_withdrew_consent = is_checked(wthd_2___1),
    circumstance_participant_died = is_checked(wthd_2___5),
    
    # ---- 3. Reason participant or parent/guardian withdrew consent ----
    reason_no_longer_interested = is_checked(wthd_3___1),
    reason_unable_to_devote_time = is_checked(wthd_3___2),
    reason_unhappy_with_participation = is_checked(wthd_3___3),
    reason_unable_to_travel = is_checked(wthd_3___4),
    reason_relocation = is_checked(wthd_3___5),
    reason_other = is_checked(wthd_3___6),
    
    # ---- 5. Method of withdrawal notification ----
    notification_in_person = is_checked(wthd_5___1),
    notification_phone = is_checked(wthd_5___2),
    notification_letter_email = is_checked(wthd_5___3),
    notification_other = is_checked(wthd_5___4)
  )

  
withdrawal_export <- withdrawal_clean %>%
  select(
    `Record ID` = record_id,
    
    `1. Date of withdrawal or contact attempts discontinued:` = withdrawal_or_discontinued_date,
    
    `1.a. Participant: [check one]` = participant_status,
    
    `1.a.1. If site can no longer contact participant, please select a reason` = lost_contact_reason,
    
    `Other reason site can no longer contact participant, specify:` = wthd_1a1_sp,
    
    `2. Circumstance for withdrawal: [check all that apply] (choice=Participant or parent/guardian withdrew consent)` = circumstance_withdrew_consent,
    
    `2. Circumstance for withdrawal: [check all that apply] (choice=Participant died)` = circumstance_participant_died,
    
    `3. Reason participant or parent/guardian withdrew consent [check all that apply] (choice=No longer interested in study)` = reason_no_longer_interested,
    
    `3. Reason participant or parent/guardian withdrew consent [check all that apply] (choice=Unable to devote time to the study)` = reason_unable_to_devote_time,
    
    `3. Reason participant or parent/guardian withdrew consent [check all that apply] (choice=Unhappy with study participation)` = reason_unhappy_with_participation,
    
    `3. Reason participant or parent/guardian withdrew consent [check all that apply] (choice=Unable to travel for study visits)` = reason_unable_to_travel,
    
    `3. Reason participant or parent/guardian withdrew consent [check all that apply] (choice=Relocation)` = reason_relocation,
    
    `3. Reason participant or parent/guardian withdrew consent [check all that apply] (choice=Other)` = reason_other,
    
    `Other, specify reason` = wthd_3_sp,
    
    `4. Details of the reasons for the withdrawal:` = wthd_4,
    
    `5. Method of withdrawal notification [check all that apply] (choice=In-person visit)` = notification_in_person,
    
    `5. Method of withdrawal notification [check all that apply] (choice=Phone)` = notification_phone,
    
    `5. Method of withdrawal notification [check all that apply] (choice=Letter or email)` = notification_letter_email,
    
    `5. Method of withdrawal notification [check all that apply] (choice=Other)` = notification_other,
    
    `Other, specify` = wthd_5_sp,
    
    `9. Comments:` = wthd_9
  )





# check -------------------------------------------------------------------

formData_check <- list(
  token = token,
  content = "record",
  action = "export",
  format = "csv",
  type = "flat",
  csvDelimiter = "",
  `records[0]` = "MTH981-01-0",
  `fields[0]` = "record_id",
  `fields[1]` = "wthd_formdt",
  `fields[2]` = "wthd_1",
  `fields[3]` = "wthd_1a",
  `fields[4]` = "adm_wthd_complete",
  rawOrLabel = "raw",
  rawOrLabelHeaders = "raw",
  exportCheckboxLabel = "false",
  exportSurveyFields = "false",
  exportDataAccessGroups = "false",
  returnFormat = "json"
)

response_check <- httr::POST(url, body = formData_check, encode = "form")

check_df <- httr::content(
  response_check,
  as = "text",
  encoding = "UTF-8"
) %>%
  readr::read_csv(show_col_types = FALSE)

check_df
