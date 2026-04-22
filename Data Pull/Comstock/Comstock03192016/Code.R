# The code is used to generate the summary for child growth measure for Sara Comstock


# 1. Setup ----------------------------------------------------------------
rm(list = ls())

library(readxl)
library(tidyverse)


# 2. Import Data ----------------------------------------------------------


base_path <- "Z:/ECHO/CHARM/Data/ECHO 2/2025 Nov Download"



# Import each dataset
cape_c1 <- read_csv(file.path(base_path, "dwForms_CPH_CAPE_C_C1.csv"))

clwt_0_23m <- read_csv(file.path(base_path, "dwForms_CPH_CLWt_0_23m.csv"))

chtwt_2_4y <- read_csv(file.path(base_path, "dwForms_CPH_CHtWt_2_4y.csv"))

chtwt_5_17y <- read_csv(file.path(base_path, "dwForms_CPH_CHtWt_5_17y.csv"))

id <- read_excel("C:/Users/tianjiah/OneDrive - Michigan State University/Data Manager/Data-Manager/Data Pull/Comstock/Comstock03192016/2026MAR05_PIDs to see if participating_HM.xlsx")

crosswalk_march <- read_excel("Z:/ECHO/CHARM/Data/Miscellaneous/Global Crosswalk/global_crosswalk.xlsx",sheet = 2)


# 3. Data Cleaning --------------------------------------------------------

## 3.1 ID map --------------------------------------------------------------

# Build MomID 
# Add prefix "P" to PID to create MomID
id_mapped <- id %>%
  mutate(
    PID = as.character(PID),
    MomID = paste0("P", PID)
  )

# Match Child_ECHO_ID 
# Keep only needed columns from crosswalk
id_child <- id_mapped %>%
  left_join(
    crosswalk_march %>%
      select(MomID, Child_ECHO_ID),
    by = "MomID"
  ) %>%
  mutate(Child_ECHO_ID = if_else(Child_ECHO_ID == "NA", NA_character_, as.character(Child_ECHO_ID)))

# there are 3 PID without Child_ECHO_ID



## 3.2 Weight --------------------------------------------------------------

# Keep valid measurement rows and standardize column names
clean_weight_form <- function(data, date_var, measure_vars, form_name) {
  data %>%
    select(DWIndividualID, all_of(date_var), all_of(measure_vars)) %>%
    filter(
      !if_all(all_of(measure_vars), ~ is.na(.)) # remove rows where all three repeated measurements are missing
    ) %>%
    transmute(
      DWIndividualID = as.character(DWIndividualID),
      Measure_date = as.Date(.data[[date_var]]), # keep formdate as measure date
      Form = form_name
    ) %>%
    distinct()
}

# Create one row per valid measurement event (date-level)
cape_c1_weight_dates <- clean_weight_form(
  data = cape_c1,
  date_var = "FormDT",
  measure_vars = c("cape_c_a1a", "cape_c_a1b", "cape_c_a1c"),
  form_name = "dwForms_CPH_CAPE_C_C1"
)

clwt_0_23m_weight_dates <- clean_weight_form(
  data = clwt_0_23m,
  date_var = "FormDT",
  measure_vars = c("clwt_0_23m_c1a", "clwt_0_23m_c1b", "clwt_0_23m_c1c"),
  form_name = "dwForms_CPH_CLWt_0_23m"
)

chtwt_2_4y_weight_dates <- clean_weight_form(
  data = chtwt_2_4y,
  date_var = "FormDT",   # Change if the real column name is different
  measure_vars = c("chtwt_2_4y_c1a", "chtwt_2_4y_c1b", "chtwt_2_4y_c1c"),
  form_name = "dwForms_CPH_CHtWt_2_4y"
)

chtwt_5_17y_weight_dates <- clean_weight_form(
  data = chtwt_5_17y,
  date_var = "FormDT",   # Change if the real column name is different
  measure_vars = c("chtwt_5_17y_c1a1", "chtwt_5_17y_c1b1", "chtwt_5_17y_c1c1"),
  form_name = "dwForms_CPH_CHtWt_5_17y"
)

# blind all weight forms
weight_dates_all <- bind_rows(
  cape_c1_weight_dates,
  clwt_0_23m_weight_dates,
  chtwt_2_4y_weight_dates,
  chtwt_5_17y_weight_dates
) %>%
  distinct()

# Match DWIndividualID to Child_ECHO_ID
weight_dates_matched <- weight_dates_all %>%
  inner_join(
    id_child %>%
      select(MomID, Child_ECHO_ID) %>%
      distinct() %>%
      mutate(Child_ECHO_ID = as.character(Child_ECHO_ID)),
    by = c("DWIndividualID" = "Child_ECHO_ID")
  ) %>%
  relocate(MomID, DWIndividualID, Measure_date, Form)

# Create visit order 
# Order dates within each child
weight_dates_long <- weight_dates_matched %>%
  arrange(DWIndividualID, Measure_date) %>%
  group_by(MomID, DWIndividualID) %>%
  mutate(
    measurement_number = row_number()
  ) %>%
  ungroup()

# Create summary count table 
# Count number of measurement dates per child
weight_count_summary <- weight_dates_long %>%
  group_by(MomID, DWIndividualID) %>%
  summarise(
    n_measurements = n(),
    .groups = "drop"
  )

# Pivot to wide table 
# Create columns: Measure_date1, Measure_date2, ...
weight_dates_wide <- weight_dates_long %>%
  transmute(
    MomID,
    Child_ECHO_ID = DWIndividualID,
    date_col = paste0("Measure_date", measurement_number),
    Measure_date = Measure_date
  ) %>%
  pivot_wider(
    names_from = date_col,
    values_from = Measure_date
  ) %>%
  left_join(weight_count_summary,
            by = c("MomID", "Child_ECHO_ID" = "DWIndividualID")) %>%
  relocate(MomID, Child_ECHO_ID, n_measurements)


final_weight_summary <- id_child %>%
  left_join(
    weight_dates_wide,
    by = c("MomID", "Child_ECHO_ID")
  ) %>%
  mutate(
    n_measurements = replace_na(n_measurements, 0)
  )

## 3.3 Height --------------------------------------------------------------

# Helper function: keep valid height measurement rows
clean_height_form <- function(data, date_var, measure_vars, form_name) {
  data %>%
    select(DWIndividualID, all_of(date_var), all_of(measure_vars)) %>%
    filter(
      !if_all(all_of(measure_vars), ~ is.na(.))
    ) %>%
    transmute(
      DWIndividualID = as.character(DWIndividualID),
      Measure_date = as.Date(.data[[date_var]]),
      Form = form_name
    ) %>%
    distinct()
}

# height date-level datasets
cape_c1_height_dates <- clean_height_form(
  data = cape_c1,
  date_var = "FormDT",
  measure_vars = c("cape_c_b1a", "cape_c_b1b", "cape_c_b1c"),
  form_name = "dwForms_CPH_CAPE_C_C1"
)

clwt_0_23m_height_dates <- clean_height_form(
  data = clwt_0_23m,
  date_var = "FormDT",
  measure_vars = c("clwt_0_23m_b1a", "clwt_0_23m_b1b", "clwt_0_23m_b1c"),
  form_name = "dwForms_CPH_CLWt_0_23m"
)

chtwt_2_4y_height_dates <- clean_height_form(
  data = chtwt_2_4y,
  date_var = "FormDT",
  measure_vars = c("chtwt_2_4y_b1a", "chtwt_2_4y_b1b", "chtwt_2_4y_b1c"),
  form_name = "dwForms_CPH_CHtWt_2_4y"
)

chtwt_5_17y_height_dates <- clean_height_form(
  data = chtwt_5_17y,
  date_var = "FormDT",
  measure_vars = c("chtwt_5_17y_b1a", "chtwt_5_17y_b1b", "chtwt_5_17y_b1c"),
  form_name = "dwForms_CPH_CHtWt_5_17y"
)

# bind all height forms
height_dates_all <- bind_rows(
  cape_c1_height_dates,
  clwt_0_23m_height_dates,
  chtwt_2_4y_height_dates,
  chtwt_5_17y_height_dates
) %>%
  distinct()

# Match DWIndividualID to Child_ECHO_ID
height_dates_matched <- height_dates_all %>%
  inner_join(
    id_child %>%
      select(MomID, Child_ECHO_ID) %>%
      distinct() %>%
      mutate(Child_ECHO_ID = as.character(Child_ECHO_ID)),
    by = c("DWIndividualID" = "Child_ECHO_ID")
  ) %>%
  relocate(MomID, DWIndividualID, Measure_date, Form)

# Create visit order
# Order dates within each child
height_dates_long <- height_dates_matched %>%
  arrange(DWIndividualID, Measure_date) %>%
  group_by(MomID, DWIndividualID) %>%
  mutate(
    measurement_number = row_number()
  ) %>%
  ungroup()

# Create summary count table
# Count number of measurement dates per child
height_count_summary <- height_dates_long %>%
  group_by(MomID, DWIndividualID) %>%
  summarise(
    n_measurements = n(),
    .groups = "drop"
  )

# Pivot to wide table
# Create columns: Measure_date1, Measure_date2, ...
height_dates_wide <- height_dates_long %>%
  transmute(
    MomID,
    Child_ECHO_ID = DWIndividualID,
    date_col = paste0("Measure_date", measurement_number),
    Measure_date = Measure_date
  ) %>%
  pivot_wider(
    names_from = date_col,
    values_from = Measure_date
  ) %>%
  left_join(
    height_count_summary,
    by = c("MomID", "Child_ECHO_ID" = "DWIndividualID")
  ) %>%
  relocate(MomID, Child_ECHO_ID, n_measurements)

# Join back to the full ID list
final_height_summary <- id_child %>%
  left_join(
    height_dates_wide,
    by = c("MomID", "Child_ECHO_ID")
  ) %>%
  mutate(
    n_measurements = replace_na(n_measurements, 0)
  )


# 4. Save Data ------------------------------------------------------------

library(writexl)

# Create a notes data frame to save as a separate sheet
notes_df <- tibble(
  Notes = c(
    "1. Data were prepared by Jiahe Tian on March 19, 2026.",
    "2. The originally provided PID was used to create MomID by adding prefix 'P'; MomID was then matched to Child_ECHO_ID to query growth measurement data.",
    "3. There were 3 MomIDs without a corresponding Child_ECHO_ID.",
    "4. Among 209 children, 88 had weight measurements and 89 had height measurements."
  )
)


# Save all outputs into one Excel workbook with multiple sheets
write_xlsx(
  list(
    Weight_Summary = final_weight_summary,
    Height_Summary = final_height_summary,
    Notes = notes_df
  ),
  path = "C:/Users/tianjiah/OneDrive - Michigan State University/Data Manager/Data-Manager/Data Pull/Comstock/Comstock03192016/growth_measure_summary.xlsx"
)
