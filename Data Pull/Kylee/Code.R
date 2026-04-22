
# 1. Load Libraries -------------------------------------------------------

library(tidyverse)
library(openxlsx)

# 2. Import Data ----------------------------------------------------------

prenatal_and_month0to5 <- read_csv("C:/Users/tianjiah/OneDrive - Michigan State University/Data Manager/Data-Manager/Data Pull/Kylee/Prenatal.csv")
month6to35 <- read_csv("C:/Users/tianjiah/OneDrive - Michigan State University/Data Manager/Data-Manager/Data Pull/Kylee/Month6to35.csv")
year3to17 <- read_csv("C:/Users/tianjiah/OneDrive - Michigan State University/Data Manager/Data-Manager/Data Pull/Kylee/Year3to17.csv")

# 3. Data Manipulation ----------------------------------------------------


## 3.1 Split prenatal and 0-5 months ---------------------------------------


prenatal_and_month0to5 <- prenatal_and_month0to5 %>%
  select(globalId, customId,familyId, firstName, lastName, birthday, sex,
         event.child_date_of_birth.completed, event.child_date_of_birth.completedDate,
         event.child_is_born.completed, event.child_is_born.completedDate,
         cv.estimated_due_date,
         cv.child_sex,
         cv.multiple_births,
         cv.consent_birth_certificate)

# revise ID
prenatal_and_month0to5 <- prenatal_and_month0to5 %>%
  mutate(
    # if globalId does not start with uppercase letter, swap with customId
    temp_globalId = if_else(!grepl("^[A-Z]", globalId), customId, globalId),
    temp_customId = if_else(!grepl("^[A-Z]", globalId), globalId, customId),
    globalId = temp_globalId,
    customId = temp_customId
  ) %>%
  select(-temp_globalId, -temp_customId) %>%
  mutate(familyId = as.character(familyId))

prenatal_and_month0to5 <- prenatal_and_month0to5 %>%
  mutate(
    ECHO_ID = str_trim(str_extract(globalId, "^[^\\(]+")),
    PIN = str_extract(globalId, "(?<=\\()[0-9]+(?=\\))")
  )

prenatal <- prenatal_and_month0to5 %>%
  filter(str_ends(ECHO_ID, "0"))  # keep rows where ECHO_ID ends with "0"

month0to5 <- prenatal_and_month0to5 %>%
  filter(!str_ends(ECHO_ID, "0")) # keep rows where ECHO_ID does NOT end with "0"


## 3.2 Rename variables ----------------------------------------------------


prenatal_givebirth <- prenatal %>%
  filter(event.child_is_born.completed == "TRUE") %>%
  select(globalId,
         `Family ID` = familyId,
         `Mother First Name` = firstName,
         `Mother Last Name` = lastName,
         `Mother DOB` = birthday,
         `Twins/Multiples (Yes/No)` = cv.multiple_births,
         `Consent to Birth Certificate` = cv.consent_birth_certificate)

month0to5 <- month0to5 %>%
  select(globalId,
         `Family ID` = familyId,
         `Child DOB` = birthday,
         `Child Sex` = sex) 

month6to35 <- month6to35 %>%
  select(globalId,
         `Family ID` = familyId,
         `Child DOB` = birthday,
         `Child Sex` = sex) %>%
  mutate(`Family ID` = as.character(`Family ID`))

year3to17 <- year3to17 %>%
  mutate(familyId = as.character(familyId)) %>% 
  select(
    globalId,
    `Family ID` = familyId,
    `Child DOB` = birthday,
    `Child Sex` = sex
  )

class(month0to5$`Family ID`)

child_all <- bind_rows(month0to5, month6to35, year3to17) %>%
  distinct(globalId, `Family ID`, `Child DOB`, `Child Sex`, .keep_all = TRUE) %>%
  rename(childID = globalId)

# 4. Merge Datasets -------------------------------------------------------



merged_data <- prenatal_givebirth %>%
  left_join(child_all, by = "Family ID") %>%
  select(
    globalId,
    `Family ID`,
    `Mother First Name`,
    `Mother Last Name`,
    `Mother DOB`,
    childID,
    `Child DOB`,
    `Child Sex`,
    `Twins/Multiples (Yes/No)`,
    `Consent to Birth Certificate`
  )

merged_data <- merged_data %>%
  select(
    -globalId,
    -childID
  )


# 5. Save data ------------------------------------------------------------
# 
output_path <- "C:/Users/tianjiah/OneDrive - Michigan State University/Data Manager/Data-Manager/Data Pull/Kylee/TFD_Birth_Certificate.xlsx"

# 
write.xlsx(merged_data, file = output_path, sheetName = "Merged Data", overwrite = TRUE)

# 
cat("✅ File saved successfully to:\n", output_path, "\n")
