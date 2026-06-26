# The code is used to calculate sample size for Shipp Aim 2

library(readxl)
library(tidyverse)
library(janitor)


# 0. Crosswalk ------------------------------------------------------------

BASE_PATH <- case_when(
  Sys.info()["sysname"] == "Windows" ~ "Z:/ECHO/CHARM",
  Sys.info()["sysname"] == "Darwin"  ~ "/Volumes/Groups/ECHO/CHARM"
)

get_path <- function(relative_path) {
  file.path(BASE_PATH, relative_path)
}


crosswalk_path <- get_path("Data/Miscellaneous/Global Crosswalk/global_crosswalk.xlsx")
global_crosswalk <- read_excel(crosswalk_path, sheet = 1)

# MomIDs to Child_ECHO_ID mapping
global_crosswalk_p1 <- global_crosswalk %>%
  mutate(
    Child_ECHO_ID = str_trim(as.character(Child_ECHO_ID)),
    ChildID = str_trim(as.character(ChildID)),
    MomID         = str_trim(as.character(MomID))
  ) %>%
  select(Child_ECHO_ID, ChildID, MomID) %>%
  distinct()


# 1. Prenatal Diet Data ------------------------------------------------------------

# Read ARCH diet dataset
arch_dietdata <- read_excel(get_path("Data/ECHO 1/ARCH Enrollment Data/Diet.xlsx"))

arch_dietdata <- arch_dietdata %>%
  mutate(
    SUBJECT_Id = as.character(SUBJECT_Id),
    SUBJECT_Id = str_trim(SUBJECT_Id)
  ) %>%
  left_join(
    global_crosswalk_p1,
    by = c("SUBJECT_Id" = "MomID")
  )

# n = 371
arch_diet_ids <- arch_dietdata %>%
  mutate(
    Child_ECHO_ID = as.character(Child_ECHO_ID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)


# 2. ARCH Childhood Diet -------------------------------------------------------

# ---- ECHO 1: Derived Forms (BLOCK) 
# 12901 for ARCH, 12902 for MARCH
ARCH_block1 <- read.csv(
  get_path("Data/ECHO 1/Derived Forms/Cohort_12901/forms_Ess_CHB_BLOCK.csv")
)

ARCH_block2 <- read.csv(
  get_path("Data/ECHO 1/Derived Forms/Cohort_12901/forms_Ess_CHB_BLOCK2.csv")
)

# n = 83
arch_block_ids <- ARCH_block1 %>%
  mutate(
    ParticipantID = as.character(ParticipantID),
    ParticipantID = str_trim(ParticipantID)
  ) %>%
  filter(!is.na(ParticipantID), ParticipantID != "") %>%
  distinct(ParticipantID) %>%
  pull(ParticipantID)


# 3. ARCH CBCL PRE -------------------------------------------------------------

# ARCH CBCL PRE
arch_cbcl_pre <- read.csv(
  get_path("Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_cnh_cbcl_pre.csv")
)

arch_cbcl_pre <- read.csv(
  get_path('Data/ECHO 1/Derived Forms/Cohort_12901/forms_Ess_CNH_CBCL_Pre.csv')
)
  
# arch_cbcl_pre
get_overlap_n(
  df = arch_cbcl_pre,
  diet_biomarker_ids = arch_diet_any_ids
)


# 4. ARCH CBCL SCH --------------------------------------------------------

arch_cbcl_sch <- read.csv(
  get_path('Data/ECHO 1/Derived Forms/Cohort_12901/forms_Ess_CNH_CBCL_Sch.csv')
)
