# The code is used to calculate sample size for Shipp Aim 2

library(readxl)
library(dplyr)
library(janitor)


# 0. Crosswalk ------------------------------------------------------------

crosswalk_path <- "Z:/ECHO/CHARM/Data/Miscellaneous/Global Crosswalk/global_crosswalk.xlsx"
global_crosswalk <- read_excel(crosswalk_path, sheet = 1)

# Prepare global crosswalk specimen dataframe
global_crosswalk_specimen <- global_crosswalk %>%
  mutate(
    SpecimenID = Specimen_ID %>%
      as.character() %>%
      str_trim() %>%
      str_pad(width = 4, side = "left", pad = "0")
  ) %>%
  select(SpecimenID, MomID)

global_crosswalk_p <- global_crosswalk %>%
  mutate(
    Mom_ECHO_ID   = str_trim(as.character(Mom_ECHO_ID)),
    Child_ECHO_ID = str_trim(as.character(Child_ECHO_ID)),
    MomID         = str_trim(as.character(MomID))
  ) %>%
  select(Mom_ECHO_ID, Child_ECHO_ID, MomID) %>%
  distinct()

global_crosswalk_p2 <- global_crosswalk %>%
  mutate(
    ChildID = str_trim(as.character(ChildID)),
    MomID   = str_trim(as.character(MomID))
  ) %>%
  select(ChildID, MomID) %>%
  distinct()

# 1. Diet Data ------------------------------------------------------------

# Read Prenatal Diet dataset
march_dietdata <- read_excel("Z:/ECHO/CHARM/Data/Code Derived/PN Dietary/PhenX and DSQ scores combined.xlsx")

march_diet_ids <- march_dietdata %>%
  mutate(
    SAMPLEID = as.character(SAMPLEID),
    SAMPLEID = str_trim(SAMPLEID)
  ) %>%
  filter(!is.na(SAMPLEID), SAMPLEID != "") %>%
  distinct(SAMPLEID) %>%
  pull(SAMPLEID)

# 2. MARCH NIH Toolbox ----------------------------------------------------

march_ntb <- read_excel(
  "Z:/ECHO/CHARM/Data/ECHO 1/NIH TOOLBOX/MARCH NIHTB July 2023 .xlsx",
  sheet = 2
)

# ECHO 2 NIH Toolbox
echo2_ntb <- read_excel("Z:/ECHO/CHARM/Data/ECHO 2/NIH Toolbox Report/NIH Toolbox report Dec2025.xlsx")

# ---- 1) Unique PINs from MARCH NTB 
march_ntb_ids <- march_ntb %>%
  mutate(
    PIN = as.character(PIN),
    PIN = str_trim(PIN)
  ) %>%
  filter(!is.na(PIN), PIN != "") %>%
  distinct(PIN) %>%
  pull(PIN)

# ---- 2) Split into ChildIDs and MomIDs based on rules  
march_ntb_child_ids <- march_ntb_ids %>%
  as.character() %>%
  str_trim() %>%
  .[str_detect(., "M")] %>%
  unique()

march_ntb_mom_ids_direct <- march_ntb_ids %>%
  as.character() %>%
  str_trim() %>%
  .[str_starts(., "P")] %>%
  unique()

# ---- 3) Map ChildID -> MomID using crosswalk 
march_ntb_child_mom <- tibble(ChildID = march_ntb_child_ids) %>%
  left_join(global_crosswalk_p2, by = "ChildID")

march_ntb_mom_ids_from_child <- march_ntb_child_mom %>%
  filter(!is.na(MomID)) %>%
  distinct(MomID) %>%
  pull(MomID)

# ---- 4) Map Child_ECHO_ID -> MomID using crosswalk
echo2_ntb_child_ids <- echo2_ntb %>%
  mutate(
    Child_ECHO_ID = as.character(ParticipantID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  left_join(global_crosswalk_p, by = "Child_ECHO_ID") %>%
  filter(!is.na(MomID), MomID != "") %>%
  distinct(MomID) %>%
  pull(MomID)

# ---- 5) Final MARCH NTB MomID list (union of mapped + direct + echo2)  
march_ntb_mom_ids <- Reduce(
  union,
  list(
    march_ntb_mom_ids_from_child,
    march_ntb_mom_ids_direct,
    echo2_ntb_child_ids
  )
)

length(intersect(march_ntb_mom_ids, march_diet_ids)) # 49

# 3. CBCL -----------------------------------------------------------------

get_overlap_n <- function(
    df,
    diet_biomarker_ids,
    crosswalk_df
) {
  
  # 1. Extract unique Child_ECHO_IDs
  child_ids <- df %>%
    mutate(
      participantid = as.character(participantid),
      participantid = str_trim(participantid)
    ) %>%
    filter(!is.na(participantid), participantid != "") %>%
    distinct(participantid) %>%
    pull(participantid)
  
  # 2. Map Child_ECHO_ID -> MomID
  child_mom <- tibble(
    Child_ECHO_ID = child_ids
  ) %>%
    mutate(
      Child_ECHO_ID = as.character(Child_ECHO_ID),
      Child_ECHO_ID = str_trim(Child_ECHO_ID)
    ) %>%
    left_join(
      crosswalk_df,
      by = "Child_ECHO_ID"
    )
  
  # 3. Extract unique MomIDs
  mom_ids <- child_mom %>%
    filter(!is.na(MomID)) %>%
    distinct(MomID) %>%
    pull(MomID)
  
  # 4. Return overlap sample size only
  print(length(intersect(mom_ids, diet_biomarker_ids)))
}

# CBCL_PRE
march_cbcl_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_cbcl_pre.csv"
)

get_overlap_n(
  df = march_cbcl_pre,
  diet_biomarker_ids = march_diet_ids,
  crosswalk_df = global_crosswalk_p
)

# 4. SRS-2 ----------------------------------------------------------------


# ---- MARCH SRS-2
march_srs2_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_srs2_pre.csv"
)

march_srs2_sch <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_srs2_sch.csv"
)


get_overlap_n(
  df = march_srs2_pre,
  diet_biomarker_ids = march_diet_ids,
  crosswalk_df = global_crosswalk_p
)

get_overlap_n(
  df = march_srs2_sch,
  diet_biomarker_ids = march_diet_ids,
  crosswalk_df = global_crosswalk_p
)

# 5. Ages and Stages -----------------------------------------------------



# ---- MARCH ASQ ----

march_asq_9 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_asq_9.csv"
)

march_asq_10 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_asq_10.csv"
)

march_asq_12 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_asq_12.csv"
)

march_asq_36 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_asq_36.csv"
)


# ---- Extract participantid from each ASQ dataset ----
asq_9_ids <- march_asq_9 %>%
  mutate(
    participantid = as.character(participantid),
    participantid = str_trim(participantid)
  ) %>%
  filter(!is.na(participantid), participantid != "") %>%
  distinct(participantid) %>%
  pull(participantid)

asq_10_ids <- march_asq_10 %>%
  mutate(
    participantid = as.character(participantid),
    participantid = str_trim(participantid)
  ) %>%
  filter(!is.na(participantid), participantid != "") %>%
  distinct(participantid) %>%
  pull(participantid)

asq_12_ids <- march_asq_12 %>%
  mutate(
    participantid = as.character(participantid),
    participantid = str_trim(participantid)
  ) %>%
  filter(!is.na(participantid), participantid != "") %>%
  distinct(participantid) %>%
  pull(participantid)

# ---- Union of ASQ 9 / 10 / 12 ----
march_asq_9_combined_ids <- Reduce(
  union,
  list(asq_9_ids, asq_10_ids, asq_12_ids)
)

# Create a minimal dataframe for reuse in get_overlap_n()
march_asq_9_combined <- tibble(
  participantid = march_asq_9_combined_ids
)

get_overlap_n(
  df = march_asq_9_combined,
  diet_biomarker_ids = march_diet_ids,
  crosswalk_df = global_crosswalk_p
)

get_overlap_n(
  df = march_asq_36,
  diet_biomarker_ids = march_diet_ids,
  crosswalk_df = global_crosswalk_p
)


# 6. ASA 24 ---------------------------------------------------------------

march_asa24_crosswalk <- read_excel(
  "Z:/ECHO/CHARM/Data/ECHO 1/ASA24/ASA24 Crosswalk.xlsx"
)

march_asa24_ids <- march_asa24_crosswalk %>%
  mutate(
    MomID = as.character(`Study ID`),
    MomID = str_trim(MomID)
  ) %>%
  filter(!is.na(MomID), MomID != "") %>%
  distinct(MomID) %>%
  pull(MomID)

length(intersect(march_ntb_mom_ids, march_asa24_ids)) # 24


get_overlap_n(
  df = march_cbcl_pre,
  diet_biomarker_ids = march_asa24_ids,
  crosswalk_df = global_crosswalk_p
)

get_overlap_n(
  df = march_srs2_pre,
  diet_biomarker_ids = march_asa24_ids,
  crosswalk_df = global_crosswalk_p
)

get_overlap_n(
  df = march_srs2_sch,
  diet_biomarker_ids = march_asa24_ids,
  crosswalk_df = global_crosswalk_p
)

get_overlap_n(
  df = march_asq_9_combined,
  diet_biomarker_ids = march_asa24_ids,
  crosswalk_df = global_crosswalk_p
)

get_overlap_n(
  df = march_asq_36,
  diet_biomarker_ids = march_asa24_ids,
  crosswalk_df = global_crosswalk_p
)
