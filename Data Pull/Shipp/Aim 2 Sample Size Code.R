# The code is used to calculate sample size for Shipp Aim 2

library(readxl)
library(dplyr)
library(janitor)


# 0. Crosswalk ------------------------------------------------------------

crosswalk_path <- "Z:/ECHO/CHARM/Data/Miscellaneous/Global Crosswalk/global_crosswalk.xlsx"
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


# 1. Diet Data ------------------------------------------------------------

# Read Prenatal Diet dataset
march_dietdata <- read_excel("Z:/ECHO/CHARM/Data/Code Derived/PN Dietary/PhenX and DSQ scores combined.xlsx")

march_dietdata <- march_dietdata %>%
  left_join(
    global_crosswalk_p1,
    by = c("SAMPLEID" = "MomID")
  )

# ---- 1. MARCH Diet: unique IDs 
march_diet_ids <- march_dietdata %>%
  mutate(
    Child_ECHO_ID = as.character(Child_ECHO_ID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

length(march_diet_ids) # 999

# 2. MARCH NIH Toolbox ----------------------------------------------------

# MARCH NIH Toolbox
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

march_ntb_mom_ids <- march_ntb_ids %>%
  as.character() %>%
  str_trim() %>%
  .[str_starts(., "P")] %>%
  unique()

# ---- 3) Map ChildID -> MomID using crosswalk 
march_ntb_child_echo_ids_from_child <- tibble(ChildID = march_ntb_child_ids) %>%
  left_join(global_crosswalk_p1, by = "ChildID") %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

march_ntb_child_echo_ids_from_mom <- tibble(MomID = march_ntb_mom_ids) %>%
  left_join(global_crosswalk_p1, by = "MomID") %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

# ---- 4) Map Child_ECHO_ID -> MomID using crosswalk
echo2_ntb_child_df <- echo2_ntb %>%
  mutate(
    Child_ECHO_ID = as.character(ParticipantID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  left_join(global_crosswalk_p1, by = "Child_ECHO_ID") 


# 1) MARCH: MomID starts with "P"
march_ntb_child_echo_ids_from_ECHO2 <- echo2_ntb_child_df %>%
  filter(
    !is.na(MomID),
    str_starts(MomID, "P")
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

# ---- 5) Final MARCH NTB MomID list (union of mapped + direct + echo2)  
march_ntb_child_echo_ids <- Reduce(
  union,
  list(
    march_ntb_child_echo_ids_from_child,
    march_ntb_child_echo_ids_from_mom,
    march_ntb_child_echo_ids_from_ECHO2
  )
)

length(intersect(march_ntb_child_echo_ids, march_diet_ids)) # 126

# 3. CBCL -----------------------------------------------------------------

get_overlap_n <- function(
    df,
    diet_biomarker_ids
) {
  
  # 1. Extract unique Child_ECHO_IDs directly from participantid
  child_echo_ids <- df %>%
    mutate(
      Child_ECHO_ID = as.character(participantid),
      Child_ECHO_ID = str_trim(Child_ECHO_ID)
    ) %>%
    filter(
      !is.na(Child_ECHO_ID),
      Child_ECHO_ID != "",
      Child_ECHO_ID != "NA"
    ) %>%
    distinct(Child_ECHO_ID) %>%
    pull(Child_ECHO_ID)
  
  # 2. Return overlap sample size (Child-level)
  print(length(intersect(child_echo_ids, diet_biomarker_ids)))
}

# CBCL_PRE
march_cbcl_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_cbcl_pre.csv"
)

get_overlap_n(
  df = march_cbcl_pre,
  diet_biomarker_ids = march_diet_ids
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
  diet_biomarker_ids = march_diet_ids
)

get_overlap_n(
  df = march_srs2_sch,
  diet_biomarker_ids = march_diet_ids
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
  diet_biomarker_ids = march_diet_ids
)

get_overlap_n(
  df = march_asq_36,
  diet_biomarker_ids = march_diet_ids
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
  left_join(
    global_crosswalk_p1,
    by = c("MomID" = "MomID")
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

length(march_asa24_ids) # 127

length(intersect(march_ntb_child_echo_ids, march_asa24_ids)) # 24


get_overlap_n(
  df = march_cbcl_pre,
  diet_biomarker_ids = march_asa24_ids
)

get_overlap_n(
  df = march_srs2_pre,
  diet_biomarker_ids = march_asa24_ids
)

get_overlap_n(
  df = march_srs2_sch,
  diet_biomarker_ids = march_asa24_ids
)

get_overlap_n(
  df = march_asq_9_combined,
  diet_biomarker_ids = march_asa24_ids
)

get_overlap_n(
  df = march_asq_36,
  diet_biomarker_ids = march_asa24_ids
)
