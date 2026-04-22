# The code is used to calculate sample size for Shipp Aim 2

library(readxl)
library(tidyverse)
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

# Read ARCH diet dataset
arch_dietdata <- read_excel("Z:/ECHO/CHARM/Data/ECHO 1/ARCH Enrollment Data/Diet.xlsx")

arch_dietdata <- arch_dietdata %>%
  mutate(
    SUBJECT_Id = as.character(SUBJECT_Id),
    SUBJECT_Id = str_trim(SUBJECT_Id)
  ) %>%
  left_join(
    global_crosswalk_p1,
    by = c("SUBJECT_Id" = "MomID")
  )

arch_diet_ids <- arch_dietdata %>%
  mutate(
    Child_ECHO_ID = as.character(Child_ECHO_ID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

# Read Prenatal Diet dataset
march_dietdata <- read_excel("Z:/ECHO/CHARM/Data/Code Derived/PN Dietary/PhenX and DSQ scores combined.xlsx")

march_dietdata <- march_dietdata %>%
  left_join(
    global_crosswalk_p1,
    by = c("SAMPLEID" = "MomID")
  )

march_diet_ids <- march_dietdata %>%
  mutate(
    Child_ECHO_ID = as.character(Child_ECHO_ID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

# 2. Infant Feeding Practice ----------------------------------------------
march_ifp <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_chb_ifp.csv"
)

# ---- Infant Feeding Practices (IW, Phase 2 – current) 
ifp_iw_phase2 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_2/Phase2_3month_Data_Delivery/IW/echo3mo_data_all_extra_MixedFormats.csv"
)

# ---- Infant Feeding Practices (Prior IW – older instrument) 
ifp_iw_prior <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_2/Phase2_3month_Data_Delivery/Prior_IW_Data_Older_Instrument/echo3mo_data_all_extra_mixedformats_03012023.csv"
)

# ---- 1) Keep only SAMPLEID + BRST_MILK and stack
ifp_breastmilk <- bind_rows(
  ifp_iw_phase2 %>%
    select(SAMPLEID, BRST_MILK) %>%
    mutate(source = "phase2"),
  
  ifp_iw_prior %>%
    select(SAMPLEID, BRST_MILK) %>%
    mutate(source = "prior")
) %>%
  mutate(
    SAMPLEID = as.character(SAMPLEID),
    SAMPLEID = str_trim(SAMPLEID)
  ) %>%
  filter(!is.na(SAMPLEID), SAMPLEID != "") %>%
  left_join(global_crosswalk_p1, by = c("SAMPLEID" = "ChildID")) %>% 
  filter(Child_ECHO_ID != "NA", !is.na(Child_ECHO_ID))


# ---- 3) Extract unique MomIDs with BRST_MILK available (optional filter)
ifp_breastmilk_child_echo_ids <- ifp_breastmilk %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)


# ---- 4) Overlap with march_diet_ids
march_diet_ifp_ids <- intersect(ifp_breastmilk_child_echo_ids, march_diet_ids) # 736

length(march_diet_ifp_ids)

# 3. Complimentary Feeding History ----------------------------------------

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

get_overlap_list <- function(
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
  return(intersect(child_echo_ids, diet_biomarker_ids))
}



march_cfh <- read.csv(
"Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_chb_cfh.csv"
)

get_overlap_n(
  df = march_cfh,
  diet_biomarker_ids = march_diet_ids
)

march_diet_cfh_ids <- get_overlap_list(
  df = march_cfh,
  diet_biomarker_ids = march_diet_ids
)
# 4. Block ----------------------------------------------------------------

# ---- ECHO 1: Derived Forms (BLOCK) 
# 12901 for ARCH, 12902 for MARCH
block_12901_block1 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/Derived Forms/Cohort_12901/forms_Ess_CHB_BLOCK.csv"
)

block_12901_block2 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/Derived Forms/Cohort_12901/forms_Ess_CHB_BLOCK2.csv"
)

block_12902_block1 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/Derived Forms/Cohort_12902/forms_Ess_CHB_BLOCK.csv"
)

# ---- ECHO 2: Download (BLOCK) 
block_echo2_block1 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 2/2025 Nov Download/dwForms_CHB_BLOCK_C1.csv"
)

block_echo2_block2 <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 2/2025 Nov Download/dwForms_CHB_BLOCK2_C1.csv"
)



# ---- ARCH BLOCK: from block_12901_block1_mom  
arch_block_ids <- block_12901_block1 %>%
  mutate(
    ParticipantID = as.character(ParticipantID),
    ParticipantID = str_trim(ParticipantID)
  ) %>%
  filter(!is.na(ParticipantID), ParticipantID != "") %>%
  distinct(ParticipantID) %>%
  pull(ParticipantID)


# ---- MARCH BLOCK: union of block_12902_block1_mom and block_echo2_block1_mom  
march_block_ids_1 <- block_12902_block1 %>%
  mutate(
    ParticipantID = as.character(ParticipantID),
    ParticipantID = str_trim(ParticipantID)
  ) %>%
  filter(!is.na(ParticipantID), ParticipantID != "") %>%
  distinct(ParticipantID) %>%
  pull(ParticipantID)

march_block_ids_2 <- block_echo2_block1 %>%
  mutate(
    ParticipantID = as.character(ParticipantID),
    ParticipantID = str_trim(ParticipantID)
  ) %>%
  filter(!is.na(ParticipantID), ParticipantID != "") %>%
  distinct(ParticipantID) %>%
  pull(ParticipantID)

march_block_ids <- union(march_block_ids_1, march_block_ids_2)

arch_diet_block_ids <- intersect(arch_block_ids, arch_diet_ids) # 82
length(arch_diet_block_ids)

march_diet_block_ids <- intersect(march_block_ids, march_diet_ids) # 107
length(march_diet_block_ids)

# 5. Childhood Outcome ----------------------------------------------------


## 5.0 Prepare List --------------------------------------------------------

arch_diet_any_ids <- arch_diet_block_ids

# Combine all IDs with diet and any of the feeding data, n = 549
march_diet_any_ids <- Reduce(
  union,
  list(
    march_diet_ifp_ids,
    march_diet_cfh_ids,
    march_diet_block_ids
  )
)

## 5.1. MARCH NIH Toolbox ----------------------------------------------------


### 5.1.1 ARCH NIH Toolbox --------------------------------------------------

# ARCH RAIND NIH Toolbox
arch_ntb_raind <- read_excel(
  "Z:/ECHO/CHARM/Data/NIH Toolbox Participants/ARCH RAIND Child Development Data 5.23.16 FINAL.xlsx",
  sheet = 2,
  range = cell_cols("A:B")
)


# Prepare ARCH RAIND NIH Toolbox IDs
arch_ntb_raind_fixed <- arch_ntb_raind %>%
  mutate(
    # ---- 1. MotherID as character (keep leading zeros for child construction) ----
    MotherID_chr = MotherID %>%
      as.character() %>%
      str_pad(width = 4, side = "left", pad = "0"),
    
    # ---- 2. Construct correct MomID (remove leading zeros) ----
    MomID = MotherID_chr %>%
      str_remove("^0+") %>%
      if_else(. == "", "0", .),   # safety for "0000"
    
    # ---- 3. Construct correct ChildID ----
    ChildID_correct = case_when(
      str_ends(ChildID, "_A") ~ paste0("7", MotherID_chr),
      str_ends(ChildID, "_B") ~ paste0("8", MotherID_chr),
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    global_crosswalk_p1,
    by = c("ChildID_correct" = "ChildID", "MomID" = "MomID")
  )


# mixed with ECHO_momID and ECHO_childID
arch_ntb_ids <- arch_ntb %>%
  mutate(
    PIN = as.character(PIN),
    PIN = str_trim(PIN)
  ) %>%
  filter(!is.na(PIN)) %>%
  distinct(PIN) %>%
  pull(PIN)


# ARCH NIH Toolbox
arch_ntb <- read_excel(
  "Z:/ECHO/CHARM/Data/NIH Toolbox Participants/ARCH NIHTB July 2023.xlsx",
  sheet = 2
)

# mixed with ECHO_momID and ECHO_childID
arch_ntb_ids <- arch_ntb %>%
  mutate(
    PIN = as.character(PIN),
    PIN = str_trim(PIN)
  ) %>%
  filter(!is.na(PIN)) %>%
  distinct(PIN) %>%
  pull(PIN)

# ---- 2. Prepare ARCH NIH Toolbox IDs 
arch_ntb_df <- tibble(PIN = arch_ntb_ids) %>%
  mutate(
    PIN = as.character(PIN),
    PIN = str_trim(PIN),
    # Identify ID type by last character
    id_type = if_else(str_ends(PIN, "0"), "mom", "child")
  ) %>%
  filter(id_type == "child")

# ---- 3. Combine and extract unique MomID  
arch_ntb_child_echo_ids <- union(
  arch_ntb_raind_fixed %>% 
    filter(Child_ECHO_ID != "NA") %>%
    distinct(Child_ECHO_ID) %>%
    pull(Child_ECHO_ID),
  arch_ntb_df %>%
    mutate(
      Child_ECHO_ID = PIN
    ) %>%
    distinct(Child_ECHO_ID) %>%
    pull(Child_ECHO_ID)
)

length(intersect(arch_ntb_child_echo_ids, arch_diet_any_ids)) # 3

### 5.1.2 MARCH NIH Toolbox -------------------------------------------------

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


# 2) ARCH: MomID does NOT start with "P"
arch_ntb_child_echo_ids_from_ECHO2 <- echo2_ntb_child_df %>%
  filter(
    !is.na(MomID),
    !str_starts(MomID, "P")
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

length(march_ntb_child_echo_ids) # 130

length(intersect(march_ntb_child_echo_ids, march_diet_any_ids)) # 124

# update ARCH
arch_ntb_child_echo_ids_update <- union(arch_ntb_child_echo_ids, arch_ntb_child_echo_ids_from_ECHO2 )

# check the overlap
length(intersect(arch_diet_any_ids, arch_ntb_child_echo_ids_update)) #38

## 5.2. CBCL -----------------------------------------------------------------

### 5.2.1 ARCH CBCL --------------------------------------------------------

# CBCL PRE
arch_cbcl_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_cnh_cbcl_pre.csv"
)

# arch_cbcl_pre
get_overlap_n(
  df = arch_cbcl_pre,
  diet_biomarker_ids = arch_diet_any_ids
)

### 5.2.2 MARCH CBCL --------------------------------------------------------


# CBCL_PRE
march_cbcl_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_cbcl_pre.csv"
)

# march_cbcl_pre
get_overlap_n(
  df = march_cbcl_pre,
  diet_biomarker_ids = march_diet_any_ids
)

## 5.3 SRS-2 ----------------------------------------------------------------

### 5.3.1 ARCH SRS-2 -------------------------------------------------------

arch_srs2_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_cnh_srs2_pre.csv"
)

arch_srs2_sch <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_cnh_srs2_sch.csv"
)

# arch_srs2_pre, n = 27
get_overlap_n(
  df = arch_srs2_pre,
  diet_biomarker_ids = arch_diet_any_ids
)

# arch_srs2_sch ,n = 61
get_overlap_n(
  df = arch_srs2_sch,
  diet_biomarker_ids = arch_diet_any_ids
)

### 5.3.2 MARCH SRS-2 -------------------------------------------------------

# ---- MARCH SRS-2
march_srs2_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_srs2_pre.csv"
)

march_srs2_sch <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_srs2_sch.csv"
)


get_overlap_n(
  df = march_srs2_pre,
  diet_biomarker_ids = march_diet_any_ids
)

get_overlap_n(
  df = march_srs2_sch,
  diet_biomarker_ids = march_diet_any_ids
)

## 5.4 Ages and Stages -----------------------------------------------------


# ---- MARCH ASQ 

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


# ---- Extract participantid from each ASQ dataset  
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

# ---- Union of ASQ 9 / 10 / 12  
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
  diet_biomarker_ids = march_diet_any_ids
)

get_overlap_n(
  df = march_asq_36,
  diet_biomarker_ids = march_diet_any_ids
)


