# The code is used to calculate sample size for Shipp Aim 1 
# And the code doesn't consider urine samples

library(readxl)
library(tidyverse)
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

# Prepare global crosswalk participant dataframe
global_crosswalk_p <- global_crosswalk %>%
  mutate(
    Mom_ECHO_ID   = str_trim(as.character(Mom_ECHO_ID)),
    Child_ECHO_ID = str_trim(as.character(Child_ECHO_ID)),
    MomID         = str_trim(as.character(MomID))
  ) %>%
  select(Mom_ECHO_ID, Child_ECHO_ID, MomID) %>%
  distinct()

# Prepare global crosswalk participant dataframe for MARCH
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

# Count unique participants based on sampleid
march_dietdata %>%
  summarise(
    n_unique_sampleid = n_distinct(SAMPLEID, na.rm = TRUE)
  ) # 1027

# Read ARCH diet dataset
arch_dietdata <- read_excel("Z:/ECHO/CHARM/Data/ECHO 1/ARCH Enrollment Data/Diet.xlsx")

# Count unique participants based on sampleid
arch_dietdata %>%
  summarise(
    n_unique_sampleid = n_distinct(SUBJECT_Id, na.rm = TRUE)
  ) # 409



# 2. Thyroid --------------------------------------------------------------

arch_thyroid <- read.csv(
  "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Thyroid/ARCH_Thyroid_Function_Data.csv"
  )

# MARCH Throid, column named "Tyr"
march_thyroid <- read_excel("Z:/ECHO/CHARM/Data/ECHO 1/MDHHS Data/2023 MDHHS Program Data/NBS/MARCH data_bloodspot.xlsx")


# 3. Iodine ---------------------------------------------------------------

# ARCH
arch_iodine <- read.csv(
  "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Iodine/ARCH_Iodine_ECHOUpload.csv"
)

# MARCH
march_iodine <- read_excel(
  "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Iodine/12902_EC0376_EM20-011_IODINE_HHEAR/Iodine_ShippingManifest.xlsx"
)



# 5. Aim 1 sample size check ----------------------------------------------


## 5.1 ARCH ----------------------------------------------------------------

# ---- 1. ARCH Diet: unique IDs 
arch_diet_ids <- arch_dietdata %>%
  mutate(
    SUBJECT_Id = as.character(SUBJECT_Id),
    SUBJECT_Id = str_trim(SUBJECT_Id)
  ) %>%
  filter(!is.na(SUBJECT_Id)) %>%
  distinct(SUBJECT_Id) %>%
  pull(SUBJECT_Id)

# ---- 2. ARCH Thyroid: unique IDs 
arch_thyroid_ids <- arch_thyroid %>%
  mutate(
    arch_id = as.character(arch_id),
    arch_id = str_trim(arch_id)
  ) %>%
  filter(!is.na(arch_id)) %>%
  distinct(arch_id) %>%
  pull(arch_id)


length(intersect(arch_diet_ids, arch_thyroid_ids)) # 0

# ---- 3. ARCH Iodine: unique IDs
arch_iodine_ids <- arch_iodine %>%
  mutate(
    arch_id = as.character(arch_id),
    arch_id = str_trim(arch_id)
  ) %>%
  filter(!is.na(arch_id)) %>%
  distinct(arch_id) %>%
  pull(arch_id)

arch_diet_iodine_ids <- intersect(arch_diet_ids, arch_iodine_ids) # 90


# 5.2 MARCH ---------------------------------------------------------------

# ---- 1. ARCH Diet: unique IDs 
march_diet_ids <- march_dietdata %>%
  mutate(
    SAMPLEID = as.character(SAMPLEID),
    SAMPLEID = str_trim(SAMPLEID)
  ) %>%
  filter(!is.na(SAMPLEID)) %>%
  distinct(SAMPLEID) %>%
  pull(SAMPLEID)

# ---- 2. ARCH Thyroid: unique IDs 
march_thyroid_ids <- march_thyroid %>%
  mutate(
    march_id = as.character(march_id),
    march_id = str_trim(march_id)
  ) %>%
  filter(!is.na(march_id)) %>%
  distinct(march_id) %>%
  pull(march_id)

march_diet_thyroid_ids <- intersect(march_diet_ids, march_thyroid_ids) #598

# ---- 3. MARCH Iodine: unique IDs
march_iodine_parsed <- march_iodine %>%
  mutate(
    # Extract SpecimenID: characters after position 4
    SpecimenID = substr(`Original Sample ID`, 5, nchar(`Original Sample ID`)),
    
    # Standardize format
    SpecimenID = as.character(SpecimenID),
    SpecimenID = str_trim(SpecimenID)
  )

# get momID
march_iodine_with_momid <- march_iodine_parsed %>%
  left_join(
    global_crosswalk_specimen,
    by = "SpecimenID"
  )

march_iodine_ids <- march_iodine_with_momid %>%
  filter(!is.na(MomID)) %>%
  distinct(MomID) %>%
  pull(MomID)


march_diet_iodine_ids <- intersect(march_diet_ids, march_iodine_ids) #133



# 6. Childhood Outcome Sample Size -------------------------------------------


# 6.0 ID for Iodine / Urine -----------------------------------------------
arch_diet_biomarker_candidate_ids <- arch_diet_iodine_ids # 90

march_diet_biomarker_candidate_ids <- march_diet_iodine_ids # 133

## 6.1 NIH Toolbox --------------------------------------------------------


### 6.1.1 ARCH NIH Toolbox --------------------------------------------------


# ARCH NIH Toolbox
arch_ntb <- read_excel(
  "Z:/ECHO/CHARM/Data/ECHO 1/NIH TOOLBOX/ARCH NIHTB July 2023.xlsx",
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

# ---- 1. only use the part of global crosswalk 
# global_crosswalk_p 

# ---- 2. Prepare ARCH NIH Toolbox IDs 
arch_ntb_df <- tibble(PIN = arch_ntb_ids) %>%
  mutate(
    PIN = as.character(PIN),
    PIN = str_trim(PIN),
    # Identify ID type by last character
    id_type = if_else(str_ends(PIN, "0"), "mom", "child")
  )

# ---- 3. Split by ID type 

## 3a. Mom_ECHO_ID → MomID
arch_ntb_mom_from_mom <- arch_ntb_df %>%
  filter(id_type == "mom") %>%
  left_join(
    global_crosswalk_p,
    by = c("PIN" = "Mom_ECHO_ID")
  )

## 3b. Child_ECHO_ID → MomID
arch_ntb_mom_from_child <- arch_ntb_df %>%
  filter(id_type == "child") %>%
  left_join(
    global_crosswalk_p,
    by = c("PIN" = "Child_ECHO_ID")
  )

# ---- 4. Combine and extract unique MomID  
arch_ntb_with_momid <- bind_rows(
  arch_ntb_mom_from_mom,
  arch_ntb_mom_from_child
)

arch_ntb_mom_ids <- arch_ntb_with_momid %>%
  filter(!is.na(MomID)) %>%
  distinct(MomID) %>%
  pull(MomID)

# check the overlap
arch_diet_biomarker_ntb_ids <- intersect(
  arch_diet_biomarker_candidate_ids,
  arch_ntb_mom_ids
)

### 6.1.2 MARCH NIH Toolbox -------------------------------------------------


# MARCH NIH Toolbox
march_ntb <- read_excel(
  "Z:/ECHO/CHARM/Data/ECHO 1/NIH TOOLBOX/MARCH NIHTB July 2023 .xlsx",
  sheet = 2
)

# ECHO 2 NIH Toolbox
echo2_ntb <- read_excel("Z:/ECHO/CHARM/Data/ECHO 2/NIH Toolbox Report/NIH Toolbox report Dec2025.xlsx")

# ---- 1. only use the part of global crosswalk 
# global_crosswalk_p2 

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


length(intersect(march_ntb_mom_ids, march_diet_biomarker_candidate_ids)) # 33

# update ARCH
arch_echo2_ntb_mom_ids <- union(arch_ntb_mom_ids, echo2_ntb_child_ids )
# check the overlap
length(intersect(
  arch_diet_biomarker_candidate_ids,
  arch_echo2_ntb_mom_ids
)) #38


## 6.2 CBCL ----------------------------------------------------------------

# ARCH CBCL
# arch_cbcl <- read.csv(
#   "Z:/ECHO/CHARM/Data/ECHO 1/ARCH Enrollment Data/RAIND/ARCH RAIND data.csv"
# )

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

# 6.2.1 ARCH --------------------------------------------------------------

# CBCL PRE
arch_cbcl_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_cnh_cbcl_pre.csv"
)

# arch_cbcl_pre, n=44
get_overlap_n(
  df = arch_cbcl_pre,
  diet_biomarker_ids = arch_diet_biomarker_candidate_ids,
  crosswalk_df = global_crosswalk_p
)



### 6.2.2 MARCH -------------------------------------------------------------

march_cbcl_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_cbcl_pre.csv"
)

# march_cbcl_pre, n = 272
get_overlap_n(
  df = march_cbcl_pre,
  diet_biomarker_ids = march_diet_biomarker_candidate_ids,
  crosswalk_df = global_crosswalk_p
)


## 6.3 SRS2 -----------------------------------------------------------------

# ---- ARCH SRS-2 
arch_srs2_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_cnh_srs2_pre.csv"
)

arch_srs2_sch <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_cnh_srs2_sch.csv"
)


# ---- MARCH SRS-2 
march_srs2_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_srs2_pre.csv"
)

march_srs2_sch <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_srs2_sch.csv"
)



# ARCH
# arch_srs2_pre, n = 13
get_overlap_n(
  df = arch_srs2_pre,
  diet_biomarker_ids = arch_diet_biomarker_candidate_ids,
  crosswalk_df = global_crosswalk_p
)

# arch_srs2_sch ,n = 40
get_overlap_n(
  df = arch_srs2_sch,
  diet_biomarker_ids = arch_diet_biomarker_candidate_ids,
  crosswalk_df = global_crosswalk_p
)

# MARCH
# march_srs2_pre, n = 438
get_overlap_n(
  df = march_srs2_pre,
  diet_biomarker_ids = march_diet_biomarker_candidate_ids,
  crosswalk_df = global_crosswalk_p
)

# march_srs2_sch, n = 256 
get_overlap_n(
  df = march_srs2_sch,
  diet_biomarker_ids = march_diet_biomarker_candidate_ids,
  crosswalk_df = global_crosswalk_p
)


## 6.4 Ages and Stages -----------------------------------------------------



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

# march_asq_9_combined
get_overlap_n(
  df = march_asq_9_combined,
  diet_biomarker_ids = march_diet_biomarker_candidate_ids,
  crosswalk_df = global_crosswalk_p
)

# march_asq_36_n
get_overlap_n(
  df = march_asq_36,
  diet_biomarker_ids = march_diet_biomarker_candidate_ids,
  crosswalk_df = global_crosswalk_p
)
