# The code is used to calculate sample size for Shipp Aim 1 
# And the code doesn't consider urine samples

library(readxl)
library(tidyverse)
library(janitor)


# 0. Crosswalk ------------------------------------------------------------

crosswalk_path <- "Z:/ECHO/CHARM/Data/Miscellaneous/Global Crosswalk/global_crosswalk.xlsx"
global_crosswalk <- read_excel(crosswalk_path, sheet = 1)

# SpecimenID to Child_ECHO_ID mapping
global_crosswalk_specimen <- global_crosswalk %>%
  mutate(
    SpecimenID = Specimen_ID %>%
      as.character() %>%
      str_trim() %>%
      str_pad(width = 4, side = "left", pad = "0")
  ) %>%
  select(SpecimenID, Child_ECHO_ID) %>%
  distinct()

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

# Count unique participants based on sampleid
march_dietdata %>%
  summarise(
    n_unique_child_echo_id = n_distinct(Child_ECHO_ID, na.rm = TRUE)
  ) # 1000

# # Identify missing Child_ECHO_ID
# march_missing_list <- march_dietdata %>%
#   mutate(
#     Child_ECHO_ID = str_trim(as.character(Child_ECHO_ID)),
#     SAMPLEID      = str_trim(as.character(SAMPLEID))
#   ) %>%
#   filter(Child_ECHO_ID == "NA") %>%
#   filter(!is.na(SAMPLEID), SAMPLEID != "") %>%
#   distinct(SAMPLEID) %>%
#   pull(SAMPLEID)
# 
# global_crosswalk_momid_in_missing_list <- global_crosswalk %>%
#   mutate(
#     MomID = str_trim(as.character(MomID))
#   ) %>%
#   filter(!is.na(MomID), MomID != "") %>%
#   filter(MomID %in% march_missing_list) %>%
#   select(MomID, Mom_ECHO_ID, ChildID, Child_ECHO_ID, pn1_survey_outcome, pn2_survey_outcome, pn3_survey_outcome) %>%
#   filter(!pn2_survey_outcome %in% c("ELIGIBLE NON-SAMPLE OUT OF STUDY AREA","ELIGIBLE NON-SAMPLE PREGNANCY LOSS", "ELIGIBLE REFUSED STUDY PARTICIPATION"))

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

# arch_missing_list <- arch_dietdata %>% filter(is.na(Child_ECHO_ID)) %>% select(SUBJECT_Id)
# write.csv(arch_missing_list, "Z:/ECHO/CHARM/Data/Miscellaneous/Global Crosswalk/ARCH_diet_missing_echo_id.csv", row.names = FALSE)

# 2. Thyroid --------------------------------------------------------------

arch_thyroid <- read.csv(
  "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Thyroid/ARCH_Thyroid_Function_Data.csv"
  )

arch_thyroid <- arch_thyroid %>%
  mutate(
    arch_id = as.character(arch_id),
    arch_id = str_trim(arch_id)
  ) %>%
  left_join(
    global_crosswalk_p1,
    by = c("arch_id" = "MomID")
  )

# MARCH Throid, column named "Tyr"
march_thyroid <- read_excel("Z:/ECHO/CHARM/Data/ECHO 1/MDHHS Data/2023 MDHHS Program Data/NBS/MARCH data_bloodspot.xlsx")

march_thyroid <- march_thyroid %>%
  mutate(
    march_id = as.character(march_id),
    march_id = str_trim(march_id)
  ) %>%
  left_join(
    global_crosswalk_p1,
    by = c("march_id" = "MomID", "child_id" = "ChildID")
  )

# 3. Iodine ---------------------------------------------------------------

# ARCH
arch_iodine <- read.csv(
  "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Iodine/ARCH_Iodine_ECHOUpload.csv"
)

arch_iodine <- arch_iodine %>%
  mutate(
    arch_id = as.character(arch_id),
    arch_id = str_trim(arch_id)
  ) %>%
  left_join(
    global_crosswalk_p1,
    by = c("arch_id" = "MomID")
  )

# MARCH
march_iodine <- read_excel(
  "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Iodine/12902_EC0376_EM20-011_IODINE_HHEAR/Iodine_ShippingManifest.xlsx"
)

# ---- 3. MARCH Iodine: unique IDs
march_iodine_parsed <- march_iodine %>%
  mutate(
    # Extract SpecimenID: characters after position 4
    SpecimenID = substr(`Original Sample ID`, 5, nchar(`Original Sample ID`)),
    
    # Standardize format
    SpecimenID = as.character(SpecimenID),
    SpecimenID = str_trim(SpecimenID)
  )

# get Child_ECHO_id
march_iodine_with_child_echo_id <- march_iodine_parsed %>%
  left_join(
    global_crosswalk_specimen,
    by = "SpecimenID"
  )


# 4. Urine Samples --------------------------------------------------------


# 4.1 MARCH ---------------------------------------------------------------


march_urine <- read.csv(
  "Z:/ECHO/CHARM/Data/Biospecimen/Databases/FreezerPro/Sent_09132024.csv"
) %>%
  # Keep urine only
  filter(Sample.Type == "Urine") %>%
  # Extract last 4 digits from Name as SpecimenID
  mutate(
    SpecimenID = str_extract(Name, "(\\d{4})$"),
    SpecimenID = str_trim(SpecimenID)
  ) 

# Create dataframe of unique urine SpecimenIDs
march_urine_specimen_df <- march_urine %>%
  filter(!is.na(SpecimenID)) %>%
  distinct(SpecimenID)


# Join ARCH urine specimens with Child_ECHO_ID from global crosswalk
march_urine_with_child_echo_id <- march_urine_specimen_df %>%
  left_join(
    global_crosswalk_specimen,
    by = "SpecimenID"
  )

# get distinct Child_ECHO_ID
march_urine_child_echo_ids <- march_urine_with_child_echo_id %>%
  filter(!is.na(Child_ECHO_ID)) %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

# 5. Aim 1 sample size check ----------------------------------------------


## 5.1 ARCH ----------------------------------------------------------------

# ---- 1. ARCH Diet: unique IDs 
# Count unique participants based on sampleid
arch_diet_ids <- arch_dietdata %>%
  mutate(
    Child_ECHO_ID = as.character(Child_ECHO_ID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

length(arch_diet_ids) # 371

# ---- 2. ARCH Thyroid: unique IDs 
arch_thyroid_ids <- arch_thyroid %>%
  mutate(
    Child_ECHO_ID = as.character(Child_ECHO_ID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  filter(Child_ECHO_ID !="NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

length(intersect(arch_diet_ids, arch_thyroid_ids)) # 0

# ---- 3. ARCH Iodine: unique IDs
arch_iodine_ids <- arch_iodine %>%
  mutate(
    Child_ECHO_ID = as.character(Child_ECHO_ID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

arch_diet_iodine_ids <- intersect(arch_diet_ids, arch_iodine_ids) # 90
length(arch_diet_iodine_ids)

# 5.2 MARCH ---------------------------------------------------------------

# ---- 1. MARCH Diet: unique IDs 
march_diet_ids <- march_dietdata %>%
  mutate(
    Child_ECHO_ID = as.character(Child_ECHO_ID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

# ---- 2. MARCH Thyroid: unique IDs 
march_thyroid_ids <- march_thyroid %>%
  mutate(
    Child_ECHO_ID = as.character(Child_ECHO_ID),
    Child_ECHO_ID = str_trim(Child_ECHO_ID)
  ) %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)

march_diet_thyroid_ids <- intersect(march_diet_ids, march_thyroid_ids) # 610
length(march_diet_thyroid_ids)

# ---- 3. MARCH Iodine: unique IDs

march_iodine_ids <- march_iodine_with_child_echo_id %>%
  filter(Child_ECHO_ID != "NA") %>%
  distinct(Child_ECHO_ID) %>%
  pull(Child_ECHO_ID)


march_diet_iodine_ids <- intersect(march_diet_ids, march_iodine_ids) # 136
length(march_diet_iodine_ids)

# ---- 4. MARCH Diet and Urine but without Idoine

# Step 1: ARCH moms with both Diet and Urine
march_diet_urine_ids <- intersect(march_diet_ids, march_urine_child_echo_ids)

# Step 2: From those, remove moms who already have Iodine
march_diet_urine_no_iodine_ids <- setdiff(march_diet_urine_ids, march_iodine_ids)

length(march_diet_urine_no_iodine_ids) # 598

# 6. Childhood Outcome Sample Size -------------------------------------------


## 6.0 ID for Iodine / Urine -----------------------------------------------
arch_diet_biomarker_candidate_ids <- arch_diet_iodine_ids # 86

march_diet_biomarker_candidate_ids <- union(march_diet_iodine_ids, march_diet_urine_no_iodine_ids) # 734

## 6.1 NIH Toolbox --------------------------------------------------------


### 6.1.1 ARCH NIH Toolbox --------------------------------------------------

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

# check the overlap
arch_diet_biomarker_ntb_ids <- intersect(
  arch_diet_biomarker_candidate_ids,
  arch_ntb_child_echo_ids
)

length(arch_diet_biomarker_ntb_ids) # 0

### 6.1.2 MARCH NIH Toolbox -------------------------------------------------


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


length(intersect(march_ntb_child_echo_ids, march_diet_biomarker_candidate_ids)) # 33

# update ARCH
arch_ntb_child_echo_ids_update <- union(arch_ntb_child_echo_ids, arch_ntb_child_echo_ids_from_ECHO2 )

# check the overlap
length(intersect(arch_diet_biomarker_candidate_ids, arch_ntb_child_echo_ids_update)) #38


## 6.2 CBCL ----------------------------------------------------------------

# ARCH CBCL
# arch_cbcl <- read.csv(
#   "Z:/ECHO/CHARM/Data/ECHO 1/ARCH Enrollment Data/RAIND/ARCH RAIND data.csv"
# )

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

# 6.2.1 ARCH --------------------------------------------------------------

# CBCL PRE
arch_cbcl_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_cnh_cbcl_pre.csv"
)

# arch_cbcl_pre, n=11
get_overlap_n(
  df = arch_cbcl_pre,
  diet_biomarker_ids = arch_diet_biomarker_candidate_ids
)



### 6.2.2 MARCH -------------------------------------------------------------

march_cbcl_pre <- read.csv(
  "Z:/ECHO/CHARM/Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_cnh_cbcl_pre.csv"
)

# march_cbcl_pre, n = 276
get_overlap_n(
  df = march_cbcl_pre,
  diet_biomarker_ids = march_diet_biomarker_candidate_ids
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
# arch_srs2_pre, n = 8
get_overlap_n(
  df = arch_srs2_pre,
  diet_biomarker_ids = arch_diet_biomarker_candidate_ids
)

# arch_srs2_sch ,n = 9
get_overlap_n(
  df = arch_srs2_sch,
  diet_biomarker_ids = arch_diet_biomarker_candidate_ids
)

# MARCH
# march_srs2_pre, n = 445
get_overlap_n(
  df = march_srs2_pre,
  diet_biomarker_ids = march_diet_biomarker_candidate_ids
)

# march_srs2_sch, n = 260
get_overlap_n(
  df = march_srs2_sch,
  diet_biomarker_ids = march_diet_biomarker_candidate_ids
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
  diet_biomarker_ids = march_diet_biomarker_candidate_ids
)

# march_asq_36_n
get_overlap_n(
  df = march_asq_36,
  diet_biomarker_ids = march_diet_biomarker_candidate_ids
)
