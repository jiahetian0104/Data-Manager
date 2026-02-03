library(readxl)
library(tidyverse)

global_crosswalk <- read_excel(
  path = "Z:/ECHO/CHARM/Data/Miscellaneous/Global Crosswalk/global_crosswalk.xlsx",
  sheet = 2
)

# include twins
dup_in_crosswalk <- global_crosswalk %>%
  count(Specimen_ID) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  filter(!is.na(Specimen_ID))

# exclude twins
dup_in_crosswalk <- global_crosswalk %>%
  count(Specimen_ID) %>%
  filter(n > 2) %>%
  arrange(desc(n)) %>%
  filter(!is.na(Specimen_ID))

# check duplicate MomID + ChildID pairs
dup_mom_child <- global_crosswalk %>%
  filter(!is.na(MomID), !is.na(ChildID)) %>%   # remove missing IDs
  count(MomID, ChildID) %>%
  filter(n > 1) %>%
  arrange(desc(n))
