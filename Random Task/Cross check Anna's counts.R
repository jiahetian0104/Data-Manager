library(tidyverse)




data <- read_csv("C:/Users/tianjiah/Desktop/2026-03-16T14_07_38.989Z_2025_3_17_study_export.csv")

table(data$event.2025_3_5_yr_cg_survey.completed)

class(data$event.2025_3_5_yr_cg_survey.completedDate)
data %>%
  filter(!is.na(event.2025_3_5_yr_cg_survey.completedDate)) %>%
  select(event.2025_3_5_yr_cg_survey.completedDate) %>%
  head(20)


# 1. count 1 --------------------------------------------------------------


count_result <- data %>%
  mutate(
    completedDate = mdy(event.2025_3_5_yr_cg_survey.completedDate)
  ) %>%
  filter(
    event.2025_3_5_yr_cg_survey.completed == TRUE,
    completedDate >= as.Date("2025-06-01"),
    completedDate <= as.Date("2025-08-31")
  ) %>%
  summarise(
    globalId_count = n_distinct(globalId)
  )

count_result


# 2. count 2 --------------------------------------------------------------


exclude_ids <- data %>%
  mutate(
    cg_completedDate = mdy(event.2025_3_5_yr_cg_survey.completedDate)
  ) %>%
  filter(
    event.2025_3_5_yr_cg_survey.completed == TRUE,
    cg_completedDate >= as.Date("2025-06-01"),
    cg_completedDate <= as.Date("2025-08-31")
  ) %>%
  distinct(globalId)

## condition 1
cg_jan_may_ids <- data %>%
  mutate(
    cg_completedDate = mdy(event.2025_3_5_yr_cg_survey.completedDate)
  ) %>%
  filter(
    event.2025_3_5_yr_cg_survey.completed == TRUE,
    cg_completedDate >= as.Date("2025-01-01"),
    cg_completedDate <= as.Date("2025-05-31")
  ) %>%
  distinct(globalId)

## condition 2
ipa_jun_aug_ids <- data %>%
  mutate(
    ipa_completedDate = mdy(event.2025_3_5_yr_ipa_completed.completedDate)
  ) %>%
  filter(
    event.2025_3_5_yr_ipa_completed.completed == TRUE,
    ipa_completedDate >= as.Date("2025-06-01"),
    ipa_completedDate <= as.Date("2025-08-31")
  ) %>%
  distinct(globalId)

# final ids
final_ids <- cg_jan_may_ids %>%
  inner_join(ipa_jun_aug_ids, by = "globalId") %>%
  anti_join(exclude_ids, by = "globalId")



# 3. count 3 --------------------------------------------------------------


