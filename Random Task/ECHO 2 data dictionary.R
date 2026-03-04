# ============================================================
# Compare two data dictionaries (current vs updated partial)
# - Identify items present in updated but missing in current
# - Identify items with same key but different attributes
# ============================================================

rm(list = ls())

library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(openxlsx)

# 1) Read data -------------------------------------------------------------
current_raw <- read_xlsx(file_current) %>% clean_names()
updated_raw <- read_xlsx(file_updated) %>% clean_names()

# Extract variable_name (no cleaning, no standardization)
current_vars <- current_raw %>%
  select(variable_name) %>%
  distinct()

updated_vars <- updated_raw %>%
  select(variable_name) %>%
  distinct()

# Variables in updated but NOT in current
missing_in_current <- updated_vars %>%
  anti_join(current_vars, by = "variable_name") %>%
  arrange(variable_name)

# Quick summary
cat("Updated unique variable_name:", nrow(updated_vars), "\n")
cat("Current unique variable_name:", nrow(current_vars), "\n")
cat("Missing in current:", nrow(missing_in_current), "\n")

# Save to Excel (optional)
out_file <- "C:/Users/tianjiah/Desktop/updated_vars_missing_in_current.xlsx"
wb <- createWorkbook()
addWorksheet(wb, "missing_in_current")
writeData(wb, "missing_in_current", missing_in_current)
saveWorkbook(wb, out_file, overwrite = TRUE)

cat("Saved:", out_file, "\n")