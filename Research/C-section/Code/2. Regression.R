# 1. Load libraries -------------------------------------------------------

library(tidyverse)
library(readxl)


# 2. Import processed data ------------------------------------------------

# Define processed data path
processed_data_path <- "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/Data-Manager/Research/C-section/Data/Processed/c_section_processed_data.xlsx"

# Read ARCH processed data
arch_data <- read_excel(
  path = processed_data_path,
  sheet = "ARCH"
)

# Read MARCH processed data
march_data <- read_excel(
  path = processed_data_path,
  sheet = "MARCH"
)


# 3. Initial data check ---------------------------------------------------

# Check dimensions
dim(arch_data)
dim(march_data)

# Check variable names
names(arch_data)
names(march_data)


# Harmonize urban/rural variable before combining
# arch_data <- arch_data %>%
#   mutate(
#     cohort = "ARCH",
#     urban_rural = UR10
#   )
# 
# march_data <- march_data %>%
#   mutate(
#     cohort = "MARCH",
#     urban_rural = UR20
#   )

# Make sure Vaginal is the reference outcome
arch_data_model <- arch_data %>%
  mutate(
    delivery_binary = factor(
      delivery_binary,
      levels = c("Vaginal", "C-section")
    ),
    UR10 = factor(UR10),
    birth_order_group = factor(
      birth_order_group,
      levels = c("First birth", "Second birth", "Third or later birth")
    )
  )

march_data_model <- march_data %>%
  mutate(
    delivery_binary = factor(
      delivery_binary,
      levels = c("Vaginal", "C-section")
    ),
    UR20 = factor(UR20),
    birth_order_group = factor(
      birth_order_group,
      levels = c("First birth", "Second birth", "Third or later birth")
    )
  )

# 4. MARCH descriptive statistics ----------------------------------------

library(gtsummary)
library(broom)

# Prepare MARCH analysis dataset
march_analysis_data <- march_data_model %>%
  select(
    delivery_binary,
    UR20,
    maternal_age,
    birth_order_group,
    gestational_age_weeks
  ) %>%
  filter(
    !is.na(delivery_binary)
  )

# Descriptive table for variables used in MARCH models
march_desc_table <- march_analysis_data %>%
  tbl_summary(
    by = delivery_binary,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ c(0, 1)
    ),
    missing = "ifany",
    label = list(
      UR20 ~ "Urban/Rural Area",
      maternal_age ~ "Maternal Age",
      birth_order_group ~ "Birth Order Group",
      gestational_age_weeks ~ "Gestational Age, weeks"
    )
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(
    label ~ "**Variable**",
    stat_0 ~ "**Overall**",
    stat_1 ~ "**Vaginal**",
    stat_2 ~ "**C-section**"
  ) %>%
  bold_labels()

march_desc_table

# 5. Logistic regression: C-section outcome -------------------------------

# Model 1: add gestational age --------------------------------------------

# ARCH logistic regression
arch_model_1 <- glm(
  delivery_binary ~ UR10 + maternal_age + birth_order_group,
  data = arch_data_model,
  family = binomial
)

# MARCH logistic regression
march_model_1 <- glm(
  delivery_binary ~ UR20 + maternal_age + birth_order_group,
  data = march_data_model,
  family = binomial
)

# View model summaries
summary(arch_model_1)
summary(march_model_1)

# Model 2: add gestational age --------------------------------------------

# ARCH logistic regression
arch_model_2 <- glm(
  delivery_binary ~ UR10 + maternal_age + birth_order_group + gestational_age_weeks,
  data = arch_data_model,
  family = binomial
)

# MARCH logistic regression
march_model_2 <- glm(
  delivery_binary ~ UR20 + maternal_age + birth_order_group + gestational_age_weeks,
  data = march_data_model,
  family = binomial
)

# View model summaries
summary(arch_model_2)
summary(march_model_2)


# Function to extract OR, 95% CI, p-value, and model N
extract_logistic_results <- function(model, model_name) {
  
  model_n <- nobs(model)
  
  tidy(
    model,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    mutate(
      model = model_name,
      n = model_n,
      OR = estimate,
      CI_95 = paste0(
        round(conf.low, 2),
        ", ",
        round(conf.high, 2)
      ),
      OR_95CI = paste0(
        round(OR, 2),
        " (",
        round(conf.low, 2),
        ", ",
        round(conf.high, 2),
        ")"
      ),
      p_value = case_when(
        p.value < 0.001 ~ "<0.001",
        TRUE ~ as.character(round(p.value, 3))
      )
    ) %>%
    select(
      model,
      n,
      term,
      OR,
      conf.low,
      conf.high,
      OR_95CI,
      p_value
    )
}

# Extract MARCH model results
march_model_1_results <- extract_logistic_results(
  model = march_model_1,
  model_name = "Model 1"
)

march_model_2_results <- extract_logistic_results(
  model = march_model_2,
  model_name = "Model 2"
)

# Combine model results
march_model_results <- bind_rows(
  march_model_1_results,
  march_model_2_results
)

march_model_results

march_model_results_clean <- march_model_results %>%
  filter(term != "(Intercept)")

march_model_results_clean

march_model_results_report <- march_model_results_clean %>%
  mutate(
    term = recode(
      term,
      "UR20U" = "Urban vs Rural",
      "maternal_age" = "Maternal age",
      "birth_order_groupSecond birth" = "Second birth vs First birth",
      "birth_order_groupThird or later birth" = "Third or later birth vs First birth",
      "gestational_age_weeks" = "Gestational age, weeks"
    )
  ) %>%
  select(
    Model = model,
    N = n,
    Variable = term,
    `OR (95% CI)` = OR_95CI,
    `p-value` = p_value
  )

march_model_results_report
