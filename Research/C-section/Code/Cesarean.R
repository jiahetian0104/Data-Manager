# 1. Load library ---------------------------------------------------------

library(tidyverse)

# 2. Import Data ----------------------------------------------------------


## 2.1 Set up file paths ---------------------------------------------------

# Define file paths
# Detect OS
if (Sys.info()["sysname"] == "Windows") {
  BASE_PATH <- "Z:/ECHO/CHARM"
} else {
  BASE_PATH <- "/Volumes/Groups/ECHO/CHARM"
}

arch_bc_path  <- file.path(BASE_PATH, "Data/ECHO 1/MDHHS Data/Birth_Certificate/ARCH_BC_Combined.csv")
march_bc_path <- file.path(BASE_PATH, "Data/ECHO 1/MDHHS Data/Birth_Certificate/MARCH_BC_FinalUpd.csv")
# Child medical records
cbmra_path <- file.path(BASE_PATH,'Data/ECHO 1/Medical Record/Final MRA MARCH/Child MRA/REDCap_ECHO1_ess_hhx_cbmra.csv')
# urban and rural info
march_ur_path <- file.path(BASE_PATH, 'Data/Code Derived/Geoinformation/Urban_Rural_Info.csv')
arch_ur_path <- file.path(BASE_PATH, 'Data/Website/ARCH Descriptive/UA_MANUAL.csv')
# weight gain
arch_weight_gain_path <- file.path(BASE_PATH,'Data/ECHO 1/RedCap/ARCH/20231202192404_41_ess_prg_tpwtgsr.csv')
march_weight_gain_path <- file.path(BASE_PATH,'Data/ECHO 1/RedCap/MARCH/20231202190220_42_ess_prg_tpwtgsr.csv')
# cross walk
cross_walk_path <- file.path(BASE_PATH, 'Data/Miscellaneous/Global Crosswalk/global_crosswalk.xlsx')

## 2.2 Read data files -----------------------------------------------------

# Read ARCH birth certificate data
arch_bc <- read_csv(arch_bc_path, show_col_types = FALSE)

# Read MARCH birth certificate data
march_bc <- read_csv(march_bc_path, show_col_types = FALSE)

# Child medical records
cbmra <- read_csv(cbmra_path, show_col_types = FALSE)

# urban info
march_ur <- read_csv(march_ur_path, show_col_types = FALSE)
arch_ur <- read_csv(arch_ur_path, show_col_types = FALSE)

# weight gain
arch_weight_gain <- read_csv(arch_weight_gain_path, show_col_types = FALSE)
march_weight_gain <- read_csv(march_weight_gain_path, show_col_types = FALSE)

# cross walk
cross_walk <- readxl::read_excel(cross_walk_path)

# 3. Data Cleaning --------------------------------------------------------

# Convert ID variables to character (important for merging later)
arch_bc <- arch_bc %>%
  mutate(arch_id = as.character(arch_id)) # mom ARCH ID

march_bc <- march_bc %>%
  mutate(sampleid = as.character(sampleid)) # child MARCH ID

## 3.1 Cesarean Recode -----------------------------------------------------

# Define recode mapping
route_levels <- c(
  "1" = "Vaginal/Spontaneous",
  "2" = "Vaginal/Forceps",
  "3" = "Vaginal/Vacuum",
  "4" = "Cesarean",
  "9" = "Unknown"
)

# Recode ARCH
arch_bc <- arch_bc %>%
  mutate(
    MD_FINAL_ROUTE = recode(as.character(MD_FINAL_ROUTE), !!!route_levels),
    MD_FINAL_ROUTE = factor(
      MD_FINAL_ROUTE,
      levels = route_levels
    )
  )

# Recode MARCH
march_bc <- march_bc %>%
  mutate(
    MD_FINAL_ROUTE = recode(as.character(MD_FINAL_ROUTE), !!!route_levels),
    MD_FINAL_ROUTE = factor(
      MD_FINAL_ROUTE,
      levels = route_levels
    )
  )



## 3.2 ARCH Urban/Rural ----------------------------------------------------

# UACE10 UR10, from birth certificate, grind is child-level, but we only have mom-level arch_id.
ur_arch <- arch_bc %>%
  select("arch_id", "MOMZIP", "RES_ZIPCODE", "UR10", "UACE10", "CENSUS")

# Manual updated UACE1o code for ARCH, from UA_MANUAL.csv, grind is mom-level, arch_id is the same as birth certificate.
MANUAL_UA_CODE <- arch_ur %>% 
  select("arch_id", "UACE10") 

ur_arch_clean <- ur_arch %>%
  mutate(
    UACE10 = as.character(UACE10)
  )

MANUAL_UA_CODE_clean <- MANUAL_UA_CODE %>%
  mutate(
    arch_id = as.character(arch_id),
    UACE10 = as.character(UACE10)
  )

# multiple birth
ur_arch_clean %>%
  count(arch_id) %>%
  filter(n > 1)

MANUAL_UA_CODE_clean %>%
  count(arch_id) %>%
  filter(n > 1)

# Check whether duplicated arch_id records have the same UACE10 in birth certificate data
ur_arch_uace_check <- ur_arch_clean %>%
  group_by(arch_id) %>%
  summarise(
    n_records = n(),
    n_unique_UACE10 = n_distinct(UACE10, na.rm = FALSE),
    UACE10_values = paste(unique(UACE10), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(n_records > 1) %>%
  arrange(desc(n_unique_UACE10), arch_id) 
# The results show that for duplicated arch_id records, they have the same UACE10 value, 
# so we can keep one record for each arch_id when merging with manual updated UACE10 code.

# Check whether duplicated arch_id records have the same UACE10 in manual UA code data
manual_uace_check <- MANUAL_UA_CODE_clean %>%
  group_by(arch_id) %>%
  summarise(
    n_records = n(),
    n_unique_UACE10 = n_distinct(UACE10, na.rm = FALSE),
    UACE10_values = paste(unique(UACE10), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(n_records > 1) %>%
  arrange(desc(n_unique_UACE10), arch_id)

# The results show that for duplicated arch_id records, they have the same UACE10 value, 
# so we can keep one record for each arch_id when merging with manual updated UACE10 code.

ur_arch_clean_dedup <- ur_arch_clean %>%
  group_by(arch_id) %>%
  summarise(
    # Keep non-missing UACE10 first; if all are missing, keep NA
    UACE10 = if_else(
      any(!is.na(UACE10)),
      first(na.omit(UACE10)),
      NA_character_
    ),
    
    # Keep non-missing UR10 first; if all are missing, keep NA
    UR10 = if_else(
      any(!is.na(UR10)),
      first(na.omit(UR10)),
      NA_character_
    ),
    
    .groups = "drop"
  )

MANUAL_UA_CODE_clean_dedup <- MANUAL_UA_CODE_clean %>%
  group_by(arch_id) %>%
  summarise(
    # Keep non-missing UACE10 first; if all are missing, keep NA
    UACE10 = if_else(
      any(!is.na(UACE10)),
      first(na.omit(UACE10)),
      NA_character_
    ),
    .groups = "drop"
  )


#------merge
UACE <- ur_arch_clean_dedup %>%
  left_join(
    MANUAL_UA_CODE_clean_dedup,
    by = "arch_id",
    suffix = c("", "_manual")
  ) %>%
  mutate(
    # Use manual UACE10 first, then birth certificate UACE10
    UACE10 = coalesce(UACE10_manual, UACE10),
    
    # If UACE10 is R or missing, classify UR10 as R; otherwise classify as U
    UR10 = if_else(
      UACE10 == "R" | is.na(UACE10),
      "R",
      "U"
    )
  ) %>%
  select(-UACE10_manual)


table(UACE$UR10, useNA = "ifany")



## 3.3 Maternal age -----------------------------------------------------------

march_bc <- march_bc %>%
  mutate(
    child_dob = make_date(BXYEAR, BXMONTH, BXDAY),
    mom_dob   = make_date(MOMBXYR, MOMBXMO, MOMBXDAY),
    maternal_age = time_length(interval(mom_dob, child_dob), "years")
  )

arch_bc <- arch_bc %>%
  mutate(
    child_dob = make_date(BXYEAR, BXMONTH, BXDAY),
    mom_dob   = make_date(MOMBXYR, MOMBXMO, MOMBXDAY),
    maternal_age = time_length(interval(mom_dob, child_dob), "years")
  ) %>%
  filter(
    maternal_age >= 18
  ) # there is a mom who is 15 years old

# clean BMI
march_bc <- march_bc %>%
  mutate(
    BMI = na_if(BMI, 999)
  )

arch_bc <- arch_bc %>%
  mutate(
    BMI = na_if(BMI, 999)
  )


## 3.4 Singleton -----------------------------------------------------------


march_bc <- march_bc %>%
  
  # 1 Create Singleton variable
  mutate(
    Singleton = case_when(
      PLURALITY == 1 ~ 1,
      PLURALITY > 1  ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%

  # 2 Create delivery binary
  mutate(
    delivery_binary = case_when(
      MD_FINAL_ROUTE == "Cesarean" ~ "C-section",
      MD_FINAL_ROUTE %in% c(
        "Vaginal/Spontaneous",
        "Vaginal/Forceps",
        "Vaginal/Vacuum"
      ) ~ "Vaginal",
      TRUE ~ NA_character_
    )
  )

arch_bc <- arch_bc %>%
  
  # 1 Create Singleton variable
  mutate(
    Singleton = case_when(
      PLURALITY == 1 ~ 1,
      PLURALITY > 1  ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%

  # 2 Create delivery binary
  mutate(
    delivery_binary = case_when(
      MD_FINAL_ROUTE == "Cesarean" ~ "C-section",
      MD_FINAL_ROUTE %in% c(
        "Vaginal/Spontaneous",
        "Vaginal/Forceps",
        "Vaginal/Vacuum"
      ) ~ "Vaginal",
      TRUE ~ NA_character_
    )
  )


## 3.5 Check prior live birth ----------------------------------------------


add_birth_order_vars <- function(df) {
  
  df %>%
    mutate(
      # Identify whether the mother had any prior live birth
      # LASTLVBXYR == 0 means no prior live birth
      has_prior_live_birth = case_when(
        is.na(LASTLVBXYR) ~ NA,
        LASTLVBXYR == 0 ~ FALSE,
        LASTLVBXYR > 0 ~ TRUE
      ),
      
      # Count how many previous children variables are non-missing
      n_previous_children_vars_nonmissing = rowSums(
        !is.na(across(c(NOWLIVING, NOWDEAD, BORNDEAD)))
      ),
      
      # Calculate total number of previous children
      previous_children_total = if_else(
        n_previous_children_vars_nonmissing == 0,
        NA_real_,
        rowSums(across(c(NOWLIVING, NOWDEAD, BORNDEAD)), na.rm = TRUE)
      ),
      
      # Identify whether the mother had any previous children
      had_previous_children = case_when(
        is.na(previous_children_total) ~ NA,
        previous_children_total > 0 ~ TRUE,
        previous_children_total == 0 ~ FALSE
      ),
      
      # Classify birth order based on previous children count
      birth_order_group = case_when(
        is.na(previous_children_total) ~ NA_character_,
        previous_children_total == 0 ~ "First birth",
        previous_children_total == 1 ~ "Second birth",
        previous_children_total >= 2 ~ "Third or later birth"
      )
    )
}


arch_bc <- add_birth_order_vars(arch_bc)

march_bc <- add_birth_order_vars(march_bc)

 
# 3.6 weight gain info ----------------------------------------------------
clean_weight_gain <- function(df, cross_walk) {
  
  cross_walk_mom <- cross_walk %>%
    transmute(
      MomID = as.character(MomID),
      Mom_ECHO_ID = as.character(Mom_ECHO_ID)
    ) %>%
    filter(!is.na(Mom_ECHO_ID), Mom_ECHO_ID != "") %>%
    distinct(Mom_ECHO_ID, .keep_all = TRUE)
  
  df %>%
    mutate(
      # Convert ID to character for joining
      participantid = as.character(participantid),
      
      # Convert variables to numeric
      tpwtgsr_1 = as.numeric(tpwtgsr_1),
      tpwtgsr_3 = as.numeric(tpwtgsr_3),
      tpwtgsr_3a_lb = as.numeric(tpwtgsr_3a_lb),
      
      # Treat refused / don't know as missing
      tpwtgsr_3 = if_else(
        tpwtgsr_3 %in% c(-7, -8),
        NA_real_,
        tpwtgsr_3
      ),
      
      # Create signed pregnancy weight change in pounds
      pregnancy_weight_change_lb = case_when(
        tpwtgsr_3 == 1 ~ tpwtgsr_3a_lb,
        tpwtgsr_3 == 2 ~ -tpwtgsr_3a_lb,
        is.na(tpwtgsr_3) ~ NA_real_,
        TRUE ~ NA_real_
      )
    ) %>%
    rename(
      mom_echo_id = participantid,
      gestational_age_weeks = tpwtgsr_1
    ) %>%
    left_join(
      cross_walk_mom,
      by = c("mom_echo_id" = "Mom_ECHO_ID")
    ) %>%
    select(
      MomID,
      mom_echo_id,
      gestational_age_weeks,
      pregnancy_weight_change_lb,
      tpwtgsr_3,
      tpwtgsr_3a_lb
    )
}

arch_weight_gain_clean <- clean_weight_gain(arch_weight_gain, cross_walk)

march_weight_gain_clean <- clean_weight_gain(march_weight_gain, cross_walk)




# 3.7 Merge dataset ---------------------------------------

arch_bc <- arch_bc %>%
  left_join(
    UACE %>% select(arch_id, UR10),
    by = "arch_id"
  ) %>%
  left_join(
    arch_weight_gain_clean,
    by = c("arch_id" = "MomID")
  )

march_bc <- march_bc %>%
  left_join(
    march_ur %>% select(march_id, child_id, UR20),
    by = c(
      "sampleid" = "child_id",
      "march_id" = "march_id"
    )
  ) %>%
  left_join(
    march_weight_gain_clean,
    by = c("march_id" = "MomID")
  )


# 4. Save dataset ---------------------------------------------------------

arch_data <- arch_bc %>%
  select(
    arch_id,
    MD_FINAL_ROUTE,
    maternal_age,
    BMI,
    Singleton,
    delivery_binary,
    has_prior_live_birth,
    had_previous_children,
    birth_order_group,
    UR10
  )


# Visualization -----------------------------------------------------------

# bar chart

# ARCH plot
ggplot(arch_bc, aes(x = MD_FINAL_ROUTE)) +
  geom_bar(aes(fill = MD_FINAL_ROUTE)) +
  labs(
    title = "ARCH: Delivery Route Distribution",
    x = "Delivery Route",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# MARCH plot
ggplot(march_bc, aes(x = MD_FINAL_ROUTE)) +
  geom_bar(aes(fill = MD_FINAL_ROUTE)) +
  labs(
    title = "MARCH: Delivery Route Distribution",
    x = "Delivery Route",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# percentage

# ARCH percentage
arch_plot <- arch_bc %>%
  count(MD_FINAL_ROUTE) %>%
  mutate(prop = n / sum(n))

ggplot(arch_plot, aes(x = MD_FINAL_ROUTE, y = prop)) +
  geom_col(aes(fill = MD_FINAL_ROUTE)) +
  labs(
    title = "ARCH: Delivery Route Distribution (%)",
    x = "Delivery Route",
    y = "Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# MARCH percentage
march_plot <- march_bc %>%
  count(MD_FINAL_ROUTE) %>%
  mutate(prop = n / sum(n))

ggplot(march_plot, aes(x = MD_FINAL_ROUTE, y = prop)) +
  geom_col(aes(fill = MD_FINAL_ROUTE)) +
  labs(
    title = "MARCH: Delivery Route Distribution (%)",
    x = "Delivery Route",
    y = "Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# comparison

bc_all <- bind_rows(
  arch_bc %>% select(MD_FINAL_ROUTE) %>% mutate(cohort = "ARCH"),
  march_bc %>% select(MD_FINAL_ROUTE) %>%mutate(cohort = "MARCH")
)

bc_plot <- bc_all %>%
  count(cohort, MD_FINAL_ROUTE) %>%
  group_by(cohort) %>%
  mutate(prop = n / sum(n))

ggplot(bc_plot, aes(x = MD_FINAL_ROUTE, y = prop, fill = cohort)) +
  geom_col(position = "dodge") +
  labs(
    title = "Delivery Route Distribution by Cohort",
    x = "Delivery Route",
    y = "Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

library(plotly)

p <- ggplot(bc_plot, aes(x = MD_FINAL_ROUTE, y = prop, fill = cohort)) +
  geom_col(position = "dodge") +
  labs(
    title = "Delivery Route Distribution by Cohort",
    x = "Delivery Route",
    y = "Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplotly(p)






table(march_bc_clean$Singleton, useNA = "ifany")
summary(march_bc_clean$maternal_age)
table(march_bc_clean$delivery_binary, useNA = "ifany")

march_bc_clean %>%
  count(delivery_binary, Singleton) %>%
  group_by(Singleton) %>%
  mutate(prop = n / sum(n))


# 5. Regression -----------------------------------------------------------

march_analysis <- march_bc_clean %>%
  left_join(
    ur_march %>% select(march_id, child_id, UR20),
    by = c(
      "sampleid" = "child_id",
      "march_id" = "march_id"
    )
  )



march_analysis <- march_analysis %>%
  mutate(
    c_section = if_else(delivery_binary == "C-section", 1, 0)
  )

# march_analysis <- march_analysis %>%
#   mutate(
#     urban = case_when(
#       UR20 == "Urban" ~ 1,
#       UR20 == "Rural" ~ 0,
#       TRUE ~ NA_real_
#     )
#   )


model_march <- glm(
  c_section ~ maternal_age + BMI + Singleton,
  data = march_analysis,
  family = binomial()
)

# library(broom)

tidy(model_march, exponentiate = TRUE, conf.int = TRUE)

summary(march_analysis[, c("c_section", "maternal_age", "BMI", "Singleton")])
nrow(march_analysis)
march_analysis_complete <- march_analysis %>%
  drop_na(c_section, maternal_age, BMI, Singleton)

