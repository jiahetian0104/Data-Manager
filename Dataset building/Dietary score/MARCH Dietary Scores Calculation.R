# The code is to generate Harmonized Score for Dietary Screener Questionnaire.


# 1. Load Library ---------------------------------------------------------

library(tidyverse)

library(readxl)

library(dbx)
library(DT)
library(openxlsx)
library(eeptools)

library(httr2)
library(readr)
library(openxlsx)



# 2. Import Data ----------------------------------------------------------

CROSSWALK <- read.csv("Z:/ECHO/CHARM/Data/ECHO 1/SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_1/Data_Delivery/Crosswalk/key_prenatal.csv")
# Registration Information
REG <- read.csv("Z:/ECHO/CHARM/Data/ECHO 1/Participant Registration/ParticipantRegistration_Export.csv")

REG <- REG %>% 
  select(SAMPLEID = CohortParticipantId, # Rename
         CohortParticipantId,DateOfBirth_Month, DateOfBirth_Day, DateOfBirth_Year) %>%
  mutate(BX_Day_Mom = make_date(DateOfBirth_Year, DateOfBirth_Month, DateOfBirth_Day)) #Birthday for Mom


PN1 <- read.csv("Z:/ECHO/CHARM/Data/ECHO 1/SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_1/Data_Delivery/Sample/PRENATAL_1_Sample_mixedformats.csv")

PN1 <- PN1 %>%
  select(DATSTAT_PARTICIPANTID, CONSENT_DATE) %>%
  left_join(CROSSWALK, by = "DATSTAT_PARTICIPANTID") %>%
  left_join(REG, by = "SAMPLEID") %>%
  select(SAMPLEID, CONSENT_DATE, BX_Day_Mom) %>%
  filter(!is.na(BX_Day_Mom))


# Survey Answer
dv <- read.csv("Z:/ECHO/CHARM/Data/Resources/Variable Scoring/Prenatal 1 Dietary Variables/PN1_DietaryVars.csv")


# Scoring Sources

# ## ECHO
# https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/
#   
# ## PhenX
# Fiber: https://www.phenxtoolkit.org/protocols/view/50601?origin=search
# Dairy: https://www.phenxtoolkit.org/protocols/view/50401
# Added Sugar: https://www.phenxtoolkit.org/protocols/view/51001?origin=search
# Fruits and Veg: https://www.phenxtoolkit.org/protocols/view/50701
# Calcium: https://www.phenxtoolkit.org/protocols/view/50202?origin=search


# 3. Age Calculation ------------------------------------------------------

PN1 <- PN1 %>%
  mutate(
    consent_age = round(
      as.numeric(as.Date(CONSENT_DATE) - as.Date(BX_Day_Mom)) / 365.25, 2
    )
  ) %>%
  select(march_id = SAMPLEID, consent_age)

# ---- PhenX & ECHO Age Group
PN1 <- PN1 %>%
  mutate(
    PhenX_group = cut(
      consent_age,
      breaks = c(17, 28, 38, 48, 57),
      labels = c("18-27", "28-37", "38-47", "48-57"),
      right = FALSE
    ),
    ECHO_group = cut(
      consent_age,
      breaks = c(17, 26, 36, 46, 60),
      labels = c("18-25", "26-35", "36-45", "46-60"),
      right = FALSE
    )
  )

# check age group frequency
table(PN1$PhenX_group)
table(PN1$ECHO_group)

# add age group to survey data
dv <- dv %>%
  left_join(PN1, by = c("SAMPLEID" = "march_id")) %>%
  filter(!is.na(consent_age)) # only keep data with ages


# 4. Spit Dataset for PhenX and Echo --------------------------------------

# define variables in need

phenx_vars <- c("SAMPLEID", "CEREAL", "FRUIT", "GREENSALAD", "FRENCHFRIES", 
                "WHITEPOTATO", "BEAN", "OTHERVEGETABLES", "TOMATOSAUCE", "SALSA", 
                "WHOLEGRAIN", "DOUGHNUT", "COOKIES", "CHEESE", "MILK", "SUGARSODA", 
                "FRUITJUICE", "SUGARFRUIT_DRINK", "CEREAL__COOKED", "CEREAL__ALLBRAN", 
                "CEREAL__SOMEBRAN", "CEREAL__LITTLEBRAN")

echo_vars <- c("SAMPLEID", "CEREAL_ECHO", "FRUIT_ECHO", "GREENSALAD_ECHO", "FRENCHFRIES_ECHO", "CHOCOLATE_ECHO",
               "WHITEPOTATO_ECHO", "BEAN_ECHO", "OTHERVEGETABLES_ECHO", "TOMATOSAUCE_ECHO", "PIZZA_ECHO", "ICECREAM_ECHO", "RICE_ECHO", "POPCORN_ECHO",
               "SALSA_ECHO", "WHOLEGRAIN_ECHO", "DOUGHNUT_ECHO", "COOKIES_ECHO", "ICECREAM_ECHO", "CHEESE_ECHO", 
               "MILK_ECHO", "SUGARSODA_ECHO", "FRUITJUICE_ECHO", "SWEETDRINK_ECHO", "SUGAR_HONEY_ECHO",
               "CEREAL__COOKED", "CEREAL__ALLBRAN", "CEREAL__SOMEBRAN", "CEREAL__LITTLEBRAN")

DV_PhenX <- dv %>%
  select(all_of(phenx_vars), PhenX_group) %>%
  filter(!is.na(CEREAL))

DV_ECHO <- dv %>%
  select(all_of(echo_vars), ECHO_group) %>%
  filter(!is.na(CEREAL_ECHO))


# 5. Recode Daily Frequency -----------------------------------------------


## 5.1 PhenX ---------------------------------------------------------------

map_PhenX <- c(
  `0` = 0,
  `1` = 0.067,
  `2` = 0.214,
  `3` = 0.5,
  `4` = 0.786,
  `5` = 1,
  `6` = 2,
  `7` = 3,
  `8` = 4,
  `9` = 5,
  `98` = NA_real_ # Don't know
)


DV_PhenX <- DV_PhenX %>%
  mutate(across(
    which(names(DV_PhenX) == "CEREAL") :
    which(names(DV_PhenX) == "SUGARFRUIT_DRINK"), 
    ~ recode(., !!!map_PhenX))
    )

## 5.2 ECHO ----------------------------------------------------------------

# for lower frequency
map_ECHO_2_16 <- c(
  `1` = 0,
  `2` = 0.033,
  `3` = 0.083,
  `4` = 0.143,
  `5` = 0.286,
  `6` = 0.5,
  `7` = 0.786,
  `8` = 1,
  `9` = 2,
  `98` = NA_real_, # Don't know
  `99` = NA_real_ # Refuse
)

# for higher frequency 
map_ECHO_17_20 <- c(
  `1`  = 0,
  `2`  = 0.033,
  `3`  = 0.083,
  `4`  = 0.143,
  `5`  = 0.286,
  `6`  = 0.5,
  `7`  = 0.786,
  `8`  = 1,
  `9`  = 2.5,
  `10` = 4.5,
  `11` = 6,
  `98` = NA_real_,# Don't know
  `99` = NA_real_ # Refuse
)

DV_ECHO <- DV_ECHO %>%
  mutate(
    across(
      which(names(DV_ECHO) == "CEREAL_ECHO") :
        which(names(DV_ECHO) == "CHEESE_ECHO"),
      ~ recode(., !!!map_ECHO_2_16)
    ),
    across(
      which(names(DV_ECHO) == "MILK_ECHO") :
        which(names(DV_ECHO) == "SUGAR_HONEY_ECHO"),
      ~ recode(., !!!map_ECHO_17_20)
    )
  )


# 6. Cereal Count ---------------------------------------------------------


## 6.1 PhenX ---------------------------------------------------------------

# These variables are about Yes/No
cereal_vars <- c("CEREAL__COOKED", "CEREAL__ALLBRAN", 
                 "CEREAL__SOMEBRAN", "CEREAL__LITTLEBRAN")

DV_PhenX <- DV_PhenX %>%
  # 1) replace NA -> 0 only for cereal flag columns 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  # 2) compute number of cereal types marked as 1
  mutate(Cereal_Count = rowSums(across(all_of(cereal_vars)))) %>%
  # 3) distribute CEREAL by flags; guard against zero denominator
  mutate(across(all_of(cereal_vars),
                ~ if_else(. == 1 & Cereal_Count > 0, CEREAL / Cereal_Count, 0)))


## 6.2 ECHO ----------------------------------------------------------------

DV_ECHO <- DV_ECHO %>%
  # 1) replace NA -> 0 only for cereal flag columns 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  # 2) compute number of cereal types marked as 1
  mutate(Cereal_Count = rowSums(across(all_of(cereal_vars)))) %>%
  # 3) distribute CEREAL_ECHO by flags; guard against zero denominator
  mutate(across(all_of(cereal_vars),
                ~ if_else(. == 1 & Cereal_Count > 0, CEREAL_ECHO / Cereal_Count, 0)))



# 7. Fiber ----------------------------------------------------------------


## 7.1 PhenX ---------------------------------------------------------------


# 1) Build coefficient lookup table (add *_coef suffix to avoid name clashes)
coef_tbl <- tribble(
  ~PhenX_group, ~COOKED_coef, ~ALLBRAN_coef, ~SOMEBRAN_coef, ~LITTLEBRAN_coef, ~MILK_coef,
  ~SUGARFRUIT_DRINK_coef, ~FRUITJUICE_coef, ~FRUIT_coef, ~FRENCHFRIES_coef, ~WHITEPOTATO_coef,
  ~BEAN_coef, ~OTHERVEGETABLES_coef, ~TOMATOSAUCE_coef, ~SALSA_coef, ~WHOLEGRAIN_coef,
  ~DOUGHNUT_coef, ~COOKIES_coef, ~CHEESE_coef,
  "18-27", 234, 42.75, 60, 46.5, 244, 360, 280.125, 118, 79.5, 122, 132.75, 56, 125, 32, 50, 67.333333, 56.7, 28.25,
  "28-37", 234, 42.75, 57, 37.5, 244, 341, 249, 118, 70, 127, 126.5, 62.04, 113.4, 31.13, 48, 58, 50, 24,
  "38-47", 234, 42.75, 53, 36.25, 244, 250, 248.8, 118, 70, 119, 126.5, 64.415, 62.5, 36.565, 47.5, 57, 48.8, 24,
  "48-57", 234, 42.75, 49.5, 33, 214.25, 250, 233.25, 118, 70, 113, 141.75, 64.92, 125, 27.8475, 45, 58.5, 55.2, 26.25
)

# calculate Fiber_Score
DV_PhenX <- DV_PhenX %>%
  left_join(coef_tbl, by = "PhenX_group") %>%
  mutate(
    Fiber_Score = (
      1.838259 +
        (0.000671 * COOKED_coef * CEREAL__COOKED) +
        (0.019873 * ALLBRAN_coef * CEREAL__ALLBRAN) +
        (0.004688 * SOMEBRAN_coef * CEREAL__SOMEBRAN) +
        (0.001493 * LITTLEBRAN_coef * CEREAL__LITTLEBRAN) +
        (0.000169 * MILK_coef * MILK) +
        (0.000115 * SUGARFRUIT_DRINK_coef * SUGARFRUIT_DRINK) +
        (0.000229 * FRUITJUICE_coef * FRUITJUICE) +
        (0.001009 * FRUIT_coef * FRUIT) +
        (0.001381 * FRENCHFRIES_coef * FRENCHFRIES) +
        (0.000693 * WHITEPOTATO_coef * WHITEPOTATO) +
        (0.003217 * BEAN_coef * BEAN) +
        (0.000925 * OTHERVEGETABLES_coef * OTHERVEGETABLES) +
        (0.001204 * TOMATOSAUCE_coef * TOMATOSAUCE) +
        (0.003239 * SALSA_coef * SALSA) +
        (0.003401 * WHOLEGRAIN_coef * WHOLEGRAIN) +
        (0.001683 * DOUGHNUT_coef * DOUGHNUT) +
        (0.001377 * COOKIES_coef * COOKIES) +
        (0.000513 * CHEESE_coef * CHEESE)
    )^3
  )

# final Fiber_Score
Fiber_PhenX <- DV_PhenX %>%
  select(SAMPLEID, Fiber_Score)


## 7.2 ECHO ----------------------------------------------------------------

# 1) Build coefficient lookup table (add *_coef suffix to avoid name clashes)
coef_echo <- tribble(
  ~ECHO_group, 
  ~FRUIT_coef, ~FRUITJUICE_coef, ~GREENSALAD_coef, ~FRENCHFRIES_coef, ~WHITEPOTATO_coef,
  ~BEAN_coef, ~OTHERVEGETABLES_coef, ~PIZZA_coef, ~SALSA_coef, ~TOMATOSAUCE_coef, 
  ~CHEESE_coef, ~MILK_coef, ~FROZENDESSERT_coef, ~SUGARSODA_coef, ~SUGARTEA_coef, 
  ~SWEETDRINK_coef, ~DOUGHNUT_coef, ~COOKIES_coef, ~CANDY_coef, ~WHOLEGRAIN_coef, 
  ~BROWNRICE_coef, ~POPCORN_coef, 
  ~CEREAL_FIBER1_coef, ~CEREAL_FIBER2_coef, ~CEREAL_FIBER3_coef,
  
  # 18–25
  "18-25", 
  128.065, 249, 30, 84, 105,
  94.88, 68.75, 171.255, 26.7, 248.4,
  25.5, 244, 103.1875, 368, 183.05,
  337.126667, 76.5, 41, 21.25, 52,
  147, 29.315,
  99.056667, 41.63, 45.56,
  
  # 26–35
  "26-35", 
  116.7333, 248, 30, 82.5, 122,
  89, 73.7525, 157.77, 30, 255.75,
  26.0125, 244, 108.56, 368, 208.6,
  350.75, 69.5, 40.4, 18.00, 52,
  142.628333, 28.175,
  177, 39.38, 45.75,
  
  # 36–45
  "36-45", 
  120.22125, 219.5, 41.25, 69.97, 114,
  86.24, 67.366667, 165.9, 32, 248,
  24.80625, 244, 111, 368, 231.375,
  325.5, 70, 39.696667, 20.535, 50,
  136.4575, 26,
  165, 43, 36.875,
  
  # 46–60
  "46-60", 
  116.625, 201.905, 45.928333, 65.833333, 113.5,
  94.88, 70, 145.35, 17.8, 217,
  24.15, 214.375, 111, 357.6, 236.8,
  323.345, 70, 40.1, 20.00, 48,
  133.886667, 39.67,
  179.25, 37.63, 40.595
)



# 2) Compute Fiber_Score vectorized by ECHO_group
DV_ECHO <- DV_ECHO %>%
  left_join(coef_echo, by = "ECHO_group") %>%
  mutate(
    Fiber_Score =
      11.322163 +
      0.015617 * FRUIT_coef         * FRUIT_ECHO +
      (-0.001998) * FRUITJUICE_coef  * FRUITJUICE_ECHO +
      0.022833 * GREENSALAD_coef    * GREENSALAD_ECHO +
      (-0.007540) * FRENCHFRIES_coef * FRENCHFRIES_ECHO +
      0.000982 * WHITEPOTATO_coef   * WHITEPOTATO_ECHO +
      0.023766 * BEAN_coef          * BEAN_ECHO +
      0.012751 * OTHERVEGETABLES_coef * OTHERVEGETABLES_ECHO +
      0.00335 * PIZZA_coef * PIZZA_ECHO +
      (-0.002519) * SALSA_coef       * SALSA_ECHO +
      0.003532 * TOMATOSAUCE_coef   * TOMATOSAUCE_ECHO +
      (-0.007777) * CHEESE_coef      * CHEESE_ECHO +
      (-0.000890) * MILK_coef        * MILK_ECHO +
      0.004637 * FROZENDESSERT_coef * ICECREAM_ECHO + 
      (-0.002178) * SUGARSODA_coef   * SUGARSODA_ECHO +
      (-0.001502) * SUGARTEA_coef * SUGAR_HONEY_ECHO +
      (-0.001625) * SWEETDRINK_coef  * SWEETDRINK_ECHO +
      0.035073 * CANDY_coef * CHOCOLATE_ECHO +
      (-0.002263) * DOUGHNUT_coef    * DOUGHNUT_ECHO +
      0.022178 * COOKIES_coef       * COOKIES_ECHO +
      0.018428 * BROWNRICE_coef * RICE_ECHO +
      0.015324 * WHOLEGRAIN_coef    * WHOLEGRAIN_ECHO +
      0.077278 * POPCORN_coef * POPCORN_ECHO +
      0.017465 * CEREAL_FIBER1_coef * CEREAL__ALLBRAN +
      0.017612 * CEREAL_FIBER2_coef * CEREAL__SOMEBRAN +
      0.067026 * CEREAL_FIBER3_coef * CEREAL__LITTLEBRAN
  )

# 3) Final output table (no need to merge four subtables)
Fiber_ECHO <- DV_ECHO %>%
  select(SAMPLEID, Fiber_Score)

# NA means there is no age data for participants, not everyone provide birthday date

# 8. Dairy ----------------------------------------------------------------

## 8.1 PhenX ---------------------------------------------------------------

# 1) Build age-specific weights for Dairy score by PhenX_group
coef_dairy <- tribble(
  ~PhenX_group, ~MILK_coef_d, ~CHEESE_coef_d,
  "18-27",      1.000,      0.517,
  "28-37",      1.000,      0.470,
  "38-47",      0.999,      0.494,
  "48-57",      0.874,      0.494
)

# 2) Vectorized Dairy_Score
DV_PhenX <- DV_PhenX %>%
  # join group-specific coefficients
  left_join(coef_dairy, by = "PhenX_group") %>%
  # vectorized Dairy_Score computation
  mutate(
    # 0.385301 + 0.782852 * sqrt( MILK_coef*MILK + CHEESE_coef*CHEESE )  all squared
    Dairy_Score = (0.385301 + 0.782852 * ((MILK_coef_d * MILK + CHEESE_coef_d * CHEESE) ^ 0.5)) ^ 2
  )

# Final output (no need to bind/merge four subtables)
Dairy_PhenX <- DV_PhenX %>%
  select(SAMPLEID, Dairy_Score)


## 8.2 ECHO ----------------------------------------------------------------

# 1) Build age-specific weights for MILK_ECHO and CHEESE_ECHO
coef_dairy_echo <- tribble(
  ~ECHO_group, ~MILK_w, ~CHEESE_w, ~PIZZA_w, ~ICECREAM_w,
  "18-25",     1.000,   0.670, 0.896667, 0.17,
  "26-35",     1.000,   0.670, 0.870000, 0.18,
  "36-45",     1.000,   0.6625, 0.780000, 0.21,
  "46-60",     0.855,   0.670, 0.795000, 0.23
)

# 2) Vectorized Dairy_Score
DV_ECHO <- DV_ECHO %>%
  left_join(coef_dairy_echo, by = "ECHO_group") %>%
  mutate(
    Dairy_Score = 0.890477 +
      1.096476 * PIZZA_w * PIZZA_ECHO +
      0.518081 * CHEESE_w * CHEESE_ECHO +
      0.508564 * MILK_w   * MILK_ECHO +
      2.102278 * ICECREAM_w * ICECREAM_ECHO
  )

# 3) Final output table
Dairy_ECHO <- DV_ECHO %>%
  select(SAMPLEID, Dairy_Score)


# 9. Added Sugar ----------------------------------------------------------

## 9.1 PhenX ---------------------------------------------------------------

# 1) Build group-specific weights with *_w suffix to avoid name clashes
coef_added_sugar <- tribble(
  ~PhenX_group, ~SUGARSODA_w, ~SUGARFRUIT_DRINK_w, ~DOUGHNUT_w, ~COOKIES_w,
  "18-27",       9.815,        7.997,               2.966,       4.133,
  "28-37",       9.683,        7.876,               2.966,       3.650,
  "38-47",       9.683,        6.418,               2.797,       3.842,
  "48-57",       9.644,        6.002,               2.966,       3.719
)


# 2) Vectorized Added_Sugar_Score computation
DV_PhenX <- DV_PhenX %>%
  left_join(coef_added_sugar, by = "PhenX_group") %>%
  mutate(
    # inner linear combination
    .lin = SUGARSODA_w * SUGARSODA +
      SUGARFRUIT_DRINK_w * SUGARFRUIT_DRINK +
      DOUGHNUT_w * DOUGHNUT +
      COOKIES_w * COOKIES,
    Added_Sugar_Score = (1.591494 + 0.491231 * (.lin)^(1/3))^3
  ) %>%
  select(-.lin)

# 3) Final tidy output (no splitting/merging needed)
Added_Sugar_PhenX <- DV_PhenX %>%
  select(SAMPLEID, Added_Sugar_Score)


## 9.2 ECHO ----------------------------------------------------------------

# 1) Coefficients by age group
coef_added_sugar_echo <- tribble(
  ~ECHO_group, ~FROZEN_w, ~SODA_w, ~SUGARTEA_w, ~SPORTDRINK_w, 
  ~DOUGHNUT_w, ~COOKIES_w, ~CANDY_w,
  
  # 18–25
  "18-25", 3.53, 7.88, 0.6875, 6.67, 3.59, 3.225, 2.30,
  
  # 26–35
  "26-35", 4.79, 7.88, 0.62, 6.12, 2.68, 3.15, 1.97,
  
  # 36–45
  "36-45", 3.79, 7.88, 0.495, 5.41, 2.715, 3.05, 2.28,
  
  # 46–60
  "46-60", 4.33, 7.655, 0.014167, 4.5925, 2.60, 2.95, 2.12
)



# 2) Join and compute Added_Sugar_Score
DV_ECHO <- DV_ECHO %>%
  left_join(coef_added_sugar_echo, by = "ECHO_group") %>%
  mutate(
    Added_Sugar_Score =
      # Intercept
      9.98989 +
      # Frozen desserts (P15)
      1.058834 * FROZEN_w * ICECREAM_ECHO +
      # Soda (P16)
      0.676036 * SODA_w * SUGARSODA_ECHO +
      # Sugar/honey in coffee/tea (P17)
      2.958761 * SUGARTEA_w * SUGAR_HONEY_ECHO +
      # Fruitades/sports drinks (P18)
      0.4531 * SPORTDRINK_w * SWEETDRINK_ECHO +
      # Candy (P19)
      1.781276 * CANDY_w * CHOCOLATE_ECHO + 
      # Doughnuts (P20)
      (-0.064991) * DOUGHNUT_w * DOUGHNUT_ECHO +
      # Cookies, cake, pie, brownies (P21)
      0.275522 * COOKIES_w * COOKIES_ECHO
  )


# 3) Final output
Added_Sugar_ECHO <- DV_ECHO %>%
  select(SAMPLEID, Added_Sugar_Score)


# 10. Fruits and Veg ------------------------------------------------------

## 10.1 PhenX --------------------------------------------------------------

# 1) group-specific weights：PF (with fries, pyramid servings)

coef_FV_PF <- tribble(
  ~PhenX_group, ~FRUITJUICE_w, ~FRUIT_w, ~GREENSALAD_w, ~FRENCHFRIES_w,
  ~WHITEPOTATO_w, ~BEAN_w, ~OTHERVEGETABLES_w, ~TOMATOSAUCE_w, ~SALSA_w,
  "18-27", 1.5005, 1.168, 0.6135, 1.481, 1.544, 0.964, 0.7022, 0.541, 0.274,
  "28-37", 1.334,  1.168, 0.5275, 1.3655,1.544, 0.684, 0.7793, 0.541, 0.266,
  "38-47", 1.334,  1.168, 0.8333, 1.272, 1.528, 0.800, 0.7925, 0.273, 0.323,
  "48-57", 1.2513, 1.168, 1.0000, 1.400, 1.544, 0.687, 0.7885, 0.541, 0.238
) %>%
  rename_with(~ paste0(.x, "_PF"), -PhenX_group)


# 2) group-specific weights：CF (with fries, cups)

coef_FV_CF <- tribble(
  ~PhenX_group, ~FRUITJUICE_w, ~FRUIT_w, ~GREENSALAD_w, ~FRENCHFRIES_w,
  ~WHITEPOTATO_w, ~BEAN_w, ~OTHERVEGETABLES_w, ~TOMATOSAUCE_w, ~SALSA_w,
  "18-27", 1.12437, 0.74924, 0.30679, 0.50960, 0.78202, 0.49215, 0.36447, 0.27125, 0.13696,
  "28-37", 1.00096, 0.86730, 0.28634, 0.45511, 0.87695, 0.34155, 0.39588, 0.27125, 0.13324,
  "38-47", 1.00018, 0.84484, 0.41663, 0.44870, 0.77126, 0.43053, 0.40430, 0.13671, 0.16308,
  "48-57", 0.93813, 0.78900, 0.49995, 0.44870, 0.77126, 0.34576, 0.40833, 0.27125, 0.11919
) %>%
  rename_with(~ paste0(.x, "_CF"), -PhenX_group)



# 3) calculate four different FV scores（add suffix when join）

DV_PhenX <- DV_PhenX %>%
  # PF (with fries, pyramid servings)
  left_join(coef_FV_PF, by="PhenX_group") %>%
  mutate(
    lin_PF = FRUITJUICE * FRUITJUICE_w_PF + FRUIT * FRUIT_w_PF +
      GREENSALAD * GREENSALAD_w_PF + FRENCHFRIES * FRENCHFRIES_w_PF +
      WHITEPOTATO * WHITEPOTATO_w_PF + BEAN * BEAN_w_PF +
      OTHERVEGETABLES * OTHERVEGETABLES_w_PF + TOMATOSAUCE * TOMATOSAUCE_w_PF +
      SALSA * SALSA_w_PF,
    FV_Score_PF = (0.658819 + 0.796243 * sqrt(lin_PF))^2,
    
    # P (without fries, pyramid servings)
    lin_P = FRUITJUICE * FRUITJUICE_w_PF + FRUIT * FRUIT_w_PF +
      GREENSALAD * GREENSALAD_w_PF + WHITEPOTATO * WHITEPOTATO_w_PF +
      BEAN * BEAN_w_PF + OTHERVEGETABLES * OTHERVEGETABLES_w_PF +
      TOMATOSAUCE * TOMATOSAUCE_w_PF + SALSA * SALSA_w_PF,
    FV_Score_P = (0.639540 + 0.804796 * sqrt(lin_P))^2
  ) %>%
  select(-lin_PF, -lin_P) %>%
  
  # CF (with fries, cups)
  left_join(coef_FV_CF, by="PhenX_group") %>%
  mutate(
    lin_CF = FRUITJUICE * FRUITJUICE_w_CF + FRUIT * FRUIT_w_CF +
      GREENSALAD * GREENSALAD_w_CF + FRENCHFRIES * FRENCHFRIES_w_CF +
      WHITEPOTATO * WHITEPOTATO_w_CF + BEAN * BEAN_w_CF +
      OTHERVEGETABLES * OTHERVEGETABLES_w_CF + TOMATOSAUCE * TOMATOSAUCE_w_CF +
      SALSA * SALSA_w_CF,
    FV_Score_CF = (0.502480 + 0.792683 * sqrt(lin_CF))^2,
    
    # C (without fries, cups)
    lin_C = FRUITJUICE * FRUITJUICE_w_CF + FRUIT * FRUIT_w_CF +
      GREENSALAD * GREENSALAD_w_CF + WHITEPOTATO * WHITEPOTATO_w_CF +
      BEAN * BEAN_w_CF + OTHERVEGETABLES * OTHERVEGETABLES_w_CF +
      TOMATOSAUCE * TOMATOSAUCE_w_CF + SALSA * SALSA_w_CF,
    FV_Score_C = (0.495205 + 0.794978 * sqrt(lin_C))^2
    
  ) %>%
  select(-lin_CF, -lin_C)


# 6) final result

FV_PhenX <- DV_PhenX %>%
  select(SAMPLEID, FV_Score_PF, FV_Score_P, FV_Score_CF, FV_Score_C)


## 10.2 ECHO ---------------------------------------------------------------


# 1) CF（with french fries）weights
coef_FV_CF <- tribble(
  ~ECHO_group, ~FRUIT_w_CF, ~FRUITJUICE_w_CF, ~GREENSALAD_w_CF, ~FRENCHFRIES_w_CF,
  ~WHITEPOTATO_w_CF, ~BEAN_w_CF, ~OTHERVEGETABLES_w_CF, ~PIZZA_w_CF, ~SALSA_w_CF, ~TOMATOSAUCE_w_CF,
  "18-25", 0.76, 0.99, 0.28, 0.535, 0.57, 0.48, 0.4925, 0.175417, 0.14, 0.47,
  "26-35", 0.733, 0.94, 0.28, 0.535, 0.54, 0.495, 0.4775, 0.17, 0.14, 0.505,
  "36-45", 0.71, 0.826667, 0.4025, 0.435, 0.59, 0.43, 0.5, 0.17, 0.17, 0.47,
  "46-60", 0.71, 0.78375, 0.44, 0.4175, 0.54, 0.47, 0.5, 0.16, 0.09, 0.4425
)


# 3) calculate
DV_ECHO <- DV_ECHO %>%
  left_join(coef_FV_CF, by = "ECHO_group") %>%
  mutate(
    #CF
    FV_Score_CF =
      1.602535 +
      0.779829 * FRUIT_w_CF           * FRUIT_ECHO +
      0.291685 * FRUITJUICE_w_CF      * FRUITJUICE_ECHO +
      1.490937 * GREENSALAD_w_CF      * GREENSALAD_ECHO +
      -0.656475 * FRENCHFRIES_w_CF     * FRENCHFRIES_ECHO +
      0.075593 * WHITEPOTATO_w_CF     * WHITEPOTATO_ECHO +
      0.503731 * BEAN_w_CF            * BEAN_ECHO +
      -0.688108 * PIZZA_w_CF          * PIZZA_ECHO +
      0.456919 * OTHERVEGETABLES_w_CF * OTHERVEGETABLES_ECHO +
      -1.568485 * SALSA_w_CF           * SALSA_ECHO +
      0.339691 * TOMATOSAUCE_w_CF     * TOMATOSAUCE_ECHO,
    # C
    FV_Score_C =
      1.426327 +
      0.807196 * FRUIT_w_CF           * FRUIT_ECHO +
      0.296939 * FRUITJUICE_w_CF      * FRUITJUICE_ECHO +
      1.625649 * GREENSALAD_w_CF      * GREENSALAD_ECHO +
      -0.175942 * WHITEPOTATO_w_CF     * WHITEPOTATO_ECHO +
      0.606093 * BEAN_w_CF            * BEAN_ECHO +
      0.525190 * OTHERVEGETABLES_w_CF * OTHERVEGETABLES_ECHO +
      -0.688108 * PIZZA_w_CF          * PIZZA_ECHO +
      -1.669805 * SALSA_w_CF           * SALSA_ECHO +
      0.310125 * TOMATOSAUCE_w_CF     * TOMATOSAUCE_ECHO
  )

# 4) final result
FV_ECHO <- DV_ECHO %>%
  select(SAMPLEID, FV_Score_CF, FV_Score_C)

# 11. Calcium -------------------------------------------------------------

## 11.1 PhenX --------------------------------------------------------------

# 1) build the weights table
coef_Calcium <- tribble(
  ~PhenX_group, 
  ~CEREAL__COOKED_w, ~CEREAL__ALLBRAN_w, ~CEREAL__SOMEBRAN_w, ~CEREAL__LITTLEBRAN_w,
  ~MILK_w, ~SUGARSODA_w, ~SUGARFRUIT_DRINK_w, ~FRUITJUICE_w, ~FRUIT_w,
  ~GREENSALAD_w, ~FRENCHFRIES_w, ~WHITEPOTATO_w, ~BEAN_w,
  ~OTHERVEGETABLES_w, ~TOMATOSAUCE_w, ~SALSA_w, ~WHOLEGRAIN_w,
  ~DOUGHNUT_w, ~COOKIES_w, ~CHEESE_w,
  
  # 18–27
  "18-27", 
  234, 42.75, 60, 46.5, 244, 372, 360, 280.125, 118,
  33.75, 79.5, 122, 132.75,
  56, 125, 32, 50,
  67.3333, 56.7, 28.25,
  
  # 28–37
  "28-37", 
  234, 42.75, 57, 37.5, 244, 372, 341, 249, 118,
  32.0833, 70, 127, 126.5,
  62.0433, 113.4, 31.13, 48,
  58, 50, 24,
  
  # 38–47
  "38-47", 
  234, 42.75, 53, 36.25, 244, 370.2, 250, 248.8, 118,
  47, 70, 119, 126.5,
  64.415, 62.5, 36.565, 47.5,
  57, 48.8, 24,
  
  # 48–57
  "48-57", 
  234, 42.75, 49.5, 33, 214.25, 368.4, 250, 233.25, 118,
  55, 70, 113, 141.75,
  64.92, 125, 27.8475, 45,
  58.5, 55.2, 26.25
)


# 2) clean the previous weights

DV_PhenX <- DV_PhenX %>%
  select(-ends_with("_w"))

# 3) calculate Calcium_Score
DV_PhenX <- DV_PhenX %>%
  left_join(coef_Calcium, by = "PhenX_group") %>%
  mutate(
    lin = 4.155762 +
      0.000484*CEREAL__COOKED_w*CEREAL__COOKED +
      0.006744*CEREAL__ALLBRAN_w*CEREAL__ALLBRAN +
      0.000074*CEREAL__SOMEBRAN_w*CEREAL__SOMEBRAN +
      -0.001305*CEREAL__LITTLEBRAN_w*CEREAL__LITTLEBRAN +
      0.002580*MILK_w*MILK +
      0.000095*SUGARSODA_w*SUGARSODA +   
      0.000326*SUGARFRUIT_DRINK_w*SUGARFRUIT_DRINK +
      0.000195*FRUITJUICE_w*FRUITJUICE +
      0.000264*FRUIT_w*FRUIT +
      -0.000723*GREENSALAD_w*GREENSALAD +
      0.000414*FRENCHFRIES_w*FRENCHFRIES +
      0.000489*WHITEPOTATO_w*WHITEPOTATO +
      0.001035*BEAN_w*BEAN +
      0.000396*OTHERVEGETABLES_w*OTHERVEGETABLES +
      0.000287*TOMATOSAUCE_w*TOMATOSAUCE +
      0.002679*SALSA_w*SALSA +
      0.000680*WHOLEGRAIN_w*WHOLEGRAIN +
      0.001873*DOUGHNUT_w*DOUGHNUT +
      0.002451*COOKIES_w*COOKIES +
      0.015442*CHEESE_w*CHEESE,
    Calcium_Score = lin^4
  ) %>%
  select(-ends_with("_w"), -lin)

Calcium_PhenX <- DV_PhenX %>%
  select(SAMPLEID, Calcium_Score)


## 11.2 ECHO ---------------------------------------------------------------

# 1) build the weights table
coef_Calcium_ECHO <- tribble(
  ~ECHO_group, 
  ~FRUIT_c, ~FRUITJUICE_c, ~GREENSALAD_c, ~FRENCHFRIES_c, ~WHITEPOTATO_c, 
  ~BEAN_c, ~OTHERVEGETABLES_c, ~PIZZA_c, ~SALSA_c, ~TOMATOSAUCE_c, 
  ~CHEESE_c, ~MILK_c, ~FROZENDESSERT_c, ~SUGARSODA_c, ~SUGARTEA_c, 
  ~SWEETDRINK_c, ~DOUGHNUT_c, ~COOKIES_c, ~CANDY_c, ~WHOLEGRAIN_c, ~BROWNRICE_c, ~POPCORN_c,
  
  # 18–25
  "18-25", 
  128.065, 249, 30, 84, 105, 
  94.88, 68.75, 171.255, 26.7, 248.4, 
  25.5, 244, 103.1875, 368, 183.05, 
  337.126667, 76.5, 41, 21.25, 52, 147, 29.315,
  
  # 26–35
  "26-35", 
  116.7333, 248, 30, 82.5, 122, 
  89, 73.7525, 157.77, 30, 255.75, 
  26.0125, 244, 108.56, 368, 208.6, 
  350.75, 69.5, 40.4, 18.00, 52, 142.6283, 28.175,
  
  # 36–45
  "36-45", 
  120.2213, 219.5, 41.25, 69.97, 114, 
  86.24, 67.3667, 165.9, 32, 248, 
  24.8063, 244, 111, 368, 231.375, 
  325.5, 70, 39.6967, 20.535, 50, 136.4575, 26,
  
  # 46–60
  "46-60", 
  116.625, 201.905, 45.9283, 65.8333, 113.5, 
  94.88, 70, 145.35, 17.8, 217, 
  24.15, 214.375, 111, 357.6, 236.8, 
  323.345, 70, 40.1, 20.00, 48, 133.8867, 39.67
)



# 2) calculate Calcium_Score
DV_ECHO <- DV_ECHO %>%
  select(-ends_with("_c")) %>%
  left_join(coef_Calcium_ECHO, by = "ECHO_group") %>%
  mutate(
    Calcium_Score =
      675.518677 +
      0.200896*FRUIT_c*FRUIT_ECHO +
      -0.054166*FRUITJUICE_c*FRUITJUICE_ECHO +
      0.498547*GREENSALAD_c*GREENSALAD_ECHO +
      0.099815*FRENCHFRIES_c*FRENCHFRIES_ECHO +
      -0.013050*WHITEPOTATO_c*WHITEPOTATO_ECHO +
      -0.678153*BEAN_c*BEAN_ECHO +
      0.127265*OTHERVEGETABLES_c*OTHERVEGETABLES_ECHO +
      1.509323*PIZZA_c*PIZZA_ECHO + 
      -0.546591*SALSA_c*SALSA_ECHO +
      0.326682*TOMATOSAUCE_c*TOMATOSAUCE_ECHO +
      4.406310*CHEESE_c*CHEESE_ECHO +
      0.518032*MILK_c*MILK_ECHO +
      1.661194*FROZENDESSERT_c*ICECREAM_ECHO +
      -0.090310*SUGARSODA_c*SUGARSODA_ECHO +
      -0.063877*SUGARTEA_c*SUGAR_HONEY_ECHO + 
      -0.078212*SWEETDRINK_c*SWEETDRINK_ECHO +
      0.624241*DOUGHNUT_c*DOUGHNUT_ECHO +
      -0.619321*COOKIES_c*COOKIES_ECHO +
      0.334033*CANDY_c * CHOCOLATE_ECHO+
      0.531902*BROWNRICE_c*RICE_ECHO+
      0.361698*WHOLEGRAIN_c*WHOLEGRAIN_ECHO+
      3.13212*POPCORN_c*POPCORN_ECHO
  ) %>%
  select(-ends_with("_c"))

Calcium_ECHO <- DV_ECHO %>%
  select(SAMPLEID, Calcium_Score)


# 12. Merge Scores --------------------------------------------------------

score_list_PhenX <- list(
  Fiber_PhenX,
  Dairy_PhenX,
  Calcium_PhenX,
  FV_PhenX,
  Added_Sugar_PhenX
)

Score_PhenX <- reduce(score_list_PhenX, full_join, by = "SAMPLEID")

score_list_ECHO <- list(
  Fiber_ECHO,
  Dairy_ECHO,
  Calcium_ECHO,
  FV_ECHO,
  Added_Sugar_ECHO
)

Score_ECHO <- reduce(score_list_ECHO, full_join, by = "SAMPLEID")


# 13. Save Datasets -------------------------------------------------------


common_vars <- c(
  "SAMPLEID", 
  "Fiber_Score", 
  "Dairy_Score", 
  "Calcium_Score", 
  "FV_Score_CF", 
  "FV_Score_C", 
  "Added_Sugar_Score"
)

# combine
Score_Combined <- bind_rows(
  Score_PhenX %>% select(all_of(common_vars)) %>% mutate(Source = "PhenX"),
  Score_ECHO  %>% select(all_of(common_vars)) %>% mutate(Source = "DSQ")
)

library(openxlsx)

notes <- c(
  "1. This Dietary Screening Questionnaire Scores were calculated by Jiahe Tian in Oct. 2025.",
  "2. Fiber score: For PhenX, https://www.phenxtoolkit.org/protocols/view/50601. For ECHO, https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/table25.html",
  "3. Dairy score: For PhenX, https://www.phenxtoolkit.org/protocols/view/50401. For ECHO, https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/table20.html",
  "4. Added Sugar: For PhenX, https://www.phenxtoolkit.org/protocols/view/51001. For ECHO, https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/table21.html",
  "5. Fruit and Vegetables: For PhenX, https://www.phenxtoolkit.org/protocols/view/50701. For ECHO, https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/table15.html",
  "6. Calcium: For PhenX, https://www.phenxtoolkit.org/protocols/view/50202. For ECHO, https://epi.grants.cancer.gov/nhanes/dietscreen/scoring/current/table27.html"
)

wb <- createWorkbook()
addWorksheet(wb, "Combined_Scores")
addWorksheet(wb, "Notes")

writeData(wb, "Combined_Scores", Score_Combined)
writeData(wb, "Notes", notes)

saveWorkbook(
  wb,
  file = "Z:/ECHO/CHARM/Data/Code Derived/PN Dietary/PhenX and DSQ scores combined.xlsx",
  overwrite = TRUE
)


# 14. Compare PhenX and ECHO group ----------------------------------------


## 14.1 Income -------------------------------------------------------------



INCOME <- read_csv("Z:/ECHO/CHARM/Data/Code Derived/Income/PN1_Income_Code.csv") %>%
  rename(SAMPLEID = march_id) %>%
  mutate(
    Income_Code = case_when(
      Prenatal_final_income == "Less than $10,000" ~ 1,
      Prenatal_final_income == "$10,000-$14,999" ~ 2,
      Prenatal_final_income == "$15,000-$19,999" ~ 3,
      Prenatal_final_income == "$20,000-$24,999" ~ 4,
      Prenatal_final_income == "Less than $25,000" ~ 5,
      Prenatal_final_income == "$25,000-$34,999" ~ 6,
      Prenatal_final_income %in% c("$35,000-$49,999", "$50,000-$74,999") ~ 7,
      Prenatal_final_income == "$75,000-$100,000" ~ 8,
      Prenatal_final_income == "Greater than $100,000" ~ 9,
      Prenatal_final_income == "Don't Know/Refuse" ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

Score_with_Income <- Score_Combined %>%
  left_join(INCOME %>% select(SAMPLEID, Income_Code), by = "SAMPLEID")

income_dist <- Score_with_Income %>%
  filter(!is.na(Income_Code)) %>%
  count(Source, Income_Code) %>%
  group_by(Source) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  )

income_dist

income_table <- table(Score_with_Income$Source, Score_with_Income$Income_Code)

# p-value < 2.2e-16，significantly different 
chisq.test(income_table)


ggplot(income_dist, aes(x = factor(Income_Code), y = pct, fill = Source)) +
  geom_col(position = "dodge") +
  labs(
    x = "Income Category (1 = lowest, 9 = highest)",
    y = "Percentage within Source",
    title = "Prenatal Income Distribution by Source"
  ) +
  scale_fill_manual(values = c("#00A6D6", "#E69F00")) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# 14.2 Age ----------------------------------------------------------------


PN1 <- PN1 %>%
  rename(SAMPLEID = march_id)

Score_with_ConsentAge <- Score_Combined %>%
  left_join(PN1 %>% select(SAMPLEID, consent_age), by = "SAMPLEID")

age_summary <- Score_with_ConsentAge %>%
  group_by(Source) %>%
  summarise(
    n = sum(!is.na(consent_age)),
    mean_age = mean(consent_age, na.rm = TRUE),
    median_age = median(consent_age, na.rm = TRUE),
    sd_age = sd(consent_age, na.rm = TRUE),
    min_age = min(consent_age, na.rm = TRUE),
    max_age = max(consent_age, na.rm = TRUE)
  )

age_summary

# p-value = 9.203e-05, not normal distribution
by(Score_with_ConsentAge$consent_age, Score_with_ConsentAge$Source, shapiro.test)

# p-value = 0.1211, not significant difference
wilcox.test(consent_age ~ Source, data = Score_with_ConsentAge)

ggplot(Score_with_ConsentAge, aes(x = Source, y = consent_age, fill = Source)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  labs(
    x = "Source",
    y = "Maternal Age at Consent",
    title = "Maternal Age Distribution by Source (PhenX vs ECHO)"
  ) +
  scale_fill_manual(values = c("#00A6D6", "#E69F00")) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# 14.3 Race and Ethnicity -------------------------------------------------


RaceEth <- read_csv("Z:/ECHO/CHARM/Data/Code Derived/Race_Ethnicity/MARCH_Race_Mom_breakdown.csv") %>%
  rename(SAMPLEID = march_id)

Score_with_RaceEth <- Score_Combined %>%
  left_join(RaceEth %>% select(SAMPLEID, Ethnicity, Race), by = "SAMPLEID")

# Ethnicity 分布
eth_dist <- Score_with_RaceEth %>%
  count(Source, Ethnicity) %>%
  group_by(Source) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# Race 分布
race_dist <- Score_with_RaceEth %>%
  count(Source, Race) %>%
  group_by(Source) %>%
  mutate(pct = round(100 * n / sum(n), 1))

eth_dist
race_dist


# 构建列联表
eth_table <- table(Score_with_RaceEth$Source, Score_with_RaceEth$Ethnicity)

# 如果每格样本量都 > 5，用卡方检验
chisq.test(eth_table)

race_table <- table(Score_with_RaceEth$Source, Score_with_RaceEth$Race)

chisq.test(race_table)

library(ggplot2)

ggplot(eth_dist, aes(x = Source, y = pct, fill = Ethnicity)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Source",
    y = "Proportion",
    title = "Distribution of Maternal Ethnicity by Source"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggplot(race_dist, aes(x = Source, y = pct, fill = Race)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Source",
    y = "Proportion",
    title = "Distribution of Maternal Race by Source"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# 14.4 Education ----------------------------------------------------------

EDUC <- read_csv("Z:/ECHO/CHARM/Data/ECHO 1/SR0 Data/ECHOsftp_final_20230918/ECHOsftp/Phase_1/Data_Delivery/Prenatal_Survey1/PRENATAL_1_SURVEY_mixedformats.csv") %>%
  select(SAMPLEID, EDUC_LVL)

EDUC <- EDUC %>%
  mutate(
    EDUC_LVL = as.numeric(EDUC_LVL),
    EDUC_cat = case_when(
      EDUC_LVL == 1 ~ "None",
      EDUC_LVL == 2 ~ "8th grade or less",
      EDUC_LVL == 3 ~ "Some high school, no diploma",
      EDUC_LVL == 4 ~ "High school graduate or GED",
      EDUC_LVL == 5 ~ "Some college credit, no degree",
      EDUC_LVL == 6 ~ "Trade/Technical/Vocational training",
      EDUC_LVL == 7 ~ "Associate degree",
      EDUC_LVL == 8 ~ "Bachelor's degree",
      EDUC_LVL == 9 ~ "Master's degree",
      EDUC_LVL == 10 ~ "Doctorate or professional degree",
      EDUC_LVL %in% c(98, 99) ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

Score_with_Educ <- Score_Combined %>%
  left_join(EDUC %>% select(SAMPLEID, EDUC_LVL, EDUC_cat), by = "SAMPLEID")

educ_dist <- Score_with_Educ %>%
  count(Source, EDUC_cat) %>%
  group_by(Source) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  ) %>%
  arrange(Source, desc(pct))

educ_dist

educ_table <- table(Score_with_Educ$Source, Score_with_Educ$EDUC_cat)

# significantly different
chisq.test(educ_table)

# if one cell n < 5，use Fisher test
fisher.test(educ_table)



ggplot(educ_dist, aes(x = Source, y = pct, fill = EDUC_cat)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Source",
    y = "Proportion",
    title = "Maternal Education Level Distribution by Source",
    fill = "Education Level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

