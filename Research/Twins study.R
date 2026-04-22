rm(list = ls())


# 1. Load Library ---------------------------------------------------------

library(tidyverse)


# 2. Import Data ----------------------------------------------------------


# DEFINE PATHS 
path_arch_bc  <- "Z:/ECHO/CHARM/Data/ECHO 1/MDHHS Data/Birth_Certificate/ARCH_BC_Combined.csv"
path_march_bc <- "Z:/ECHO/CHARM/Data/ECHO 1/MDHHS Data/Birth_Certificate/MARCH_BC_FinalUpd.csv"

# Read ARCH birth certificate data
arch_bc <- read_csv(
  file = path_arch_bc,
  col_types = cols(),        # auto-detect column types safely
  name_repair = "unique"     # ensure unique column names
)

# Read MARCH birth certificate data
march_bc <- read_csv(
  file = path_march_bc,
  col_types = cols(),
  name_repair = "unique"
)

# Check dimensions
dim(arch_bc)
dim(march_bc)

table(march_bc$PLURALITY)
table(arch_bc$PLURALITY)


# 3. Data Manipulation ----------------------------------------------------

bc_all <- bind_rows(
  march_bc %>% 
    mutate(cohort = "MARCH") %>% 
    mutate(sampleid = as.character(sampleid)) %>%
    select(sampleid, 
           GRAMS, 
           cohort,
           PLURALITY,
           SEX,
           APGAR5MIN,
           CALCGEST,
           ESTWKSGEST
    ),
  arch_bc  %>% 
    mutate(cohort = "ARCH") %>% 
    mutate(sampleid = as.character(arch_id)) %>%
    select(sampleid, 
           GRAMS,
           cohort,
           PLURALITY,
           SEX,
           APGAR5MIN,
           CALCGEST,
           ESTWKSGEST
    )
  ) %>%
  mutate(
    # ------------------ Core variables ------------------
    birth_weight = as.numeric(GRAMS),
    
    gest_age = as.numeric(CALCGEST), # weeks
    
    # fallback if CALCGEST missing
    gest_age = ifelse(is.na(gest_age), as.numeric(ESTWKSGEST), gest_age), # some have 12 weeks as gest_age, need to work on werid values
    
    apgar5 = as.numeric(APGAR5MIN),
    apgar5 = ifelse(apgar5 == 99, NA, apgar5),
    
    sex = as.factor(SEX),
    
    # ------------------ Twin indicator ------------------
    is_twin = ifelse(PLURALITY == 2, 1, 0),
    
    # ------------------ Derived outcomes ------------------
    low_bw = ifelse(birth_weight < 2500, 1, 0),
    
    preterm = ifelse(gest_age < 37, 1, 0)
  ) %>%
  mutate(
    sex = recode(sex,
                 "1" = "Male",
                 "2" = "Female")
  )



# 4. Demographic ----------------------------------------------------------


## 4.1 Weight --------------------------------------------------------------


# combine cohorts

# ------------------------------ COMBINE DATA 


# ------------------------------ CLEAN WEIGHT 
bc_all <- bc_all %>%
  mutate(
    birth_weight = as.numeric(GRAMS)
  ) %>%
  filter(!is.na(birth_weight))

# ------------------------------ BASIC SUMMARY 

library(gt)

summary_table <- bc_all %>% 
  group_by(is_twin) %>% 
  summarise( 
    n = n(), 
    mean = mean(birth_weight, na.rm = TRUE), 
    median = median(birth_weight, na.rm = TRUE), 
    sd = sd(birth_weight, na.rm = TRUE), 
    IQR = IQR(birth_weight, na.rm = TRUE) 
    )

gt_table <- summary_table %>%
  mutate(
    is_twin = ifelse(is_twin == 1, "Twin", "Singleton")
  ) %>%
  gt() %>%
  cols_label(
    is_twin = "Group",
    n = "N",
    mean = "Mean",
    median = "Median",
    sd = "SD",
    IQR = "IQR"
  ) %>%
  fmt_number(
    columns = c(mean, median, sd, IQR),
    decimals = 1
  ) %>%
  tab_header(
    title = "ARCH + MARCH"
  )


gtsave(
  gt_table,
  filename = "C:/Users/tianjiah/OneDrive - Michigan State University/Data Manager/Data-Manager/Research/Twin Study/Figure/summary_birth_weight.png"
)


# Density plot
p_dens_overall <- ggplot(bc_all, aes(x = birth_weight, fill = factor(is_twin))) +
  geom_density(alpha = 0.4) +
  labs(fill = "Twin (1=Yes)", x = "Birth Weight (grams)", y = "Density") +
  theme_minimal()

# Boxplot
ggplot(bc_all, aes(x = factor(is_twin), y = birth_weight)) +
  geom_boxplot() +
  labs(x = "Twin Status", y = "Birth Weight (grams)") +
  theme_minimal()

# Sample (avoid huge N issue)
bc_sample <- bc_all %>% sample_n(500)

shapiro.test(bc_sample$birth_weight[bc_sample$is_twin == 0])
shapiro.test(bc_sample$birth_weight[bc_sample$is_twin == 1])


# save the plot
output_path <- "C:/Users/tianjiah/OneDrive - Michigan State University/Data Manager/Data-Manager/Research/Twin Study/Figure"

ggsave(
  filename = file.path(output_path, "density_birth_weight_twin.png"),
  plot = p_dens_overall,
  width = 8,
  height = 4,
  dpi = 300
)



# add gestational age很重要


# separate cohort
summary_table <- bc_all %>% 
  group_by(cohort, is_twin) %>% 
  summarise( 
    n = n(), 
    mean = mean(birth_weight), 
    median = median(birth_weight), 
    sd = sd(birth_weight), 
    IQR = IQR(birth_weight), 
    .groups = "drop" 
    )

gt_summary_table <- summary_table %>%
  mutate(
    cohort = as.character(cohort),
    is_twin = ifelse(is_twin == 1, "Twin", "Singleton")
  ) %>%
  gt(groupname_col = "cohort") %>%
  cols_label(
    is_twin = "Group",
    n = "N",
    mean = "Mean",
    median = "Median",
    sd = "SD",
    IQR = "IQR"
  ) %>%
  fmt_number(
    columns = c(mean, median, sd, IQR),
    decimals = 1
  ) %>%
  tab_header(
    title = "Birth Weight Summary by Twin Status",
    subtitle = "Stratified by Cohort"
  )

# Preview in RStudio viewer
gt_summary_table


output_path <- "C:/Users/tianjiah/OneDrive - Michigan State University/Data Manager/Data-Manager/Research/Twin Study/Figure"
gtsave(
  gt_summary_table,
  filename = file.path(output_path, "summary_birth_weight_by_cohort.png")
)


test_results <- bc_all %>%
  group_by(cohort) %>%
  group_modify(~{
    
    # Wilcoxon test
    test <- wilcox.test(birth_weight ~ is_twin, data = .x)
    
    tibble(
      p_value = test$p.value,
      method = "Wilcoxon rank-sum"
    )
  })

test_results

p_dens_cohort <- ggplot(bc_all, aes(x = birth_weight, fill = factor(is_twin))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~cohort) +
  labs(fill = "Twin (1=Yes)", x = "Birth Weight (grams)", y = "Density") +
  theme_minimal()

ggsave(
  filename = file.path(output_path, "density_birth_weight_twin_cohort.png"),
  plot = p_dens_cohort,
  width = 8,
  height = 4,
  dpi = 300
)


ggplot(bc_all, aes(x = factor(is_twin), y = birth_weight)) +
  geom_boxplot() +
  facet_wrap(~cohort) +
  labs(x = "Twin Status", y = "Birth Weight (grams)") +
  theme_minimal()



## 4.2 Gestational Age -----------------------------------------------------
p_gest_age <- ggplot(bc_all, aes(x = gest_age, fill = factor(is_twin))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~cohort) +
  labs(fill = "Twin", x = "Gestational Age (weeks)", y = "Density") +
  theme_minimal()

ggsave(
  filename = file.path(output_path, "density_gestational_age_twin_cohort.png"),
  plot = p_gest_age,
  width = 8,
  height = 4,
  dpi = 300
)


gest_test <- bc_all %>%
  group_by(cohort) %>%
  group_modify(~{
    test <- wilcox.test(gest_age ~ is_twin, data = .x)
    tibble(p_value = test$p.value)
  })

gest_test



## 4.3 Low Birth Weight / Preterm ------------------------------------------

lbw_summary <- bc_all %>%
  group_by(cohort, is_twin) %>%
  summarise(
    low_bw_rate = mean(low_bw, na.rm = TRUE),
    preterm_rate = mean(preterm, na.rm = TRUE),
    .groups = "drop"
  )

lbw_test <- bc_all %>%
  group_by(cohort) %>%
  group_modify(~{
    
    lbw_p <- chisq.test(table(.x$is_twin, .x$low_bw))$p.value
    preterm_p <- chisq.test(table(.x$is_twin, .x$preterm))$p.value
    
    tibble(
      lbw_p_value = lbw_p,
      preterm_p_value = preterm_p
    )
  })

lbw_test



## 4.4 Sex Distribution ----------------------------------------------------
sex_test <- bc_all %>%
  group_by(cohort) %>%
  group_modify(~{
    
    tab <- table(.x$is_twin, .x$sex)
    
    tibble(
      male_pct_twin = mean(.x$sex[.x$is_twin == 1] == "Male", na.rm = TRUE),
      male_pct_singleton = mean(.x$sex[.x$is_twin == 0] == "Male", na.rm = TRUE),
      p_value = chisq.test(tab)$p.value
    )
  })

sex_test



## 4.5 Apgar Score---------------------------------------------------------------

apgar_test <- bc_all %>%
  group_by(cohort) %>%
  group_modify(~{
    
    test <- wilcox.test(apgar5 ~ is_twin, data = .x)
    
    tibble(p_value = test$p.value)
  })

apgar_test




p_score <- ggplot(bc_all, aes(x = apgar5, fill = factor(is_twin))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~cohort) +
  labs(fill = "Twin", x = "Apgar Scores", y = "Density") +
  theme_minimal()

ggsave(
  filename = file.path(output_path, "density_score_twin_cohort.png"),
  plot = p_score,
  width = 8,
  height = 4,
  dpi = 300
)




final_results <- gest_test %>%
  rename(gest_p = p_value) %>%
  left_join(lbw_test, by = "cohort") %>%
  left_join(sex_test %>% select(cohort, sex_p = p_value), by = "cohort") %>%
  left_join(apgar_test %>% rename(apgar_p = p_value), by = "cohort")

final_results
