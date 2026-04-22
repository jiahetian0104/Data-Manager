

library(tidyverse)

# Define file paths
arch_bc_path  <- "Z:/ECHO/CHARM/Data/ECHO 1/MDHHS Data/Birth_Certificate/ARCH_BC_Combined.csv"
march_bc_path <- "Z:/ECHO/CHARM/Data/ECHO 1/MDHHS Data/Birth_Certificate/MARCH_BC_FinalUpd.csv"

# Read ARCH birth certificate data
arch_bc <- read_csv(arch_bc_path, show_col_types = FALSE)

# Read MARCH birth certificate data
march_bc <- read_csv(march_bc_path, show_col_types = FALSE)

# Convert ID variables to character (important for merging later)
arch_bc <- arch_bc %>%
  mutate(arch_id = as.character(arch_id))

march_bc <- march_bc %>%
  mutate(sampleid = as.character(sampleid))

# Quick check
glimpse(arch_bc)
glimpse(march_bc)

arch_bc$MD_FINAL_ROUTE
march_bc$MD_FINAL_ROUTE 

class(arch_bc$MD_FINAL_ROUTE)



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


arch_bc$


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



march_bc_clean <- march_bc %>%
  
  # 1️⃣ Create Singleton variable
  mutate(
    Singleton = case_when(
      PLURALITY == 1 ~ 1,
      PLURALITY > 1  ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # 2️⃣ Create Maternal Age
  mutate(
    maternal_age = BXYEAR - MOMBXYR
  ) %>%
  
  # 3️⃣ Remove unrealistic age (QC step)
  mutate(
    maternal_age = if_else(
      maternal_age < 10 | maternal_age > 60,
      NA_real_,
      maternal_age
    )
  ) %>%
  
  # 4️⃣ Create delivery binary
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


table(march_bc_clean$Singleton, useNA = "ifany")
summary(march_bc_clean$maternal_age)
table(march_bc_clean$delivery_binary, useNA = "ifany")

march_bc_clean %>%
  count(delivery_binary, Singleton) %>%
  group_by(Singleton) %>%
  mutate(prop = n / sum(n))
