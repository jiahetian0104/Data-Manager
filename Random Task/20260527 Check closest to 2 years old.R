# 1. Load Packages --------------------------------------------------------

library(tidyverse)
library(readxl)
library(httr2) # Use API

# 2. Import Data ----------------------------------------------------------


## 2.1. Set up parameters ----------------------------------------------------

base_url <- "https://echocharm.ripplescience.com/v1/export"

auth_key <- "Basic dGlhbmppYWhAbXN1LmVkdTpUamg2MTI0MjUyMDAwMDEwNCoq"

# export-type from DevTools payload
study_id <- "REakqQKvCboEdX7BL"

team_id <- "pze6EXgGw6hLwhhRy"

timezone <- "America/New_York"


## 2.2 Variables selected in Ripple export UI ------------------------------------

vars <- c(
  "globalId",
  "birthday",
  "tags",
  "statusId",
  "Events (All or None)"
)


## 2.3 Build Body ---------------------------------------------------------------

body_list <- list(
  "access_token" = "",   # optional, usually not needed if using Authorization header
  "teamId" = team_id,
  "export-type" = study_id,
  "export-timezone" = timezone,
  "surveyExportSince" = ""
)

# Add selected variables
for (v in vars) {
  body_list[[v]] <- "on"
}


## 2.4. Data Request ---------------------------------------------------------

resp <- request(base_url) %>%
  req_headers(
    Authorization = auth_key
  ) %>%
  req_body_form(!!!body_list) %>%
  req_perform()


# Check status
resp_status(resp)


## 2.5 Get CSV text --------------------------------------------------------------

csv_text <- resp_body_string(resp)


## 2.6 Read into dataframe -------------------------------------------------------

ripple_data <- read_csv(
  I(csv_text),
  show_col_types = FALSE,
  guess_max = 5000
)

class(ripple_data$birthday)

# 3. Identify participants under age 2 -------------------------------------

current_date <- as.Date("2026-05-27")

under_2_closest <- ripple_data %>%
  mutate(
    # Convert birthday from character in mm/dd/yyyy format to Date
    birthday_date = mdy(birthday),
    
    # Calculate the second birthday
    second_birthday = birthday_date %m+% years(2),
    
    # Calculate age in days as of current_date
    age_days = as.numeric(current_date - birthday_date),
    
    # Calculate days until turning 2 years old
    days_until_2 = as.numeric(second_birthday - current_date)
  ) %>%
  filter(
    !is.na(birthday_date),
    days_until_2 > 0
  ) %>%
  arrange(days_until_2) %>%
  select(
    globalId,
    birthday,
    statusId,
    tags,
    birthday_date,
    second_birthday,
    age_days,
    days_until_2
  )

under_2_closest
