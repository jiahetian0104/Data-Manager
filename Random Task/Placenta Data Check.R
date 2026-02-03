library(tidyverse)
library(readxl)

# Load derived CSV
placenta_2d <- read_csv(
  "Z:/ftps/Liu/Data/Placenta/Plancenta 2D data.csv",
  show_col_types = FALSE
)

# Load original Excel
placenta_raw <- read_excel(
  "Z:/ECHO/CHARM/Data/Biospecimen/Bioassays/Placenta/placenta/salafia/MSU_SkeltraceOutput_July2021.xls"
)

# Column name comparison
common_vars <- intersect(names(placenta_2d), names(placenta_raw))
only_in_2d  <- setdiff(names(placenta_2d), names(placenta_raw))
only_in_raw <- setdiff(names(placenta_raw), names(placenta_2d))

list(
  common_variables = common_vars,
  only_in_2d_data  = only_in_2d,
  only_in_raw_data = only_in_raw
)

# the results show that 2d data is from the raw data