### Zero Bill Analysis ###
## MVH 11/24 ##

library(DBI)
library(RSQLite)
library(tidyverse)

# Load random files

eq_factor <- read_csv("./Necessary_Files/eq_factor.csv") |>
  select(year, eq_factor_final)

cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  mutate(class = as.character(class_code)) |>  # rename to match other data frames
  select(-c(loa_2022, Option2, class_desc, land, vacant_ind, last2dig,
            Res_nonRes, assessment_level, used_in2021, class_code)) %>%
  mutate_at(.vars = c("improvement_ind", "incent_prop", "class_1dig", "major_class_code"), .funs = as.character
  )

# Connect to the database
con <- dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")

# Pull residential PINs between 2010 and 2023 with bills of 0 or eq_av less than 150
# For eq factor, used the upper bound of 3.3 to capture all the low-eav bills.
pin_data <- dbGetQuery(con, "
SELECT
  *
FROM pin
WHERE
  year BETWEEN 2010 AND 2023
  AND substr(CAST(class as TEXT), 1, 1) = '2'
  AND (
    (av_clerk * 3.3) < 150
    OR tax_bill_total = 0
  )")

pin_data <- pin_data |>
  left_join(eq_factor, by = "year") |>
  mutate(zero_bill = ifelse(tax_bill_total == 0, 1, 0)) |>
  mutate(eq_av = av_clerk*eq_factor_final) |>
  mutate(exe_total = rowSums(across(starts_with("exe_")))) |>
  mutate(eav = eq_av - exe_total) |>
  mutate(no_eav = ifelse(eav <= 0, 1, 0)) |>
  mutate(exemps_no_eav = ifelse(eq_av < exe_total, 1, 0)) |>
  select(-av_mailed, -av_certified, -av_board)

# Section 18-40 Zero Dollar Bills ----------------------------------

# These bills should have an EAV < $150 and > 0 after accounting for exemptions

pin_data |>
  filter(eav > 0 & zero_bill == 1) |>
  group_by(year) |>
  summarize(n = n(), sum(eq_av), sum(exe_total), sum(eav))
