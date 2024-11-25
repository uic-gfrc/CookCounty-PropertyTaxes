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

taxcode_rates  <- dbGetQuery(con, "
SELECT DISTINCT year, tax_code_num, tax_code_rate
FROM tax_code
                             ")


pin_data <- pin_data |>
  left_join(eq_factor, by = "year") |>
  left_join(taxcode_rates, by = c("year", "tax_code_num")) %>%
  mutate(zero_bill = ifelse(tax_bill_total == 0, 1, 0)) |>
  mutate(eq_av = av_clerk*eq_factor_final) |>
  mutate(exe_total = rowSums(across(starts_with("exe_")), na.rm=T)) |>
  mutate(taxed_eav = eq_av - exe_total) |>
  mutate(no_eav = ifelse(taxed_eav <= 0, 1, 0)) |>
  mutate(exemps_no_eav = ifelse(eq_av < exe_total, 1, 0)) |>
  select(-av_mailed, -av_certified, -av_board)

# All PINs that didn't pay tax bills -------------------------------

# Over 27,000 residential PINs did not have to pay a tax bill in 2023.
pin_data |>
  filter(
           tax_bill_total < 1) |>
  group_by(year) |>
  summarize(n = n(), eq_av = sum(av_clerk*eq_factor_final, na.rm=TRUE), 
            total_exempt = sum(exe_total), taxed_eav = sum(av_clerk*eq_factor_final - exe_total, na.rm=T))


# Section 18-40 Zero Dollar Bills ----------------------------------

# These bills should have an EAV < $150 and > 0 after accounting for exemptions
# but they tend to have EAVs much greater than 150, even after exemptions are subtracted
pin_data |>
  filter(taxed_eav > 0 & tax_bill_total == 0) |>
  group_by(year) |>
  summarize(n = n(), eq_av = sum(av_clerk*eq_factor_final, na.rm=TRUE), 
            total_exempt = sum(exe_total), taxed_eav = sum(av_clerk*eq_factor_final - exe_total, na.rm=T),
            billed = sum(tax_bill_total),
            max_av = max((av_clerk*eq_factor_final- exe_total)/eq_factor_final),
            max_eav = max(av_clerk*eq_factor_final),
            max_taxable_eav = max(av_clerk*eq_factor_final- exe_total))


# double checking
pin_data |> filter(exe_total < av_clerk*eq_factor_final ) %>%
  reframe(n=n(),
          sum(eq_av), sum(exe_total), sum(taxed_eav), billed = sum(tax_bill_total),
          .by = year)

# More Exemptions than EAV -----------------------------------------

# These properties do not have tax bills due to having exemptions that remove all taxable value.
# Over 9600 PINs had enough in exemptions to cover all of their EAV

pin_data |> filter(exe_total >= av_clerk*eq_factor_final ) %>%
  mutate(skin_in_game = 0.1*av_clerk*eq_factor_final*tax_code_rate) |>
  reframe(n=n(),
          rev_change = sum(skin_in_game),
          .by = year)



pin_data |> filter(av_clerk == 0 ) %>%
  reframe(n=n(), .by = year)

pin_data |> filter(av_clerk  < 150 ) %>%
  reframe(n=n(), .by = year)
