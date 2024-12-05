### Zero Bill Analysis ###
## MVH 11/24 ##

library(DBI)
library(RSQLite)
library(tidyverse)
library(ptaxsim)

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
ptaxsim_db_conn <- dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")


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
  mutate(exe_total = rowSums(across(starts_with("exe_")))) |>
  mutate(taxable_eav = eq_av - exe_total) |>
  mutate(flag_missingdata = ifelse(taxable_eav > 1000, 1, 0)) |>
  mutate(no_eav = ifelse(taxable_eav <= 0, 1, 0)) |>
  mutate(exemps_no_eav = ifelse(eq_av < exe_total, 1, 0)) |>
  select(-av_mailed, -av_certified, -av_board)

pin_data |> filter(taxable_eav > 1000) |> 
  group_by(year) |> 
  summarize( n = n(), 
             taxable_eav = sum(taxable_eav), 
             shifted_rev = sum(eq_av * tax_code_rate/100))



# ALL zero-dollar PINs in 2023 -------------------------------

## Total: 27053 <<<- this value

pin_data |>
  filter(year == 2023) |>
  filter(tax_bill_total == 0) |>
  summarize(n = n()) # 27053

## AV of 0: 74 <<<- this value

pin_data |>
  filter(year == 2023) |>
  filter(av_clerk == 0) |>
  summarize(n = n())

## AV > 0, Taxable EAV <= 0: 9622 <<-- this value

pin_data |>
  filter(year == 2023) |>
  filter(av_clerk > 0 & taxable_eav <= 0) |>
  summarize(n = n())

## Taxable EAV > 0 and Zero-Bill: 17357 <<-- this value

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav > 0 & tax_bill_total == 0) |>
  summarize(n = n())

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav > 0 & tax_bill_total == 0) |>
  summarize(sum_foregone_rev = sum(taxable_eav*tax_code_rate/100, na.rm = T))

## Taxable EAV between 0 and 150: 10,003

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav > 0 & taxable_eav < 150 & tax_bill_total <= 0) |>
  summarize(n = n())

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav > 0 & taxable_eav < 150 & tax_bill_total <= 0) |>
  mutate(foregone_rev = taxable_eav*tax_code_rate/100, na.rm = T) |>
  summarize(sum(foregone_rev, na.rm = T)) # $50mil

## Taxable EAV > $150: 7354

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav > 150 & tax_bill_total <= 0) |>
  summarize(n = n())

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav > 150 & tax_bill_total <= 0) |>
  mutate(foregone_rev = taxable_eav*tax_code_rate/100, na.rm = T) |>
  summarize(sum(foregone_rev, na.rm = T)) # $50mil

# Over 27,000 residential PINs did not have to pay a tax bill in 2023.
pin_data |>
  filter(tax_bill_total <= 0) |>
  group_by(year) |>
  summarize(n = n(),
            sum_eq_av = sum(av_clerk*eq_factor_final, na.rm=TRUE),
            total_exempt = sum(exe_total), taxable_eav = sum(av_clerk*eq_factor_final - exe_total, na.rm=T))


# Section 18-40 Zero Dollar Bills ----------------------------------

# These bills should have an EAV < $150 and > 0 after accounting for exemptions
# but they tend to have EAVs much greater than 150, even after exemptions are subtracted



## All with Taxable EAV between 0 and 150 --------------------


# Using maximum calculations
pin_data |>
  filter(taxable_eav > 0 & taxable_eav <= 150 & tax_bill_total == 0) |>
  group_by(year) |>
  summarize(n = n(),
            eq_av = sum(av_clerk*eq_factor_final, na.rm=TRUE),
            total_exempt = sum(exe_total), taxed_eav = sum(av_clerk*eq_factor_final - exe_total, na.rm=T),
            billed = sum(tax_bill_total),
            max_av = max((av_clerk*eq_factor_final- exe_total)/eq_factor_final),
            max_eav = max(av_clerk*eq_factor_final),
            max_taxable_eav = max(av_clerk*eq_factor_final- exe_total))


# Raw Values
pin_data |>
  filter(taxable_eav > 0 & taxable_eav <= 150 & tax_bill_total == 0) |>
  group_by(year) |>
  summarize(n = n(),
            sum_av = sum(av_clerk, na.rm = T),
            sum_eq_av = sum(eq_av, na.rm=TRUE),
            sum_eav_exempt = sum(exe_total, na.rm = T),
            sum_taxable_eav = sum(taxable_eav, na.rm=T),
            sum_billed = sum(tax_bill_total),
            sum_foregone = sum(taxable_eav*tax_code_rate/100, na.rm = T))


# double checking
pin_data |> filter(exe_total < av_clerk*eq_factor_final ) %>%
  reframe(n=n(),
          sum(eq_av), sum(exe_total), sum(taxable_eav), billed = sum(tax_bill_total),
          .by = year)

## Zero Bill with EAV > $150 --------------------------------

pin_data |>
  filter(taxable_eav > 150 & tax_bill_total == 0) |>
  group_by(year) |>
  summarize(n = n(),
            sum_av = sum(av_clerk, na.rm = T),
            sum_eq_av = sum(eq_av, na.rm = T),
            sum_exe_total = sum(exe_total, na.rm = T),
            sum_taxable_eav = sum(taxable_eav, na.rm = T),
            sum_foregone = sum(taxable_eav*tax_code_rate/100, na.rm = T))

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

## All Exempt EAV > eq_av Zero-Bills: 9696

pin_data |>
  filter(year == 2023) |>
  filter(exe_total >= eq_av) |>
  summarize(n = n())
