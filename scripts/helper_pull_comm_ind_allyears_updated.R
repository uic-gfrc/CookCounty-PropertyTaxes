### Helper File: Pull all PINs EVER Comm. and/or Ind. in Cook County ###
### Years 2006 - 2024 ###
### Generates "./Output/comm_ind_PINs_ever_2006to2024.csv" ###

### PINS in UNINCORPORATED AREAS are NOT included in this data pull!!! ###

# Setup -----------------------------------------------------------------

options(scipen = 999) # scientific notation sucks

# function to mutate classes to integer64 type

is.integer64 <- function(x) {
  class(x) == "integer64"
}

## Load Packages ---------------------------------------------------------

library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)

## Import supporting files --------------------------------------

# Class dictionary

cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  mutate(class = as.character(class_code)) |>  # rename to match other data frames
  select(-c(loa_2022, Option2, class_desc, land, vacant_ind, last2dig,
    Res_nonRes, assessment_level, used_in2021, class_code)) |>
  mutate_at(.vars = c("improvement_ind", "incent_prop", "class_1dig", "major_class_code"), .funs = as.character
  )

# Levels of assessment by year (they change over time)

ccao_loa <- read_csv("./inputs/ccao_loa.csv") |>
  mutate(class = as.character(class_code)) |>
  filter(year > 2005) |>
  select(-class_code) |>
  mutate(loa = as.numeric(loa)) |>
  mutate(loa = ifelse(loa == 0, NA, loa) # avoid dividing by zero errors
  )

# "Clean" muni names

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")  |>
  select(agency_number, clean_name, Triad, Township) |>
  mutate(agency_number = str_pad(agency_number, width = 9, side = "left", pad = "0")) |>
  mutate(agency_number = as.character(agency_number))

## Create class and border crosser variables. ---------------------

# The following munis cross county lines and are consistently dropped across the PTAX project

# "Frankfort", "Homer Glen",  "Oak Brook", "East Dundee", "University Park",
# "Bensenville", "Hinsdale", "Roselle", "Deer Park", "Deerfield"

cross_county_lines <- c("030440000", "030585000", "030890000", "030320000", "031280000",
  "030080000", "030560000", "031120000", "030280000", "030340000",
  "030150000", "030050000", "030180000", "030500000", "031210000")


# These had to be manually broken up because major classes 4, 5, and 8 include
# commercial and industrial properties

# Note: major class 400 is for nonprofits

commercial_classes <- c(401:435, 490, 491, 492, 496:499,
  500:535, 590, 591, 592, 597:599,
  700:799,
  800:835, 891, 892, 897, 899
)

industrial_classes <- c(480:489, 493,
  550:589, 593,
  600:699,
  850:890, 893
)

## Instantiate DB connection ---------------------------------------------

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2024.0.0.db")

# Query ptaxsim.db and merge data -----------------------------------------

# SQL syntax:
# "*" means "all the things"
# "from" indicates relevant table
# "where" starts a filter

## agency_info table for names -------------------------

# note: Cicero is not listed in ptaxsim.db as a muni and is thus pulled via its agency_num: 020060000

# 134 munis

muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'
  "
)

## relevant tax codes -------------------------------

# 56187 tax codes

tax_codes_muni <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT year, agency_num, tax_code_num, tax_code_rate
  FROM tax_code
  WHERE agency_num IN ({muni_agency_names$agency_num*})
  AND year <= 2024
  ",
    .con = ptaxsim_db_conn
))

# associate clean muni names w/ tax codes

tax_codes_muni <- tax_codes_muni |>
  left_join(nicknames, by = c("agency_num" = "agency_number"))

## Identify all comm/ind PINs -------------------

# 1,739,306 obs.

muni_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT year, pin, class, tax_code_num
  FROM pin
  WHERE class > 399 AND class < 900
  AND tax_code_num IN ({tax_codes_muni$tax_code_num*})
  AND year <= 2024
  ",
    .con = ptaxsim_db_conn
))

# need all distinct PINs to pull them in for years they WERE NOT comm/ind

# 120,179 distinct PINs

distinct_pins <- muni_pins |>
  select(pin) |>
  distinct(pin)

## Final query and merge in existing extra data --------------------------

# NOW we can pull all comm/ind PINs ever comm/ind 2006 through 2024

# We pull exe_abate because it isn't a residential exemption.


# 1,938,402 obs.

comm_ind_pins_ever <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT year, pin, class, tax_code_num, tax_bill_total, av_mailed, av_certified, av_board, av_clerk, exe_abate
   FROM pin
   WHERE pin IN ({distinct_pins$pin*})
   AND year >= 2006
   AND year <= 2024
  ",
    .con = ptaxsim_db_conn
)) |>

  # fix variable types
  mutate_if(is.integer64, as.double) |>
  mutate(class = as.character(class)) |>

  # join in variables from other sheets
  left_join(cde, by = "class") |>
  left_join(ccao_loa, by = c("year", "class")) |>
  mutate(comparable_props = as.character(comparable_props),
  )

## TIF increments -------------------------------------------------

# 2024 changed the relevant TIF table from tax-code-level information to
# PIN-level information. Build standardized TIF objects first, then join them
# with different keys:
#   - pre-2024: year + tax_code_num
#   - 2024+:    year + pin + tax_code_num

years <- 2006:2024

# Old method: tax-code-level TIF distribution, through 2023.
tif_info_old <- purrr::map_dfr(years[years < 2024], function(i) {
  DBI::dbGetQuery(
    ptaxsim_db_conn,
    paste("SELECT * FROM tif_distribution WHERE year = ", i, ";")
  ) |>
    mutate_if(is.integer64, as.double) |>
    transmute(
      year,
      tax_code_num,
      # tif_eav = tax_code_eav,
      # tif_frozen_eav = tax_code_frozen_eav,
      # tif_revenue = tax_code_revenue,
      # tif_increment_eav = tax_code_eav - tax_code_frozen_eav,
      tif_distribution_pct = tax_code_distribution_pct / 100,

      # These fields only exist in the 2024+ PIN-level table.
      transit_tif_to_cps = NA_real_,
      transit_tif_to_tif = NA_real_,
      transit_tif_to_dist = NA_real_,
      is_transit_tif = NA
    )
})

# New method: PIN-level TIF distribution, starting in 2024.
tif_info_new <- purrr::map_dfr(years[years >= 2024], function(i) {
  DBI::dbGetQuery(
    ptaxsim_db_conn,
    paste("SELECT * FROM pin_tif_distribution WHERE year = ", i, ";")
  ) |>
    mutate_if(is.integer64, as.double) |>
    transmute(
      year,
      pin,
      tax_code_num,
      tif_eav = pin_eav,
      tif_frozen_eav = pin_frozen_eav,
      tif_revenue = pin_revenue,
      tif_increment_eav = pin_increment_eav,
      tif_distribution_pct = pin_distribution_pct / 100,
      transit_tif_to_cps,
      transit_tif_to_tif,
      transit_tif_to_dist,
      is_transit_tif
    )
})

in_tif <- as.character(ifelse(tax_code_num %in% tif_info$tax_code_num, 1, 0))

# Mutate new variables ---------------------------------------------

comm_ind_pins_ever <- comm_ind_pins_ever |>

  # Alea_cat--manually coded based on assessor categories

  rename(land_use = Alea_cat) |>
  arrange(pin) |>

  # Has muni clean_name in it.
  left_join(tax_codes_muni, by = c("year", "tax_code_num")) |>

  # Pre-2024 TIF information is at the tax-code level.
  left_join(tif_info_old, by = c("year", "tax_code_num")) |>

  # 2024+ TIF information is at the PIN-tax-code level.
  left_join(
    tif_info_new,
    by = c("year", "pin", "tax_code_num"),
    suffix = c("_old", "_new")
  ) |>

  mutate(
    # Collapse old and new TIF variables into one standardized set.
    # tif_eav = coalesce(tif_eav_new, tif_eav_old),
    # tif_frozen_eav = coalesce(tif_frozen_eav_new, tif_frozen_eav_old),
    # tif_revenue = coalesce(tif_revenue_new, tif_revenue_old),
    # tif_increment_eav = coalesce(tif_increment_eav_new, tif_increment_eav_old),
    tif_distribution_pct = coalesce(tif_distribution_pct_new, tif_distribution_pct_old),
    # transit_tif_to_cps = coalesce(transit_tif_to_cps_new, transit_tif_to_cps_old),
    # transit_tif_to_tif = coalesce(transit_tif_to_tif_new, transit_tif_to_tif_old),
    # transit_tif_to_dist = coalesce(transit_tif_to_dist_new, transit_tif_to_dist_old),
    is_transit_tif = coalesce(is_transit_tif_new, is_transit_tif_old),

    # Keep non-TIF rows from becoming NA in later summaries.
    tif_eav = replace_na(tif_eav, 0),
    tif_frozen_eav = replace_na(tif_frozen_eav, 0),
    tif_revenue = replace_na(tif_revenue, 0),
    tif_increment_eav = replace_na(tif_increment_eav, 0),
    tif_increment_eav = ifelse(tif_increment_eav < 0, 0, tif_increment_eav),
    tif_distribution_pct = replace_na(tif_distribution_pct, 0),

    # transit_tif_to_cps = replace_na(transit_tif_to_cps, 0),
    # transit_tif_to_tif = replace_na(transit_tif_to_tif, 0),
    # transit_tif_to_dist = replace_na(transit_tif_to_dist, 0),
    is_transit_tif = replace_na(is_transit_tif, 0),

    has_AB_exemp = as.character(ifelse(exe_abate > 0, 1, 0)),
    fmv = av_clerk / loa,
    fmv_NA_flag = ifelse(is.na(fmv), 1, 0),
    fmv = ifelse(is.na(fmv), 0, fmv),

    # Use the standardized TIF variable instead of checking one raw table.
    in_tif = as.character(ifelse(tif_distribution_pct > 0, 1, 0)),

    class_group = str_sub(class, 1, 1),
    class_group = case_when( # well this is quite the thirsty case_when, isn't it?
      class_group == 5 & class %in% commercial_classes ~ "5A",
      class_group == 5 & class %in% industrial_classes ~ "5B",
      class_group == 7 & class < 742 ~ "7A",    # commercial developments less than $2 million
      class_group == 7 & class >= 742 ~ "7B",   # commercial developments greater than $2 million
      class_group == 8 & class %in% commercial_classes ~ "8A",
      class_group == 8 & class %in% industrial_classes ~ "8B",
      TRUE ~ as.character(class_group)
    )
  ) |>

  # Remove temporary suffix columns created by joining old and new TIF tables.
  select(-ends_with("_old"), -ends_with("_new"))

# Done with database pulls.
dbDisconnect(ptaxsim_db_conn)

# Write csv --------------------------------------

write_csv(comm_ind_pins_ever, "./Output/comm_ind_PINs_ever_2006to2024.csv")
