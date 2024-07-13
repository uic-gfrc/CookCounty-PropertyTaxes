### Generate balanced panel for commercial and industrial properties. ###
### Tax Years 2011 - 2022 ###
### Last updated: 7/13/2024 ###

# Load packages

library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)

## Function to identify variables in integer64 format...and change them.

is.integer64 <- function(x){
  class(x)=="integer64"
}

cross_county_lines <- c("030440000", "030585000", "030890000", "030320000", "031280000",
                        "030080000", "030560000", "031120000", "030280000", "030340000",
                        "030150000","030050000", "030180000","030500000", "031210000")


commercial_classes <- c(401:435, 490, 491, 492, 496:499,
                        500:535,590, 591, 592, 597:599,
                        700:799,
                        800:835, 891, 892, 897, 899
)

industrial_classes <- c(480:489,493,
                        550:589, 593,
                        600:699,
                        850:890, 893
)

## Instantiate DB connection.

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

## Read in supplemental files

### Class dictionary (levels of assessments; code definitions; land use definitions, etc.)
cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  mutate(class = as.character(class_code)) |>  # rename to match other data frames
  select(-c(loa_2022, Option2, class_desc, land, vacant_ind, last2dig,
            Res_nonRes, assessment_level, used_in2021, class_code)) %>%
  mutate_at(.vars = c("improvement_ind", "incent_prop", "class_1dig", "major_class_code"), .funs = as.character
  )

ccao_loa <- read_csv("./inputs/ccao_loa.csv") %>%
  mutate(class = as.character(class_code)) %>%
  filter(year > 2005) %>%
  select(-class_code) %>%
  mutate(loa = as.numeric(loa)) %>%
  mutate(loa = ifelse(loa == 0, NA, loa) # avoid dividing by zero error
  )

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")  %>%
  select(agency_number, clean_name, Triad, Township) %>%
  mutate(agency_number = as.character(agency_number)) %>%
  mutate(agency_number = str_pad(agency_number, width = 9, side = "left", pad = "0"))

# Cook County agency number is 010010000

## Pull Muni Taxing Agency Names from agency_info table
muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'
    "
)

tax_codes_muni <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT year, agency_num, tax_code_num, tax_code_rate
  FROM tax_code
  WHERE agency_num IN ({muni_agency_names$agency_num*})
  AND YEAR >= 2011 AND YEAR <= 2022
  ",
           .con = ptaxsim_db_conn
  ))

muni_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT year, pin, class, tax_code_num
  FROM pin
  WHERE class > 399 AND class < 900
  AND tax_code_num IN ({tax_codes_muni$tax_code_num*})
  ",
    .con = ptaxsim_db_conn
  ))

tax_codes_muni <- tax_codes_muni %>%
  left_join(nicknames, by = c("agency_num" = "agency_number"))

distinct_pins <- muni_pins |>
  select(pin) |>
  distinct(pin)

comm_ind_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT year, pin, class, tax_code_num, tax_bill_total, av_mailed, av_certified, av_board, av_clerk, exe_abate
   FROM pin
   WHERE pin IN ({distinct_pins$pin*})
   AND YEAR >= 2011 AND YEAR <= 2022
  ",
    .con = ptaxsim_db_conn
  )) |>
  mutate_if(is.integer64, as.double ) %>%
  mutate(class = as.character(class)) |>
  left_join(cde, by = "class") |>
  left_join(ccao_loa, by = c("year", "class")) |>
  mutate(comparable_props = as.character(comparable_props),
  )

tif_distrib <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT year, tax_code_num, tax_code_distribution_pct
  FROM tif_distribution
  WHERE YEAR >= 2011 AND YEAR <= 2022
  ",
    .con = ptaxsim_db_conn)
)

comm_ind_pins_modified <- comm_ind_pins %>%
  rename(land_use = Alea_cat) |>
  arrange(pin) %>%
  left_join(tax_codes_muni, by = c("year", "tax_code_num")) %>%
  left_join(tif_distrib) %>%
  mutate(
    has_AB_exemp = as.character(ifelse(exe_abate > 0, 1, 0)),
    fmv = av_clerk / loa,
    fmv_NA_indicator = ifelse(is.na(fmv),1,0),
    fmv = ifelse(is.na(fmv), 0, fmv),
    in_tif = as.character(ifelse(tax_code_num %in% tif_distrib$tax_code_num, 1, 0)),
    class_group = str_sub(class, 1,1),
    class_group = case_when(
      (class_group == 5 & class %in% commercial_classes) ~ "5A",
      (class_group == 5 & class %in% industrial_classes) ~ "5B",
      class_group == 7 &  class < 742 ~ "7A",    # commercial developments less than $2 million
      class_group == 7 &  class >= 742 ~ "7B",   # commercial developments greater than $2 million
      (class_group == 8 & class %in% commercial_classes ) ~ "8A",
      (class_group == 8 & class %in% industrial_classes ) ~ "8B",
      TRUE ~ as.character(class_group))
  )

comm_ind_pins_modified_2 <- comm_ind_pins_modified  %>%
  group_by(pin) |>
  mutate(
    years_existed = n(),
    incentive_years = sum(incent_prop == "Incentive"),
    landuse_change =
      ifelse(
        sum(land_use == "Commercial") == 12, "Always Commercial",
        ifelse(sum(land_use == "Industrial") == 12, "Always Industrial",
               # some properties had an incentive class before 2011 and then were tax exempt. Dropped from panel.
               ifelse(sum(land_use == "Exempt") == 12, "Drop Me",
                      "Changes Land Use" ))),
    incent_change = case_when(
      incentive_years == 12 ~ "Always Incentive",
      incentive_years == 0 ~ "Never Incentive",
      TRUE ~ "Changes Sometime")
  ) |>
  ungroup()

comm_ind_2011to2022 <- comm_ind_pins_modified_2

# 1,284,971 obs before dropping PINs

## Examine PINs dropped from 2011-2022 panel data --------------------------


# only keep PINs that existed all years and if they are from a municipality
# assumes that clean_name and agency_number correctly merged to tax codes from municipalities and their agency number
# 97,521 PINs will be dropped from the 2011-2022 panel data
dropped_pins2 <-  comm_ind_2011to2022 %>%
  filter(
    years_existed < 12  |      ##  69,491 PINs do not exist all 12 years
      is.na(clean_name)  |       ## 4,370 PINs do not have municipality names
      agency_num %in% cross_county_lines |  ## 13,915 PINs located in Municipalities that have a majority of their EAV in other counties
      landuse_change == "Drop Me"     ## 12,048 PINs were tax exempt for all 12 years
  )


### Drop PINs and Export File -----------------------------------------------

comm_ind_2011to2022 <- comm_ind_2011to2022 |>
  filter(
    years_existed == 12 &
      !is.na(clean_name) &
      !agency_num %in% cross_county_lines &
      landuse_change != "Drop Me"
  )
## 1,187,450 obs remain

comm_ind_2011to2022 <- comm_ind_2011to2022 |>
  mutate(exempt_indicator = ifelse(land_use == "Exempt", 1, 0)) |>
  group_by(pin) |>
  mutate(
    base_year_fmv_2011 = ifelse( sum(exempt_indicator) > 0, NA, fmv[year == 2011]),
    fmv_growth_2011 = fmv/base_year_fmv_2011) |>
  ungroup()

## 100,900 PINs existed since 2011 (and did not become tax exempt)
## 102,717 PINs if not filtering for NA fmv growth
## 99,088 PINs as of July 11 - AWM (1.12 obs. - MVH)
## BUT some are exempt and will have 0 or errors for the fmv growth!!



comm_ind_2011to2022 %>%
  select(year, land_use, incent_prop, fmv_growth_2011) %>%
  filter(year == 2022)


## Write CSV to Output Folder
write_csv(comm_ind_2011to2022, "./Output/comm_ind_2011-2022_balanced.csv")
