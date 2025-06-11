### Helper File: Pull all PINs EVER Comm. and/or Ind. in Cook County ###
### Years 2006 - 2023 ###



library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)


# Pull and Prep Data ------------------------------------------------------

is.integer64 <- function(x){
  class(x)=="integer64"
}

# Instantiate DB connection.

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")

# Read in class dictionary
cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  mutate(class = as.character(class_code)) |>  # rename to match other data frames
  select(-c(loa_2022, Option2, class_desc, land, vacant_ind, last2dig,
            Res_nonRes, assessment_level, used_in2021, class_code)) %>%
  mutate_at(.vars = c("improvement_ind", "incent_prop", "class_1dig", "major_class_code"), .funs = as.character
  )

# Bring in the Level of Assessments for each year. They have changed over time!!
ccao_loa <- read_csv("./inputs/ccao_loa.csv") %>%
  mutate(class = as.character(class_code)) %>%
  filter(year > 2005) %>%
  select(-class_code) %>%
  mutate(loa = as.numeric(loa)) %>%
  mutate(loa = ifelse(loa == 0, NA, loa) # avoid dividing by zero errors
         )

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

## PINS in UNINCORPORATED AREAS ARE NOT included in this data pull!!!

## Pulls ALL distinct PINs that existed between 2006 and 2022 in munis
## Syntax: "*" means "all the things" "pin" references the table w/in PTAXSIM DB
## 1,661,125 PINs when only including classes 400 to 899

tax_codes_muni <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT year, agency_num, tax_code_num, tax_code_rate
  FROM tax_code
  WHERE agency_num IN ({muni_agency_names$agency_num*})
  AND year <= 2023
  ",
           .con = ptaxsim_db_conn
  ))


# 1,643,662 PINs in municipalities
muni_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT year, pin, class, tax_code_num
  FROM pin
  WHERE class > 399 AND class < 900
  AND tax_code_num IN ({tax_codes_muni$tax_code_num*})
  AND year <= 2023

  ",
    .con = ptaxsim_db_conn
  ))

options(scipen = 999)

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")  %>%
  select(agency_number, clean_name, Triad, Township) %>%
  mutate(agency_number = str_pad(agency_number, width = 9, side = "left", pad = "0")) %>%

  mutate(agency_number = as.character(agency_number))


tax_codes_muni <- tax_codes_muni %>%
  left_join(nicknames, by = c("agency_num" = "agency_number"))


# 119,030 distinct PINs in  Municipalities were comm or ind. property for at least 1 year
distinct_pins <- muni_pins |>
  select(pin) |>
  distinct(pin)
# 120179 distinct PINs after 2023 update

## Use unique PIN list to get all obs. for all years they existed.
## ~1.8mil PINs
## Note: Alea_cat is a variable created by Alea in the class_dict file that indicates land use type.
## Land use type is based off of the description provided for property classes.

## 1,818,155 PINs within municipalities. Excludes unincorporated PINs

## Excluding the Exemption variables except for abatements.
comm_ind_pins_ever <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT DISTINCT year, pin, class, tax_code_num, tax_bill_total, av_mailed, av_certified, av_board, av_clerk, exe_abate
   FROM pin
   WHERE pin IN ({distinct_pins$pin*})
  ",
  .con = ptaxsim_db_conn
  )) |>
  mutate_if(is.integer64, as.double ) %>%
  mutate(class = as.character(class)) |>
  left_join(cde, by = "class") |>
  left_join(ccao_loa, by = c("year", "class")) |>
  mutate(comparable_props = as.character(comparable_props),
         )


## List of tax codes that are in TIFs and the proportion of value in the tax_code that goes to the TIF
tif_distrib <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT DISTINCT year, tax_code_num, tax_code_distribution_pct
  FROM tif_distribution
  ",
    .con = ptaxsim_db_conn)
  )


## Add additional variables ---------------------------------------------


# "Frankfort", "Homer Glen",  "Oak Brook", "East Dundee", "University Park",
# "Bensenville", "Hinsdale", "Roselle", "Deer Park", "Deerfield"
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


comm_ind_pins_ever <- comm_ind_pins_ever %>%
  rename(land_use = Alea_cat) |>
  arrange(pin) %>%
  left_join(tax_codes_muni, by = c("year", "tax_code_num")) %>%  # has muni clean_name  in it
  left_join(tif_distrib) %>%
  mutate(
    has_AB_exemp = as.character(ifelse(exe_abate > 0, 1, 0)),
    fmv = av_clerk / loa,
    fmv_NA_flag = ifelse(is.na(fmv),1,0),
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



