library(tidyverse)
library(ptaxsim)
library(DBI) #Guess we don't need the "here" package anymore?
library(glue)

# Instantiate DB connection.

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

# Read in class dictionary

cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  rename(class = class_code) # rename to match other data frames

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")

## Pulls ALL distinct PINs that existed between 2006 and 2022.
## Syntax: "*" means "all the things" "pin" references the table w/in PTAXSIM DB
## Takes a while to run. (~1 min w/ 64GB RAM or 28GB M2 Chip)
## ~31.47 million obs. (PIN-YEAR combos)
## Change to numeric to merge w/ CDE

cook_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT *
  FROM pin
  ",
  .con = ptaxsim_db_conn
  )) |>
  mutate(class = as.numeric(class))

## Filter to just industrial/commercial properties INCLUDING relevant Maj. Class 4
## ~1.6mil obs
## MVH NOTE: WE SHOULD RENAME ALEA_CAT TO SOMETHING MORE, ERRR, OBJECTIVE SOUNDING.

comm_ind_pins <- cook_pins |>
  left_join(cde, by = "class") |>
  filter(Alea_cat %in% c("Commercial", "Industrial"))

# 119k distinct pins

distinct_pins <- comm_ind_pins |>
  select(pin) |>
  distinct(pin)

# 3124 distinct tax codes

distinct_tc <- comm_ind_pins |>
  select(tax_code_num) |>
  distinct()

## But we want those PINs as obs. for all years, even if they weren't classified
## as an incentive in that time.

## Use incentive PIN list to get all obs.
## ~1.8mil PINs

comm_ind_pins_ever <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT DISTINCT *
   FROM pin
   WHERE pin IN ({distinct_pins$pin*})
  ",
  .con = ptaxsim_db_conn
  )) |>
  mutate(class = as.numeric(class)) |>
  left_join(cde, by = "class") |> #Whoops! It got lost! Need to merge it back in!
  mutate(comparable_props = as.character(comparable_props))

## Identify all tax codes found w/ all prior PINs

comm_ind_tc_ever <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT *
   FROM tax_code
   WHERE tax_code_num IN ({distinct_tc$tax_code_num*})
  ",
    .con = ptaxsim_db_conn
  ))

## Muni Names

muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'
    "
)

## Merge PIN data with muni names

comm_ind_tc_names <- comm_ind_tc_ever |>
  filter(agency_num %in% muni_agency_names$agency_num) |>
  rename(agency_number = agency_num) |>
  mutate(agency_number = as.numeric(agency_number)) |>
  left_join(nicknames)

comm_ind_pins_ever <- comm_ind_pins_ever |>
  left_join(comm_ind_tc_temp, by = c("year", "tax_code_num"))

comm_ind_pins_ever <- comm_ind_pins_ever |>
  as.data.frame()

## Write CSV to Output Folder

write_csv(comm_ind_pins_ever, "./Output/comm_ind_PINs_2006-2022.csv")

