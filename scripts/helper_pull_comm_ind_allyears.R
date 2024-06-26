library(tidyverse)
library(ptaxsim)
library(DBI) #Guess we don't need the "here" package anymore?
library(glue)

# Instantiate DB connection.

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

# Read in class dictionary

cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  rename(class = class_code) # rename to match other data frames

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
  ))


## Write CSV to Output Folder

write_csv(comm_ind_pins_ever, "./Output/comm_ind_PINs_2006-2022.csv")

