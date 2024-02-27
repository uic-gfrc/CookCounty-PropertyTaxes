library(tidyverse)
library(ptaxsim)
library(DBI) #Guess we don't need the "here" package anymore?
library(glue)

# Instantiate DB connection.

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

## Pulls ALL distinct PINs that existed, ever.
## Takes a while to run. (~1 min w/ 64GB RAM)
## 31+ million obs. (PIN-YEAR combos)

cook_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT *
  FROM pin
  ",
  .con = ptaxsim_db_conn
  ))


## Limit to just incentive major classes (6, 7, 8)
## ~46k obs (PIN-YEAR combos)


cook_pins <- cook_pins %>% 
  filter(class > 599 & class < 900)

# get distinct pins
distinct_pins <- cook_pins %>% 
  select(pin) %>%     
  distinct(pin)   # 5,869 distinct pins

# 45,724 obs. (PIN-YEAR combos)

## But we want those PINs as obs. for all years, even if they weren't classified
## as an incentive in that time.

## Use incentive PIN list to get all obs.

incentive_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT DISTINCT *
   FROM pin
   WHERE pin IN ({distinct_pins$pin*})
  ",
  .con = ptaxsim_db_conn
  ))


## Write CSV to Output Folder

write_csv(incentive_pins, "./Output/incentivePINs_allyears.csv")

