library(tidyverse)
library(ptaxsim)
library(DBI) #Guess we don't need the "here" package anymore.
library(glue)

# Instantiate and Test DB connection.

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")



## Pulls ALL distinct PINs that existed, ever.
## Takes a while to run.
## 31+ million obs. (PIN-YEAR combos)

cook_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT *
  FROM pin
  ",
  .con = ptaxsim_db_conn
  ))

ever_incent <- cook_pins %>%
  filter(between(class, ))
)

ever_incent_pins <- as.list(ever_incent)

# MVH NOTE: OLD CODE ONLY PULLED INCENTIVE PINs FOR YEARS WHERE THEY HAD
# INCENTIVE CLASSIFICATIONS REVISED CODE PULLS ALL PINs THAT EVER HAD AN
# INCENTIVE CLASSIFICATION AT ANY TIME.

# keep only incentive properties : 600-899 class properties
#cook_pins <- cook_pins %>% filter(class > 599 & class < 900)
# 45,724 obs. (PIN-YEAR combos)

write_csv(cook_pins, "./Output/incentivePINs_allyears.csv")
