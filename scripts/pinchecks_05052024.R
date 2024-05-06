
library(tidyverse)
library(ptaxsim)
library(data.table)


ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

# Wholefoods example:
pins <- c("17052120010000", "17052120020000", "17052120030000", "17052120040000",
          "17052120050000", "17052120060000", "17052120070000", "17052120080000",
          "17052120090000", "17052120100000")
years <- c(2022, 2021, 2020)

pinchecks <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT*
  FROM pin
  WHERE pin IN ({pins*}) AND
  year IN ({years*})
  ",
  .con = ptaxsim_db_conn
  ))

pinchecks %>% filter(year == 2021) %>% summarize(av = sum(av_clerk))


DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT*
  FROM pin
  WHERE pin = 17032020530000 AND
  year IN ({years*})
  ",
  .con = ptaxsim_db_conn
  ))


