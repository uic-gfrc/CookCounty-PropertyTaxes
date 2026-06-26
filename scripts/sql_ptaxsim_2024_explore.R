# 2024 update exploration


library(tidyverse)
library(data.table)
library(ptaxsim)
library(glue)
library(DBI)

# ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")
# pins_2023 <- dbGetQuery(ptaxsim_db_conn, "SELECT * FROM pin WHERE year = 2023")

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2024.0.0.db")

agency_dt <- dbGetQuery(ptaxsim_db_conn, paste("SELECT * FROM agency WHERE year = ", i, ";"))

# For 2024 onward, TIF distributions are calculated at the PIN level and are
# stored in a different table
pin_tif_dists_24 <- DBI::dbGetQuery(
  ptaxsim_db_conn, "
  SELECT *
  FROM pin_tif_distribution
  WHERE year = 2024
  "
)

tif_taxcodes_pre24 <- DBI::dbGetQuery(
  ptaxsim_db_conn, "
  SELECT *
  FROM tif_distribution
  WHERE year = 2023
  "
)


# taxcode level totals
tif_distrib_b4_2024 <- DBI::dbGetQuery(ptaxsim_db_conn, paste("SELECT * FROM tif_distribution WHERE year = 2022"))


#-- See all agencies that have changed to funds
agency_crosswalk <- dbGetQuery(ptaxsim_db_conn,
  "SELECT * FROM agency_crosswalk")

#-- See the same change at the fund level
agency_fund_crosswalk <- dbGetQuery(ptaxsim_db_conn,
  "SELECT * FROM agency_fund_crosswalk")


agency_fund_info <- dbGetQuery(ptaxsim_db_conn,
  "SELECT * FROM agency_fund_info WHERE fund_num NOT LIKE '%000'"
)

pins_2024 <- dbGetQuery(ptaxsim_db_conn, "SELECT * FROM pin WHERE year = 2024")

# 144 in 2024
classes_2024 <- dbGetQuery(ptaxsim_db_conn, "SELECT DISTINCT class FROM pin WHERE year = 2024")

classes_all <- dbGetQuery(ptaxsim_db_conn, "SELECT DISTINCT class FROM pin")
# 171 in ever

classes_inptax
