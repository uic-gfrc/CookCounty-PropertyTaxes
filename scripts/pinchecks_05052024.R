
library(tidyverse)
library(ptaxsim)
library(data.table)
library(glue)


ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")
years <- c(2022, 2021, 2020, 2008, 2007, 2006)
years <- c(2022: 2006)

# Wholefoods example, North Chicago:
pins <- c("17052120010000", "17052120020000", "17052120030000", "17052120040000",
          "17052120050000", "17052120060000", "17052120070000", "17052120080000",
          "17052120090000", "17052120100000")

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


## nonfireproof hotel example:
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


## Walmart in Niles example:
DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT*
  FROM pin
  WHERE pin = 10294030240000 AND
  year IN ({years*})
  ",
  .con = ptaxsim_db_conn
  ))


DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT*
  FROM pin
  WHERE pin = 29111330230000
 AND
  year IN ({years*})
  ",
  .con = ptaxsim_db_conn
  ))



DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT*
  FROM pin
  WHERE pin = 28012040220000
 AND
  year IN ({years*})
  ",
 .con = ptaxsim_db_conn
  ))

## for the shed example - class 593, industrial no incentives
pins <- c(33201040020000, 33192050300000, 33192050360000)
DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT*
  FROM pin
  WHERE pin IN ({pins*})
  AND year IN ({years*})
  ",
  .con = ptaxsim_db_conn
  ))


# all pins for 2022
ptax_pins <- read_csv("Output/Dont_Upload/0_joined_PIN_data_2022.csv") %>% 
  mutate(class = as.numeric(class)) %>%
  # keep 500-899 class PINs
  filter(class %in% c(550,580, 581, 583, 587, 589, 593)) %>%
  select(-c(propclass_1dig:av.y))


# Abatement PIN checks ---------------------

# for PIN 24264070100000. Part of Truck Stop Pilot W

ptaxsim::lookup_pin(years, "24264070100000")

pins <- "24264070100000"
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

eq22 <- 2.9237

pinchecks %>% filter(year == 2022) %>% 
  summarize(av = sum(av_clerk)) %>%
  mutate(eav = av*eq22)
pinbills <- ptaxsim::tax_bill(2022, "24264070100000",
                              simplify = FALSE)


pinbills %>% filter(year == 2022) %>% 
  summarize(prebill = sum(tax_amt_pre_exe),
            postbill = sum(tax_amt_post_exe)) %>% 
  mutate(dif = prebill-postbill)

abatement <- 1476960






ptaxsim::lookup_pin(years, "18232000010000")

pins <- "18232000010000"

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

pinchecks %>% filter(year == 2022) %>% 
  summarize(av = sum(av_clerk)) %>%
  mutate(eav = av*eq22)

pinbills <- ptaxsim::tax_bill(2022, "18232000010000",
                              simplify = FALSE)

# Project PIN Bills ---------------------------------------------

pins <- c("32174220040000", "32174220050000", "32174220060000",
          "32174220070000")

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

#pinbills <- ptaxsim::tax_bill(2022, pins, simplify = FALSE)




# PINs have prorated amounts of 0.95 and 0.05
pins <- c("32174240070000", "32174240080000")
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
