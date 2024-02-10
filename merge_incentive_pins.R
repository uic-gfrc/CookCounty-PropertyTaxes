library(tidyverse)
library(ptaxsim)
library(DBI)
library(httr)
library(jsonlite)
library(glue)
library(sf)
library(readxl)

incentive_pins <- read_csv("./Output/7_output_incentive_classes.csv")

access_db <- read_excel("incentivePINs_accessDB.xlsx")  %>% 
  arrange((Status_cleaned) ) 



leftjoin_pins <- left_join(incentive_pins, access_db, by= c("pin" = "Concat PIN")) %>%
  select(pin, Status, `Start Year`, class, Notes )


# 3652 unique pins. Left join adds duplicate entries from Access files 
# two different status entries creates additional rows when joined
leftjoin_pins %>%
  distinct(pin) %>%
  count()

  
innerjoin_pins <- inner_join(incentive_pins, access_db, by= c("pin" = "Concat PIN")) %>%
  select(pin, Status, `Start Year`, class, Notes )

nojoin_pins <- anti_join(incentive_pins, access_db, by= c("pin" = "Concat PIN"))
# 457 incentive PINs from 2021 are not found in the Access files
## Need to add Class C and L PINs to Access file! 


nojoin_pins2 <- anti_join(access_db, incentive_pins, by= c("Concat PIN" = "pin"))

