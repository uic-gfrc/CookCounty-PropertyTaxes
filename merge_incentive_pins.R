library(tidyverse)
library(ptaxsim)
library(DBI)
library(httr)
library(jsonlite)
library(glue)
# library(sf)
library(readxl)

options(scipen = 999)

# 3652 in 2021
incentive_pins <- read_csv("./Output/7_output_incentive_classes.csv") %>%
  mutate(#pin = as.character(pin),
         pin = str_pad(pin, 13, "left"))



access_db <- read_excel("incentivePINs_accessDB.xlsx") # %>% arrange((Status_cleaned) ) 


# access_db <- access_db %>%  
#   mutate(keypin = ifelse(`Concat PIN` %in% keypins, 1, 0)) %>%
#   select(keypin, `Concat PIN`, everything())

first_pins <- access_db %>% filter(Status_cleaned == "Approved") %>%
  group_by(CONTROL) %>%
  mutate(keypin = first(`Concat PIN`),
  ) %>% mutate(pin = `Concat PIN`) %>% 
  select(pin, keypin, CONTROL)

incentive_pins <- left_join(incentive_pins, first_pins, by = c("pin" = "pin") )

incentive_pins %>% group_by(CONTROL) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,284 incentive projects
incentive_pins %>% group_by(keypin) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,253 incentive projects based on keypins.


commerc_prop_keys <- read_csv("./Necessary_Files/Assessor_-_Commercial_Valuation_Data_20240212.csv") %>%
  filter(year == 2021 #& !is.na(pins)
         ) %>% 
  select(clean_pin, clean_keypin, keypin, pins, `class(es)`) %>%
  mutate(
   # clean_pin = as.character(clean_pin),
   # clean_pin = str_pad(clean_pin, 13, "left"),
   # keypin = as.character(keypin),
   keypin = gsub("-","", keypin),
   keypin = gsub(" ","", keypin),
    keypin = str_pad(keypin, 13, "left"),
   clean_keypin = gsub("-","", clean_keypin),
   
    clean_keypin = gsub(" ","", clean_keypin),
    classes = as.character(`class(es)`)
    #kingpin = str_pad(kingpin, 13, "left"),
  ) %>% 
  arrange(desc(clean_keypin))

keypins <- unique(commerc_prop_keys$clean_keypin)
#cleanpins <- unique(commerc_prop_keys$clean_pin)
#joined_dbs <- inner_join(commerc_prop_keys, incentive_pins, by = c("kingpin" = "pin") )

incentive_pins <- incentive_pins %>% 
  mutate(keypin_com_props = ifelse(pin %in% keypins, 1, 0),
         #kingpin = ifelse(pin %in% cleanpins, 1, 0),
         keypin_com_props2 = ifelse(pin ==keypin, 1, 0)) %>%
  select(year,CONTROL, keypin, keypin_com_props, keypin_com_props2, pin, everything())

table(incentive_pins$keypin_com_props)
table(incentive_pins$keypin_com_props2)




#joined_dbs <- left_join(incentive_pins, commerc_prop_keys, by = c("pin" = "clean_keypin") ) %>%
 # select(year, clean_keypin, pin, everything())


 

leftjoin_pins <- left_join(incentive_pins, access_db, by= c("pin" = "Concat PIN")) %>%
  select(pin, Status, `Start Year`, class, Notes )


# 4146 unique pins. Left join adds duplicate entries from Access files 
# 
# two different status entries creates additional rows when joined
incentive_pins %>%
  distinct(pin)

# 3984  
innerjoin_pins <- inner_join(incentive_pins, access_db, by= c("pin" = "Concat PIN")) %>%
  select(pin, Status, `Start Year`, class, Notes )

innerjoin_pins 

nojoin_pins <- anti_join(incentive_pins, access_db, by= c("pin" = "Concat PIN"))
# 162 PINs not in Access incentive PINs from 2021 are not found in the Access

