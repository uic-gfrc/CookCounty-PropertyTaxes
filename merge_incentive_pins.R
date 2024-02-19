library(tidyverse)
library(ptaxsim)
library(DBI)
library(httr)
library(jsonlite)
library(glue)
library(readxl)

options(scipen = 999)


# PTAXSIM incentive properties --------------------------------------------
# Pulled from PTAXSIM - Al PINs from 2021, filtered by class_code between 600&900
# 3652 PINs in 2021
incentive_pins <- read_csv("./Output/7_output_incentive_classes.csv") %>%
    mutate(
      # pin = as.character(pin), 
      # pin = str_pad(pin, 13, "left"),
         parcel = str_sub(pin, 1, 10),
         block = str_sub(pin, 1, 7))  



# CMAP DATABASE -----------------------------------------------------------
# combined PIN tables into 1 file
# 10,275 obs from copying/pasting PIN tables into one table.
# 3677 unique "Controls" (synonimous with Key PIN I think)
# Includes all incentive PINs, even from old projects
# excel tab: 'incentive PINs (ever)' 
access_db <- read_excel("incentivePINs_accessDB_2.xlsx")


# create a keypin variable based on smallest PIN to compare to keypin used by assessor
first_pins <- access_db %>% 
  arrange(Status_cleaned,`Concat PIN`) %>%
  group_by(CONTROL) %>% 
  mutate( keypin = first(`Concat PIN`),
         pin = `Concat PIN`,
         n_PINs_inControl = n(),
         class = first(Class)
         ) %>% 
  select(pin, 
         keypin, CONTROL, n_PINs_inControl, class) %>% 
  arrange(CONTROL, keypin, pin,)


## Join PTAXSIM incentive properties to CMAP incentive data
# CMAP incentive PINs appear multiple times in database
incentive_pins <- left_join(incentive_pins, first_pins, by = c("pin" = "pin") )

incentive_pins %>% group_by(CONTROL) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,284 incentive projects

incentive_pins %>% group_by(keypin) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,253 incentive projects based on keypins.


commerc_prop_keys <- read_csv("./Necessary_Files/Assessor_-_Commercial_Valuation_Data_20240212.csv") 

commerc_prop_keys <- commerc_prop_keys%>%
 # filter(#year == 2021 #& !is.na(pins)
  #       ) %>% 
  select(clean_pin, clean_keypin, keypin, pins, `class(es)`) %>%
  mutate(
    first_digit = str_sub(`class(es)`, 1, 1),
   # clean_pin = as.character(clean_pin),
   # clean_pin = str_pad(clean_pin, 13, "left"),
   # keypin = as.character(keypin),
   keypin = gsub("-","", keypin),
   keypin = gsub(" ","", keypin),
   keypin = str_pad(keypin, 13, "left"),
   clean_keypin = gsub("-","", clean_keypin),
   
    clean_keypin = gsub(" ","", clean_keypin),
    classes = as.character(`class(es)`),
   first_digit = str_sub(`class(es)`, 1, 1),
   
    #kingpin = str_pad(kingpin, 13, "left"),
  ) %>% 
  arrange(desc(clean_keypin))


## Manually selected all property options that began with a 6, 7, or 8, for any year (2021, 2022, and 2023)
## using online Cook County data portal
prefiltered <- readxl::read_excel("./Necessary_Files/Manual_filter_assessorscommercialproperty.xlsx")

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

