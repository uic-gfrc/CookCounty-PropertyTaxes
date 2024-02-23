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


# Incentive PINs Each Year ------------------------------------------------

library(ptaxsim)
library(DBI)
library(glue)

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

cook_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT DISTINCT *
  FROM pin
  ",
  .con = ptaxsim_db_conn
  ))


cook_pins <- cook_pins %>% filter(class > 599 & class < 900)
cook_pins %>% group_by(year) %>% summarize(incentive_count = n())


pin_classes <- cook_pins %>% group_by(pin, class) %>% distinct()

cook_pins %>% group_by(pin) %>% summarize(count = n()) %>% arrange(-count)
cook_pins %>% group_by(pin) %>% summarize(count = n()) %>% filter(count > 16)
# 632 PINs have existed AND been incentive properties for all years in database.


cook_pins %>% 
  pivot_wider(id_cols = c(pin), names_from = "year", values_from =  "class") %>%
  mutate(change = as.numeric(`2021`)-as.numeric(`2006`)) %>% 
  filter(change !=0
  )


# CMAP DATABASE -----------------------------------------------------------
# combined PIN tables into 1 file
# 10,275 obs from copying/pasting PIN tables into one table.
# 3677 unique "Controls" (synonimous with Key PIN I think)
# Includes all incentive PINs, even from old projects
# excel tab: 'incentive PINs (ever)' 
access_db <- read_excel("incentivePINs_accessDB_2.xlsx")


# create a keypin variable based on smallest PIN to compare to keypin used by assessor
# ideally, each CONTROL variable would have its own Key PIN.
access_db <- access_db %>% 
  arrange(Status_cleaned,`Concat PIN`) %>%
  group_by(CONTROL, Status_cleaned) %>% 
  mutate(keypin = first(`Concat PIN`),
         pin = `Concat PIN`,
         n_PINs_inControlGroup = n(),  # Can include the same PIN multiple times!! 
         class = first(Class),
         parcel = str_sub(pin, 1, 10),
         block = str_sub(pin, 1, 7)
         ) %>% 
  select(pin, keypin, CONTROL, n_PINs_inControlGroup, class, Status_cleaned, parcel, block) %>% 
  arrange(CONTROL, keypin, pin)

## Good start of making access database comparable to keypin database of assessor valuation data


## Join PTAXSIM incentive properties to CMAP incentive data
# CMAP incentive PINs appear multiple times in database
incentive_pins <- left_join(incentive_pins, access_db, by = c("pin" = "pin") )

incentive_pins %>% group_by(CONTROL) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,284 incentive projects
# 2149 obs

incentive_pins %>% group_by(keypin) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,253 incentive projects based on keypins.
# 1,967 obs


# 
# # Commercial Valuation Dataset - Cook County Data Portal ------------------
# 
# commerc_prop_keys <- read_csv("./Necessary_Files/Assessor_-_Commercial_Valuation_Data_20240212.csv") 
# 
# commerc_prop_keys <- commerc_prop_keys%>%
#  # filter(#year == 2021 #& !is.na(pins)
#   #       ) %>% 
#   select(clean_pin, clean_keypin, keypin, pins, `class(es)`) %>%
#   mutate(
#     first_digit = str_sub(`class(es)`, 1, 1),
#    # clean_pin = as.character(clean_pin),
#    # clean_pin = str_pad(clean_pin, 13, "left"),
#    # keypin = as.character(keypin),
#    keypin = gsub("-","", keypin),
#    keypin = gsub(" ","", keypin),
#    keypin = str_pad(keypin, 13, "left"),
#    clean_keypin = gsub("-","", clean_keypin),
#    
#     clean_keypin = gsub(" ","", clean_keypin),
#     classes = as.character(`class(es)`),
#    first_digit = str_sub(`class(es)`, 1, 1),
#    
#     #kingpin = str_pad(kingpin, 13, "left"),
#   ) %>% 
#   arrange(desc(clean_keypin))


# Commercial Valuation Dataset - Cook County Data Portal ------------------

## Manually selected all property options that began with a 6, 7, or 8, for any year (2021, 2022, and 2023)
## using online Cook County data portal
prefiltered <- readxl::read_excel("./Necessary_Files/Manual_filter_assessorscommercialproperty.xlsx")

prefiltered <- prefiltered %>% 
  mutate(keypin_concat = as.character(keypin_concat), 
   keypin_concat = str_pad(keypin_concat, 14, "left", pad = "0"))  %>%
  select(keypin_concat:`class(es)`)

# #### Assign Unique ID ######
# library(data.table)
# 
# setDT(prefiltered)[, Unique_ID := .GRP, by = keypin_concat]
# prefiltered <- prefiltered %>% arrange(keypin_concat)
# 
# prefiltered$pin = 1
# 
# nrow(prefiltered) # 306
# 
# for (i in 1:306){
#   prefiltered$pin_num[[i + 1]] = ifelse(prefiltered$Unique_ID[[i + 1]] != prefiltered$Unique_ID[[i]], 1, 
#                                  (prefiltered$pin_num[[i]] + 1))
# }

table(prefiltered$year)

table(prefiltered$`class(es)`)


keypins <- unique(prefiltered$keypin_concat)
# 306 unique keypins from CCAO property valuation


joined_dbs <- anti_join(prefiltered, incentive_pins, by = c("keypin_concat" = "pin") )

pins_pivot<- prefiltered %>% select(keypin_concat, pins) %>%
  muatate(pin[i] = str_split(pins, pattern = ","))

  pivot_wider(id_cols = keypin_concat, values_from = "pins" , names_from = "pins", 
              names_sep = ",")


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

