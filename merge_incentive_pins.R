# Setup Packages ----------------------------------------------------------
library(tidyverse)
library(ptaxsim)
library(DBI)
library(httr)
library(jsonlite)
library(glue)
library(sf)
library(readxl)


# Incentive PINs Each Year from PTAXSIM ------------------------------------------------


ptax_pins <- read_csv("./Output/incentivePINs_allyears.csv") # file created in helper_pull_incentivepins_allyears.R


ptax_pins %>% group_by(pin) %>% summarize(count = n()) %>% 
  arrange(-count)
# 5869 incentive PINs have existed at some point in time.
ptax_pins %>% filter(class > 599 & class < 900) %>% group_by(year) %>% summarize(incentive_count = n())
# 4383 existed in 2022. 
# 3652 existed in 2021, etc.

ptax_pins %>% group_by(pin) %>% summarize(count = n()) %>% filter(count > 16)
# 632 PINs have existed AND been incentive properties for all years in database.
# 4,654 PINs existed during every year (doesn't matter what property class)

# 5869-4654 PINs created since 2006 ~ 1200 new PINs

ptax_pins %>% mutate(parcel = str_sub(pin, 1, 10) ) %>%
  group_by(parcel) %>% summarize(count = n())

ptax_pins %>% mutate(block = str_sub(pin, 1, 7) ) %>%
  group_by(block) %>% summarize(count = n())

pin_change <- ptax_pins %>% 
  pivot_wider(id_cols = c(pin), names_from = "year", values_from =  "class") %>%
  mutate(change = as.numeric(`2021`)-as.numeric(`2006`)) %>% 
  filter(change !=0
  )
pin_change
# at least 47 incentive PINs changed incentive class type over the years
# became 3,132 PINs after merging more complete file in.



# CMAP DATABASE -----------------------------------------------------------
# combined PIN tables into 1 file
# 10,275 obs from copying/pasting PIN tables into one table.
# 9,209 unique PINs
# 9023 unique parcels
# 3679 unique "Controls" (synonymous with Key PIN I think)
# Includes all incentive PINs, even from old projects
# excel tab: 'incentive PINs (ever)' 
access_db <- read_excel("incentivePINs_accessDB_2.xlsx")


# create a keypin variable based on smallest PIN to compare to keypin used by assessor
# ideally, each CONTROL variable would have its own Key PIN.
access_db <- access_db %>% 
  arrange(Status_cleaned, `Concat PIN`) %>% # `Status Cleaned` was manually created in Excel & based off of the `Status` variable.
  group_by(CONTROL, Status_cleaned) %>% 
  mutate(keypin = first(`Concat PIN`), # ideally grabs lowest PIN since that appears to mostly be how the assessor does it. But not always. 
         pin = `Concat PIN`,
         n_PINs_inControlGroup = n(),  # Can include the same PIN multiple times!! 
         keypin_class = first(Class),
         parcel = str_sub(pin, 1, 10),
         block = str_sub(pin, 1, 7)
         ) %>% 
  select(pin, keypin, CONTROL, n_PINs_inControlGroup, keypin_class, Status_cleaned, parcel, block,  everything()) %>% 
  arrange(CONTROL, keypin, pin)

## Good start of making access database comparable to keypin database of assessor valuation data

access_db %>% group_by(pin) %>% summarize(count = n())

# Join PTAXSIM & Data from CMAP -------------------------------------------

## Join PTAXSIM incentive properties to CMAP incentive data
# CMAP incentive PINs appear multiple times in database
incentive_pins <- left_join(ptax_pins, access_db, by = c("pin" = "pin"), relationship = "many-to-many" )
# Left join adds duplicate entries from Access files 
# two different status entries creates additional rows when joined

incentive_pins %>%
  distinct(pin) %>%
  count()    # 5,869 unique pins

incentive_pins %>%
  distinct(parcel) %>%
  count()    # 4,808 parcels


# keep only PINs that existed in both databases
innerjoin_pins <- inner_join(ptax_pins, access_db, by= c("pin" = "Concat PIN")) %>%
  select(pin, Status, `Start Year`, class, Notes )

# Find pins that didn't exist in both. 
nojoin_pins <- anti_join(ptax_pins, access_db, by= c("pin" = "Concat PIN"))
nojoin_pins %>% group_by(year) %>% summarize(count = n())
# almost all PINs that didn't match came from last 2 years

nojoin_pins2 <- anti_join(access_db, incentive_pins, by= c("Concat PIN" = "pin"))
nojoin_pins2 %>% group_by(CONTROL) %>% summarize(count = n())



incentive_pins %>% group_by(CONTROL) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,284 incentive projects
# 2149 obs
# 2775 CONTROL variables when using all incentive pins ever from ptaxsim (instead of only 2021)

incentive_pins %>% group_by(year, keypin) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,253 incentive projects based on keypins.
# 1,967 obs
# 2,651 key pins ever


incentive_pins %>% group_by(year, CONTROL) %>% summarize(pin_count = n()) %>% arrange(-pin_count)



# Control = 7028 has 153 pins associated with it. Started in 2013. Tax Year 2023 is its first ramp up year. 
# CostCo at Hastings and Ashland in Chicago, Class 7b
# PINs are primarily in blocks 1719221 & 1719215
# PINs do exist in PTAXSIM database 


# Control = 7007 has 64 PINs but the project is expired. 
# was for LAWNDALE PLAZA SHOPPING CENTER
# incentive PINs do exist in 2006, 2007, and 2008 before it expired. 


anti_join(ptax_pins, access_db, by = "pin") %>% filter(year < 2020) %>% group_by(pin) %>% summarize(n = n())
# 946 PINs existed in either the ptaxsim database or the cmap database.
# Makes sense since there was a huge increase in incentive PINs in the last tax year (2022)
# access database was last updated in end of 2020 
# 84 PINs didn't match if using pre 2020 PINs
 


# # Commercial Valuation Dataset - Cook County Data Portal ------------------

## NEED TO DOWNLOAD WHOLE FILE AND THEN CLEAN AND FILTER IT.
## NOT DONE YET - AWM 02/23/2024

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


## Filtered Commercial Valuation Dataset - Cook County Data Portal ------------------

## Manually selected all property options that began with a 6, 7, or 8, for any year (2021, 2022, and 2023)
## using online Cook County data portal
prefiltered <- readxl::read_excel("./Necessary_Files/Manual_filter_assessorscommercialproperty.xlsx")

prefiltered <- prefiltered %>% 
  mutate(keypin_concat = as.character(keypin_concat), 
   keypin_concat = str_pad(keypin_concat, 14, "left", pad = "0"))  %>%
  select(keypin_concat:`class(es)`)


table(prefiltered$year)

table(prefiltered$`class(es)`)


keypins <- unique(prefiltered$keypin_concat)
# 306 unique keypins from CCAO property valuation


joined_dbs <- left_join(prefiltered, incentive_pins, by = c("keypin_concat" = "keypin") )


new_projects <- anti_join(prefiltered, incentive_pins, by = c("keypin_concat" = "keypin") )


pins_pivot <- prefiltered %>% 
  mutate(pins = tolower(pins)) %>%
  select(keypin_concat, keypin, pins) %>%
  mutate(has_range = ifelse(str_detect(pins,"thru"), 1, 0),
  pins = str_replace_all(pins, ",,", ",")) %>%
  mutate(pins2 = str_split(pins, pattern = ",")) %>%
  unnest(pins2) %>%
  mutate( pins2 = trimws(pins2) ) %>%
  filter(!is.na(pins2) | pins2 == " ") %>%
  mutate(pins3 = str_split(pins2, pattern = " ")) %>%
  unnest(pins3) %>%
  mutate(check_me = ifelse(str_length(pins3)<14, 1, 0))
# 657 obs

# write_csv(pins_pivot, "./Output/manually_cleaned_incentive_pins.csv")

# 707 obs after manually adding pins with weird formatting
pins_pivot_cleaned <- read.csv("./Output/manually_cleaned_incentive_pins.csv") %>%
  mutate(keypin_concat = as.character(keypin_concat)) %>%
  mutate(keypin_concat2 = str_pad(keypin_concat, 14, "left", pad = "0"))

pins_pivot_cleaned <- pins_pivot_cleaned %>% 
  mutate(check_me = ifelse(str_length(pins3) < 14, 1, 0)) %>% 
  filter(check_me == 0)
# 696 obs

pins_pivot_cleaned %>% group_by(keypin_concat) %>% summarize(n = n()) %>% arrange(-n)
# 216 key pins (aka projects)
    

incentive_pins <- incentive_pins %>% 
  mutate(keypin_com_props = ifelse(pin %in% keypins, 1, 0),
         #kingpin = ifelse(pin %in% cleanpins, 1, 0),
         keypin_com_props2 = ifelse(pin == keypin, 1, 0)) %>%
  select(year,CONTROL, keypin, keypin_com_props, keypin_com_props2, pin, everything())

table(incentive_pins$keypin_com_props)
table(incentive_pins$keypin_com_props2)
 


