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

ptax_pins <- ptax_pins %>% filter(class > 599 & class < 900)
# 45,724 obs

# CMAP DATABASE -----------------------------------------------------------
# combined PIN tables into 1 file
# 10,275 obs from copying/pasting PIN tables into one table.
# 9,209 unique PINs
# 9023 unique parcels
# 3679 unique "Controls" (synonymous with Key PIN I think) aka "Projects"
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

# access_db %>% group_by(pin) %>% summarize(count = n())

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


# # keep only PINs that existed in both databases
# innerjoin_pins <- inner_join(ptax_pins, access_db, by= c("pin" = "Concat PIN")) %>%
#   select(pin, Status, `Start Year`, class, Notes )

# # # Find pins that didn't exist in both. 
# nojoin_pins <- anti_join(ptax_pins, access_db, by= c("pin" = "Concat PIN"))
# nojoin_pins %>% group_by(year) %>% summarize(count = n())
# # almost all PINs that didn't match came from last 2 years



incentive_pins %>% group_by(CONTROL) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,284 incentive projects
# 2149 obs
# 2775 CONTROL variables when using all incentive pins ever from ptaxsim (instead of only 2021)


# group by created keypin variable. Uses lowest pin if multiple PINs exist within a CONTROL / project
incentive_pins %>% group_by(keypin, year) %>% summarize(pin_count = n()) %>% arrange(-pin_count)
# 1,253 incentive projects based on keypins.
# 1,967 obs
# 2,641 key pins ever. Does not increase group count when grouping by year or keypin. 
# That means keypins do not exist over multiple years in commercial valuation dataset.


incentive_pins %>% group_by(year, CONTROL) %>% summarize(pin_count = n()) %>% arrange(-pin_count)


anti_join(ptax_pins, access_db, by = "pin") %>% filter(year < 2020) %>% group_by(pin) %>% summarize(n = n()) %>% arrange(-n)
# 946 PINs existed in either the ptaxsim database or the cmap database.
# Makes sense since there was a huge increase in incentive PINs in the last tax year (2022)
# access database was last updated in end of 2020 
# 94 PINs didn't match if using pre 2020 PINs. Not too bad. 
 


# # Commercial Valuation Dataset - Cook County Data Portal ------------------

## DOWNLOADED WHOLE FILE AND NOW NEED TO CLEAN AND FILTER IT.
## NOT DONE YET - AWM 03/01/2024
# old file that was filtered for only 1 year:
# commerc_prop_keys <- read_csv("./Necessary_Files/Assessor_-_Commercial_Valuation_Data_20240212.csv") 

# downloaded entire file from data portal. No prefiltering:
comval <- read_csv("./Necessary_Files/Assessor_-_Commercial_Valuation_Data_20240301.csv") %>% 
  filter(keypin > 0 & keypin != "TOTAL PINS")
#  63,142 observations for 2021, 2022, and 2023. Includes all commercial property

comval <- comval %>% mutate(class_char = as.character(`class(es)`),
                            first_dig = str_sub(`class(es)`, 1, 1),
                            first_dig_chr = str_sub(`class(es)`, 1, 1)) %>% 
  filter((first_dig > 5 & first_dig < 9) | (first_dig_chr > 5 & first_dig_chr < 9) )
# 1831 obs


## Clean up / create a class variable using the first class that appears for each keypin
comval <- comval %>% 
  # an odd ball that showed up with a class == 60. searched it and manually adding it and then removing it just to document it.
  mutate(`class(es)` = ifelse(keypin_concat == "17313100110000", "522", `class(es)`)) %>%
  
  mutate(keypin_concat = as.character(keypin),
         keypin_concat = str_remove_all(keypin_concat, "-"),
         keypin_concat = str_pad(keypin_concat, 14, "left", pad = "0"),
         class_4dig = str_sub(`class(es)`, 1, 4),
         class_3dig = str_remove_all(class_4dig, "[-,]"))  %>%
  filter(class_3dig>599) %>% # remove the oddball
  select(keypin_concat, class_3dig, pins, `class(es)`, everything()) 
# 1830 obs

table(comval$year)

table(comval$class_3dig)
table(comval$first_dig)


keypins <- unique(comval$keypin_concat)


joined_dbs <- comval %>% select(-c(year,studiounits:`4brunits`)) %>%
  left_join(incentive_pins, by = c("keypin_concat" = "keypin") ) %>%
  select(keypin_concat, class_3dig, pin, year, av_mailed, av_certified, CONTROL, keypin_class, everything() )
# 26946 obs


joined_dbs2 <- comval %>% select(-c(year,studiounits:`4brunits`)) %>%
  right_join(incentive_pins, by = c("keypin_concat" = "keypin") ) %>%
  select(keypin_concat, class_3dig, pin, year, av_mailed, av_certified, CONTROL, keypin_class, everything() )



pins_pivot <- comval %>% 
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
# 3,131 obs



# write_csv(pins_pivot, "./Output/manually_cleaned_incentive_pins_AWM.csv")

# 707 obs after manually adding pins with weird formatting
# 3201 obs after manually adding pins with weird formatting when using nonfiltered assessor commercial valuation file 
pins_pivot_cleaned <- read.csv("./Output/manually_cleaned_incentive_pins_AWM.csv") %>%
  mutate(keypin_concat = as.character(keypin_concat)) %>%
  mutate(keypin_concat2 = str_pad(keypin_concat, 14, "left", pad = "0"))

pins_pivot_cleaned <- pins_pivot_cleaned %>% 
  mutate(check_me = ifelse(str_length(pins3) < 14, 1, 0)) %>% 
  filter(check_me == 0)
# 696 obs
# 3170 obs



## Filtered Commercial Valuation Dataset - Cook County Data Portal ------------------
# ## Manually selected all property options that began with a 6, 7, or 8, for any year (2021, 2022, and 2023)
# ## using online Cook County data portal
# prefiltered <- readxl::read_excel("./Necessary_Files/Manual_filter_assessorscommercialproperty.xlsx")
# # 306 
# 
# prefiltered <- prefiltered %>% 
#   mutate(keypin_concat = as.character(keypin_concat), 
#    keypin_concat = str_pad(keypin_concat, 14, "left", pad = "0"))  %>%
#   select(keypin_concat:`class(es)`)
# 
# table(prefiltered$year)
# 
# table(prefiltered$`class(es)`)
# 
# keypins <- unique(prefiltered$keypin_concat)
# # 306 unique keypins from CCAO property valuation
# 
# joined_dbs <- left_join(prefiltered, incentive_pins, by = c("keypin_concat" = "keypin") )
# 
# new_projects <- anti_join(prefiltered, incentive_pins, by = c("keypin_concat" = "keypin") )
# 
# pins_pivot <- prefiltered %>% 
#   mutate(pins = tolower(pins)) %>%
#   select(keypin_concat, keypin, pins) %>%
#   mutate(has_range = ifelse(str_detect(pins,"thru"), 1, 0),
#   pins = str_replace_all(pins, ",,", ",")) %>%
#   mutate(pins2 = str_split(pins, pattern = ",")) %>%
#   unnest(pins2) %>%
#   mutate( pins2 = trimws(pins2) ) %>%
#   filter(!is.na(pins2) | pins2 == " ") %>%
#   mutate(pins3 = str_split(pins2, pattern = " ")) %>%
#   unnest(pins3) %>%
#   mutate(check_me = ifelse(str_length(pins3)<14, 1, 0))
# # 657 obs

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
 


