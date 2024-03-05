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


ptax_pins  %>% group_by(pin, class) %>% 
  summarize(count = n(),
            first_year = first(year),
            last_year = last(year)) %>% 
  arrange(-count)
# 5869 incentive PINs have existed at some point in time.
# 6,178 groups exist when grouping by pin and class
# implying that some incentive PINs change property classes over time


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
  filter(change !=0)

pin_change
# at least 47 incentive PINs changed incentive class type over the years
# became 3,132 PINs after merging more complete file in.

# ptax_incentpins_allyrs <- ptax_pins


# ptax_pins <- ptax_pins %>% filter(class > 599 & class < 900)
# 45,724 obs when filtered for only years they were incentive classes

# # only grouped by pins, not used
# unique_ptax <- ptax_pins %>% 
#   group_by(pin) %>% 
#   summarize(years_existed = n(),
#             first_year = first(year),
#             last_year = last(year))


##  Grouped by PIN and 1st digit of class (aka major class) -----------------
unique_ptax_MC <- ptax_pins %>% 
  mutate(majorclass = str_sub(class, 1, 1)) %>%
  group_by(pin, majorclass) %>% 
  summarize(count = n(),
            first_year = first(year),
            last_year = last(year)) %>% ungroup()

unique_ptax_w_MC <- unique_ptax_MC %>% 
  group_by(pin) %>%
  mutate(var2 = cumsum(row_number() == 1 | (majorclass != dplyr::lag(majorclass)))) %>% 
  ungroup()


unique_ptax_wide_MC <- unique_ptax_w_MC %>%
  pivot_wider(id_cols = "pin",
              names_from = var2,
              values_from = c(majorclass, count, first_year, last_year))


## Grouped by PIN and class ----------------------------------------------
unique_ptax_w_class <- ptax_pins %>% 
  group_by(pin, class) %>% 
  summarize(count = n(),
            first_year = first(year),
            last_year = last(year)) %>% 
  ungroup() %>%
  arrange(pin, first_year)

  
unique_ptax_w_class <- unique_ptax_w_class %>% group_by(pin) %>%
  mutate(var2 = cumsum(row_number() == 1 | (class != dplyr::lag(class))))


unique_ptax_wide <- unique_ptax_w_class %>%
  pivot_wider(id_cols = "pin",
              names_from = var2,
              values_from = c(class, count, first_year, last_year))





# # Commercial Valuation Dataset - Cook County Data Portal ------------------

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
  mutate(keypin_concat = as.character(keypin)) %>%
  
  # an odd ball that showed up with a class == 60. searched it and manually adding it and then removing it just to document it.
  mutate(`class(es)` = ifelse(keypin_concat == "17313100110000", "522", `class(es)`)) %>%
  
  mutate(keypin_concat = as.character(keypin),
         keypin_concat = str_remove_all(keypin_concat, "-"),
         keypin_concat = str_pad(keypin_concat, 14, "left", pad = "0"),
         class_4dig = str_sub(`class(es)`, 1, 4),
         class_3dig = str_remove_all(class_4dig, "[-,]"))  %>%
  filter(class_3dig > 599) %>% # remove the oddball
  select(keypin_concat, class_3dig, pins, `class(es)`, everything()) 
# 1830 obs

table(comval$year)

table(comval$class_3dig)
table(comval$first_dig)


keypins <- unique(comval$keypin_concat)


# joined_dbs <- comval %>% select(-c(year,studiounits:`4brunits`)) %>%
#   left_join(incentive_pins, by = c("keypin_concat" = "keypin") ) %>%
#   select(keypin_concat, class_3dig, pin, year, av_mailed, av_certified, CONTROL, keypin_class, everything() )
# # 26946 obs

# 
# joined_dbs2 <- comval %>% select(-c(year,studiounits:`4brunits`)) %>%
#   right_join(incentive_pins, by = c("keypin_concat" = "keypin") ) %>%
#   select(keypin_concat, class_3dig, pin, year, av_mailed, av_certified, CONTROL, keypin_class, everything() )
# 


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



# OLD count - 707 obs after manually adding pins with weird formatting
# 3201 obs after manually adding pins with weird formatting when using nonfiltered assessor commercial valuation file 
pins_pivot_cleaned <- read.csv("./Output/manually_cleaned_incentive_pins_AWM.csv") %>%
  mutate(keypin_concat = as.character(keypin_concat)) %>%
  mutate(keypin_concat2 = str_pad(keypin_concat, 14, "left", pad = "0"))

pins_pivot_cleaned <- pins_pivot_cleaned %>% 
  mutate(check_me = ifelse(str_length(pins3) < 14, 1, 0)) %>% 
  filter(check_me == 0)
# OLD count - 696 obs
# 3170 obs


# number of pins associated with the key pin
pins_pivot_cleaned %>% group_by(keypin_concat) %>% summarize(n = n()) %>% arrange(-n)
# OLD - 216 key pins (aka projects)
# 1831 key pins (aka projects)

pins_pivot_cleaned <- pins_pivot_cleaned %>% 
  mutate(pin_cleaned = str_remove_all(pins3, "-")) %>%
  select(keypin_concat, pin_cleaned) %>%
  mutate(keypin_concat = as.character(keypin_concat),
         keypin_concat = str_pad(keypin_concat, 14, side = "left", pad = "0"))


unique_comval <- pins_pivot_cleaned %>% select(pin_cleaned, keypin_concat) %>% 
  distinct() %>%
  mutate(keypin_concat = as.character(keypin_concat),
         keypin_concat = str_pad(keypin_concat, 14, side = "left", pad = "0"))


# Create PIN Crosswalk for Project and Keypin -----------------------------


# Combine unique incentive PINs that have existed ever, The CONTROL variable from CMAP,
# and the keypin from the experimental commercial valuation dataset 

head(unique_ptax)

head(unique_ptax_wide)

head(unique_comval)

cleanjoin <- full_join(unique_ptax_wide, unique_comval, by = c("pin" = "pin_cleaned"))
# 6,064 obs

cleanjoin <- cleanjoin %>% select(keypin = keypin_concat, pin, 
                                  class_1, first_year_1, last_year_1, yrs_existed_1 = count_1,
                                  class_2, first_year_2, last_year_2, yrs_existed_2 = count_2,
                                  class_3, first_year_3, last_year_3, yrs_existed_3 = count_3,
                                  
)
# cleanjoin %>% distinct(pin) # 6062 distinct pins

write_csv(cleanjoin, "./Output/project_pins_wide.csv")
# cleanjoin %>% filter(is.na(years_existed))       
# 193 obs are missing "years existed" variable.
# meaning that they do not exist as incentives in the PTAXSIM database.
# BUT they are associated with an incentive project where all the pins are not incentive PINs
# non-incentive PINs get in there from unnesting the comval dataset in previous steps

# comval dataset included all pins associated with a keypin but does not clarify which pin is which class type
nonincent_pins <- cleanjoin %>% filter(is.na(years_existed))       # 183 obs are missing "years existed" variable.
# these PINs are associated with incentive projects, but were not classified as incentive PINs.

cleanjoin <- cleanjoin %>% filter(!is.na(years_existed))    # 5871 remaining unique PINs that were incentives in PTAXSIM database at some point
cleanjoin %>% distinct(pin)                                 # 5968 unique pins

cleanjoin2 <- full_join(cleanjoin, unique_cmap)

write.csv(cleanjoin, "./Output/incentive_crosswalk.csv")



# Other code that brings in CMAP's access database files: ------------------


## Join PTAXSIM db with Online Commercial Valuation dataset ----------------

ptax_pins <- ptax_pins %>% filter(class > 599 & class < 900)

joined <- ptax_pins %>% left_join(pins_pivot_cleaned, by = c("pin" = "pin_cleaned")) 

joined <- joined %>% select(year, pin, keypin_concat, av_mailed, av_clerk, everything())

# pins2021 <- joined %>% filter(year == 2021)
# pins2021 %>% group_by(keypin_concat) %>% summarize(n = n()) %>% arrange(-n)
# 1763 projects, 788 NAs


# pins2019 <- joined %>% filter(year == 2019)
# pins2019 %>% group_by(keypin_concat) %>% summarize(n = n()) %>% arrange(-n)
# 1576 projects, 855 NAs that didn't have keypins?



## CMAP DATABASE -----------------------------------------------------------
# combined PIN tables into 1 file
# 10,275 obs from copying/pasting PIN tables into one table.
# 9,209 unique PINs
# 9023 unique parcels
# 3679 unique "Controls" (synonymous with Key PIN I think) aka "Projects"
# Includes all incentive PINs, even from old projects
# excel tab: 'incentive PINs (ever)' 
access_db <- read_excel("incentivePINs_accessDB_2.xlsx")


missing <- c("n/a", "N/A", "NA")

access_db <- access_db %>% 
  mutate(startyear_clean = ifelse(`Start Year` %in% missing, NA, `Start Year`))


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


unique_cmap <- access_db %>% 
  ungroup() %>%
  reframe(`Concat PIN`, CONTROL, startyear_clean, 
          n_occurs = n(), .by = `Concat PIN`) %>%
  select(pin = `Concat PIN`, CONTROL, startyear_clean) %>% 
  distinct() %>% 
  arrange(pin, desc(startyear_clean))



## Good start of making access database comparable to keypin database of assessor valuation data

# access_db %>% group_by(pin) %>% summarize(count = n())

## Join PTAXSIM & Data from CMAP -------------------------------------------

## Join PTAXSIM incentive properties to CMAP incentive data
# CMAP incentive PINs appear multiple times in database
incentive_pins <- left_join(ptax_pins, access_db, by = c("pin" = "pin"), 
                            relationship = "many-to-many" )
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

incentive_pins <- incentive_pins %>% 
  mutate(keypin_com_props = ifelse(pin %in% keypins, 1, 0),
         #kingpin = ifelse(pin %in% cleanpins, 1, 0),
         keypin_com_props2 = ifelse(pin == keypin, 1, 0)) %>%
  select(year,CONTROL, keypin, keypin_com_props, keypin_com_props2, pin, everything())

table(incentive_pins$keypin_com_props)
table(incentive_pins$keypin_com_props2)


anti_join(ptax_pins, access_db, by = "pin") %>% filter(year < 2020) %>% group_by(pin) %>% summarize(n = n()) %>% arrange(-n)
# 946 PINs existed in either the ptaxsim database or the cmap database.
# Makes sense since there was a huge increase in incentive PINs in the last tax year (2022)
# access database was last updated in end of 2020 
# 94 PINs didn't match if using pre 2020 PINs. Not too bad. 
 



