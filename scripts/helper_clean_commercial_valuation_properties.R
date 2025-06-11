#########################################################################
# Combines Commercial Valuation Data [a preliminary/experimental dataset!] 
#        Downloaded on March 1st, 2024 from the Cook County Data Portal
# Extracts keypin and pin variables to create a keypin-pin crosswalk
#        to be used with data extracted from PTAXSIM
# Initial steps of building panel data to examine commercial and industrial 
#        property in Cook County
#########################################################################


library(tidyverse)
library(ptaxsim)
library(DBI) 
library(glue)
library(janitor)


amazon <- readxl::read_excel("./amazonPINs.xlsx")
library(stringr)
library(tidyverse)

amazon <- amazon %>% 
  mutate(pin_clean = str_remove_all(PIN, "-"),
         parcel = str_sub(pin_clean, 1, 10),
         block = str_sub(pin_clean,1,7),
         township = str_sub(pin_clean, 1, 2))


# Archived Parcel Universe Dataset -----

parcuniverse_keypins <- readxl::read_xlsx("./Inputs/parceluniverse_keypins_20240725.xlsx", 
                                          sheet = "keypins_20240725") %>%
  mutate(pin = str_remove_all(pin, "-")) 

parcuniverse_keypins <- parcuniverse_keypins %>%
  mutate(
    pin14 = str_pad(as.character(pin), width = 14, side = "left", pad = "0"),
    keypin = str_pad(as.character(proration_key_pin), width = 14, side = "left", pad = "0"),
    pin10 = str_sub(pin14,1,10),
    pin7 = str_sub(pin14,1,7), .before = "pin") %>%
  select(-c(pin_7dig, pin, Column1)) %>%
  filter(class != "EX")

parcuniverse_keypins %>% 
  group_by(keypin) %>% 
  summarize(pincount = n(), 
            class = mean(as.numeric(class)),
            has_incent = sum(ifelse(as.numeric(between(class, 600, 899)), 1, 0), na.rm=TRUE)) %>%
  filter(pincount > 1) %>% 
  arrange(desc(has_incent), desc(pincount))

# Commercial Valuation Dataset - Cook County Data Portal ------------------


# downloaded entire file from data portal on March 1 2024. No prefiltering:
comval <- read_csv("./Necessary_Files/Assessor_-_Commercial_Valuation_Data_20240301.csv") 

comval <- comval %>% 
  filter(keypin > 0 & keypin != "TOTAL PINS")
#  63,142 observations for 2021, 2022, and 2023. Includes all commercial property

## Clean up / create a class variable using the first class that appears for each keypin
comval <- comval %>% 
  mutate(keypin_concat = as.character(keypin)) %>%
  
  # an odd ball that showed up with a class == 60. searched it and manually adding it and then removing it just to document it.
  mutate(`class(es)` = ifelse(keypin_concat == "17313100110000", "522", `class(es)`)) %>%
  
  mutate(keypin_concat = as.character(keypin),
         keypin_concat = str_remove_all(keypin_concat, "-"),
         keypin_concat = str_pad(keypin_concat, 14, "left", pad = "0"),
         class_4dig = str_sub(`class(es)`, 1, 4),
         class_3dig = str_remove_all(class_4dig, "[-,]"),
         pins = as.character(pins))  %>%
  select(keypin_concat, class_3dig, pins, `class(es)`, everything()) 
# 63142 obs - implies the number of projects since the comval data was 1 keypin per row


keypins <- unique(comval$keypin_concat)
# 62,925 unique keypins 

pins_pivot <- comval %>% 
  filter(class_3dig < 600) %>%
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
  mutate(check_me = ifelse(str_length(pins3) < 14, 1, 0)) # if pin does not have the number of characters expected (14), then flag it
# 81128 obs




# not sure if all KeyPINs exist as their own PIN variable  (i.e. the keypin = pin)
# so adding this step just in case
# will create redundant rows but that is fine, we only keep unique occurrences later

pins_pivot <- pins_pivot %>%    # was read.csv("./Output/manually_cleaned_incentive_pins_AWM.csv") %>%
  filter(check_me == 0) %>%
  mutate(keypin_concat = as.character(keypin_concat)) %>%
  mutate(keypin_concat2 = str_pad(keypin_concat, 14, "left", pad = "0"),
         pins_add = keypin_concat2) %>%     
  mutate(pin_cleaned = str_remove_all(pins3, "-")) 

addinkeypin_PINs <- pins_pivot %>% 
  select(keypin_concat = keypin_concat2, # need matching variable names for row bind
         pin_cleaned = pins_add)

pins_pivot_cleaned <- pins_pivot %>% 
  select(keypin_concat, pin_cleaned)

pins_pivot_cleaned <- rbind(pins_pivot_cleaned, addinkeypin_PINs)
pins_pivot_cleaned <- pins_pivot_cleaned %>% unique()
# 78,067 obs

pins_pivot_cleaned <- pins_pivot_cleaned  %>%
  mutate(keypin_concat = as.character(keypin_concat),
         keypin_concat = str_pad(keypin_concat, 14, side = "left", pad = "0")) %>%
  mutate(check_me = ifelse(str_length(pin_cleaned) < 14, 1, 0)) %>% 
  filter(check_me == 0)
# 78,049


unique_comval <- pins_pivot_cleaned %>% 
  select(pin_cleaned, keypin_concat) %>% 
  distinct()
# 78,049 PINs with keypins.


unique_comval %>% group_by(keypin_concat) %>% summarize(n = n()) %>% arrange(-n)
# 50,115 unique pins with their keypin



# PTAXSIM Commercial PINs - all years -------------------------------------

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

## Pulls ALL distinct PINs that existed, ever.
## Takes a while to run. (~1 min w/ 64GB RAM)
## 31+ million obs. (around 1.5 million pins in cook X 17 years, checks out)
cook_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT *
  FROM pin
  ",
  .con = ptaxsim_db_conn
  ))    # 31,408,907  distinct observations from `pin` table in PTAXSIM



## Limit to just non-incentive major class commercial properties (500-level props) ##
# 1,602,401      500-level pin-class-year combinations
# 1,648,125      500 to 800 level pin-class properties
cook_pins <- cook_pins %>%
  filter(class > 499 & class < 900)

# get distinct commercial pins with 500-level property classes
distinct_pins <- cook_pins %>%
  select(pin) %>%
  distinct(pin)   # 117,493 distinct commercial pins
# 119,404 when using classes 500-899


## But we want those PINs as obs. for all years, even if they weren't classified
## as an commercial property for all years that time (to potentially capture land use change)

## Use unique commercial PIN list to get all obs.
commercial_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT DISTINCT *
   FROM pin
   WHERE pin IN ({distinct_pins$pin*})
  ",
  .con = ptaxsim_db_conn
  ))
# 1,798,619 obs. (PIN-YEAR combos)
# 1,825,490 obs when using classes 500-899

##  Commercial PINs Each Year from PTAXSIM ------------------------------------------------


##  Grouped by PIN and 1st digit of class (aka major class) -----------------
# unique_ptax_MC <- commercial_pins %>% 
#   mutate(majorclass = str_sub(class, 1, 1)) %>%
#   group_by(pin, majorclass) %>% 
#   summarize(count = n(),
#             first_year = first(year),
#             last_year = last(year)) %>% 
#   ungroup() %>% 
#   arrange(pin, first_year)
# 
# 
# unique_ptax_w_MC <- unique_ptax_MC %>% 
#   group_by(pin) %>%
#   mutate(var2 = cumsum(row_number() == 1 | (majorclass != dplyr::lag(majorclass)))) %>% 
#   ungroup()
# 
# 
# unique_ptax_wide_MC <- unique_ptax_w_MC %>%
#   pivot_wider(id_cols = "pin",
#               names_from = var2,
#               values_from = c(majorclass, count, first_year, last_year))


## Grouped by PIN and class ----------------------------------------------
unique_ptax_w_class <- commercial_pins %>% 
  arrange(pin, year) %>%
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

# Create PIN Crosswalk for Project and Keypin -----------------------------


# Combine unique commercial PINs that have existed ever, The CONTROL variable from CMAP,
# and the keypin from the experimental commercial valuation dataset 


cleanjoin <- full_join(unique_ptax_wide, unique_comval, by = c("pin" = "pin_cleaned"))
# 135,180 obs
# 136,955 obs

cleanjoin <- cleanjoin %>% select(keypin = keypin_concat, pin, 
                                          class_1, first_year_1, last_year_1, yrs_existed_1 = count_1,
                                          class_2, first_year_2, last_year_2, yrs_existed_2 = count_2,
                                          class_3, first_year_3, last_year_3, yrs_existed_3 = count_3,
                                          class_4, first_year_4, last_year_4, yrs_existed_4 = count_4,
                                          class_5, first_year_5, last_year_5, yrs_existed_5 = count_5,
                                          class_6, first_year_6, last_year_6, yrs_existed_6 = count_6)

# cleanjoin_MC <- full_join(unique_ptax_wide_MC, unique_comval, by = c("pin" = "pin_cleaned"))
# 
# cleanjoin_MC <- cleanjoin_MC %>% select(keypin = keypin_concat, pin, 
#                                         majorclass_1, first_year_1, last_year_1, yrs_existed_1 = count_1,
#                                         majorclass_2, first_year_2, last_year_2, yrs_existed_2 = count_2,
#                                         majorclass_3, first_year_3, last_year_3, yrs_existed_3 = count_3,
#                                         majorclass_4, first_year_4, last_year_4, yrs_existed_4 = count_4,
#                                         majorclass_5, first_year_5, last_year_5, yrs_existed_5 = count_5,
#                                         
# )


## Use unique occurrences of commercial pins that existed in tax year 2022
## and merge in keypin variables
pins2022 <- unique_ptax_w_class %>%
  filter(last_year == 2022) %>%  # implies still existed in 2022
  left_join(unique_comval, by = c("pin" = "pin_cleaned") ) %>%
  mutate(parcel = str_sub(pin, 1, 10),
         block = str_sub(pin, 1, 7),
         township = str_sub(pin, 1, 2)) %>%
  mutate(keypin_concat = ifelse(pin %in% amazon$pin_clean, "Amazon", keypin_concat))
  



## keep pins that already have a keypin and save them within commerc_projects_2022 object
keypins <- pins2022 %>%   
  
  # create indicator for if it had a pin or will have one created from the pin
  mutate(needs_keypin = ifelse(is.na(keypin_concat), 1, 0)) %>% 
  
  # if missing a keypin, fill it in with the pin so there are not NA values in keypin
  mutate(keypin_concat = ifelse(is.na(keypin_concat), as.character(pin), as.character(keypin_concat)) ) %>%

  select(pin, keypin_concat, needs_keypin) %>% group_by(keypin_concat) %>% mutate(pins_per_keypin = n()) %>% ungroup()


# want to merge in muni names to the tax codes of the pins.
source("helper_tc_muninames_2022.R")  


# keep only distinct pin and keypin observations for tax year 2022. 
projects <- keypins %>% 
  select(keypin=keypin_concat, pin, pins_per_keypin, needs_keypin) %>% 
  distinct()
# 105,236 "projects" - remember, any pins without a keypin are considered individual projects for now
# 106,939 projects after using classes 500-899


write_csv(projects, "Output/all_keypins_2022.csv")

# 106,939 pin-projects after using classes 500-899
# 
commercpins_2022 <- pins2022 %>% 
  filter(class >= 500 & class < 900) %>% 
  left_join(projects, by = "pin", relationship = "many-to-many") %>% 
#  mutate(tax_code_num = as.character(tax_code_num)) %>%
  select(keypin_concat, pin, class, everything()) # %>% 
#  select(-c(exe_homeowner:exe_vet_dis_ge70)) # %>%
 # left_join(tc_muninames) %>%
 # mutate(clean_name = ifelse(is.na(clean_name), "Unincorporated", clean_name))

