library(tidyverse)
library(ptaxsim)
library(DBI) 
library(glue)




# Commercial Valuation Dataset - Cook County Data Portal ------------------


# downloaded entire file from data portal. No prefiltering:
comval <- read_csv("./Necessary_Files/Assessor_-_Commercial_Valuation_Data_20240301.csv") %>% 
  filter(keypin > 0 & keypin != "TOTAL PINS")
#  63,142 observations for 2021, 2022, and 2023. Includes all commercial property


# property classes as a character string, grab 1st digit to help with data cleaning.
# comval <- comval %>% mutate(class_char = as.character(`class(es)`),
#                             first_dig = str_sub(`class(es)`, 1, 1),
#                             first_dig_chr = str_sub(`class(es)`, 1, 1)) 
# %>%  filter((first_dig > 5 & first_dig < 9) | (first_dig_chr > 5 & first_dig_chr < 9) )# 1831 obs



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
  filter(class_3dig < 600 | class_3dig > 800) %>%
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
# 81128 obs


pins_pivot_cleaned <- pins_pivot %>%
  filter(check_me == 0) %>%
  #read.csv("./Output/manually_cleaned_incentive_pins_AWM.csv") %>%
  mutate(keypin_concat = as.character(keypin_concat)) %>%
  mutate(keypin_concat2 = str_pad(keypin_concat, 14, "left", pad = "0"))

pins_pivot_cleaned <- pins_pivot_cleaned %>% 
  mutate(check_me = ifelse(str_length(pins3) < 14, 1, 0)) %>% 
  filter(check_me == 0)

# 80,262 rows




# number of pins associated with the key pin
pins_pivot_cleaned %>% 
  group_by(keypin_concat) %>% 
  summarize(n = n()) %>% 
  arrange(-n)
# 51,207 keypins (aka projects)
# range of 1 to 83 pins associated with keypin

pins_pivot_cleaned <- pins_pivot_cleaned %>% 
  mutate(pin_cleaned = str_remove_all(pins3, "-")) %>%
  select(keypin_concat, pin_cleaned) %>%
  mutate(keypin_concat = as.character(keypin_concat),
         keypin_concat = str_pad(keypin_concat, 14, side = "left", pad = "0"))


unique_comval <- pins_pivot_cleaned %>% 
  select(pin_cleaned, keypin_concat) %>% 
  distinct() %>%
  mutate(keypin_concat = as.character(keypin_concat),
         keypin_concat = str_pad(keypin_concat, 14, side = "left", pad = "0"))
# 79,895 PINs with keypins.


ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

## Pulls ALL distinct PINs that existed, ever.
## Takes a while to run. (~1 min w/ 64GB RAM)
## 31+ million obs. (PIN-YEAR combos)

cook_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT *
  FROM pin
  ",
  .con = ptaxsim_db_conn
  ))


## Limit to just non-incentive major classes (5)
# 31,408,907 below class 600
# 1,602,401 500 level pins
cook_pins <- cook_pins %>%
  filter(class > 499 & class < 600)

# get distinct pins
distinct_pins <- cook_pins %>%
  select(pin) %>%
  distinct(pin)   # 117,493 distinct pins


## But we want those PINs as obs. for all years, even if they weren't classified
## as an commercial property for all years that time. (to potentially capture land use change)

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


# ## Write CSV to Output Folder
# write_csv(incentive_pins, "./Output/commercialPINs_allyears.csv")

# Incentive PINs Each Year from PTAXSIM ------------------------------------------------


#ptax_pins <- read_csv("./Output/incentivePINs_allyears.csv") # file created in helper_pull_incentivepins_allyears.R


commercial_pins  %>% group_by(pin) %>%#, class) %>% 
  summarize(count = n(),
            first_year = first(year),
            last_year = last(year)) %>% 
  arrange(-count)
# 117,483 commercial PINs have existed at some point in time.
# 160,666 groups exist when grouping by pin and class
# implying that some commercial PINs change property classes over time


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
            last_year = last(year)) %>% 
  ungroup() %>% 
  arrange(pin, first_year)


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






# Create PIN Crosswalk for Project and Keypin -----------------------------


# Combine unique incentive PINs that have existed ever, The CONTROL variable from CMAP,
# and the keypin from the experimental commercial valuation dataset 

