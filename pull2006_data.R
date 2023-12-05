library(tidyverse)
library(DBI)
library(data.table)
library(ggspatial)
library(gstat)
library(here)
library(httr)
library(jsonlite)
library(ptaxsim)
library(sf)
library(stars)
library(glue)

# Create the DB connection with the default name expected by PTAXSIM functions
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2021.0.4.db")


nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")

class_dict <- read_csv("./Necessary_Files/class_dict.csv")

cook_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name
  FROM agency_info
  "
)

# # has all tax codes and the taxing agency that taxes them. Tax code rates and agency rates.
# # 41,948 in 2006?
# cook_tax_codes <- DBI::dbGetQuery(
#   ptaxsim_db_conn,
#   glue_sql("
#   SELECT*
#   FROM tax_code
#   WHERE agency_num IN ({cook_agency_names$agency_num*})
#   AND year = 2006
#   ",
#   .con = ptaxsim_db_conn
#   )
# )  

## has all tax codes and the composite tax rate for the tax code 
## 2917 in 2006
tax_codes <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT tax_code_num, tax_code_rate
  FROM tax_code
  WHERE year = 2006  
  ",
  .con = ptaxsim_db_conn
  )
)



# There are 1,864,594 pins taxed by Cook County in 2021.
# There are 1,771,315 pins in 2006
cook_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT DISTINCT pin, class, tax_code_num
  FROM pin
  WHERE tax_code_num IN ({tax_codes$tax_code_num*})
  AND year = 2006
  ",
  .con = ptaxsim_db_conn
  ))


# grabs all unique muni names & numbs
# don't forget Cicero
muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
  FROM agency_info
  WHERE minor_type = 'MUNI'
  OR agency_num = '020060000'  

  "
)

muni_agency_names <- muni_agency_names %>% 
  mutate(first6 = str_sub(agency_num,1,6),
         first5 = str_sub(agency_num,1,5)) %>% 
  select(-minor_type)



#Makes a list of ALL taxing agencies, including TIFs, SSAs, etc.

# all agency names, numbers, and types
# includes TIF and non-TIF agencies
all_taxing_agencies <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT agency_num, agency_name, major_type, minor_type
  FROM agency_info
  "
) %>% mutate(first6 = str_sub(agency_num,1,6),
             first5 = str_sub(agency_num,1,5))


muni_agency_nums<- all_taxing_agencies %>% 
  filter(minor_type %in% c("MUNI") | 
           agency_num == "020060000") %>%
  select(agency_num)


# list of all taxcodes in municipalities. 
# This does NOT include unincorporated tax codes!!
muni_tax_codes <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT*
  FROM tax_code
  WHERE agency_num IN ({muni_agency_names$agency_num*})
  AND year = 2006
  ",
  .con = ptaxsim_db_conn
  )
)# %>% select(-agency_rate)

# Agency number and agency name for all TIFs
TIF_agencies <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, major_type, minor_type
  FROM agency_info
  WHERE minor_type = 'TIF'
  "
)

unique_tif_taxcodes <- DBI::dbGetQuery(
  ptaxsim_db_conn, 
  glue_sql("
  SELECT DISTINCT tax_code_num
  FROM tax_code
  WHERE agency_num IN ({TIF_agencies$agency_num*})
  AND year = 2006
  ",
  .con = ptaxsim_db_conn
  )
)


tif_distrib <- DBI::dbGetQuery(
  ptaxsim_db_conn, 
  glue_sql("
  SELECT *
  FROM tif_distribution
  WHERE tax_code_num IN ({muni_tax_codes$tax_code_num*})
  AND year = 2006
  ",
  .con = ptaxsim_db_conn
  )
) %>% mutate(tax_code_num = as.character(tax_code_num))



all_taxing_agencies <- all_taxing_agencies %>% 
  left_join(muni_agency_names, by = c("first5", "first6")) %>% 
  rename(muni_name =  agency_name.y,
         muni_num = agency_num.y,
         agency_name = agency_name.x,
         agency_num = agency_num.x)


# combine taxing agency names and agency type to data table that has eav and extension values
agency_data <- right_join(agency_dt, all_taxing_agencies) %>% 
  # get rid of unneeded columns to make table outputs smaller
  select(-c(cty_dupage_eav:cty_livingston_eav)) %>% # drop some of the unused variables
  arrange(agency_num)


taxbills_2006 <- tax_bill(2006,  
                             cook_pins$pin, 
                             #  pin_dt = exe_dt, # default option, change for "no exemption" simulation
                             simplify = FALSE)


sapply(taxbills_current, function(x) sum(is.na(x)))


# 1,825,816 billed properties with 14-digit PINs  
pin14_bills_2006 <- taxbills_2006 %>%
  group_by(tax_code, class, pin) %>%
  
  mutate(total_bill = final_tax_to_dist + final_tax_to_tif) %>% # from each taxing agency
  
  summarize(total_billed = sum(total_bill, na.rm = TRUE), # total on someone's property tax bill
            av = first(av),
            eav = first(eav),
            taxing_agency_count = n(), # number of taxing agencies that tax the pin
            final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE), # portion of all levies paid by the pin
            final_tax_to_tif = sum(final_tax_to_tif, na.rm = TRUE), 
            tax_amt_exe = sum(tax_amt_exe, na.rm = TRUE),           # revenue lost due to exemptions
            tax_amt_pre_exe = sum(tax_amt_pre_exe, na.rm = TRUE),   # total rev before all exemptions
            tax_amt_post_exe = sum(tax_amt_post_exe, na.rm = TRUE), # total rev after all exemptions
            rpm_tif_to_cps = sum(rpm_tif_to_cps, na.rm = TRUE),     # not used
            rpm_tif_to_rpm = sum(rpm_tif_to_rpm, na.rm=TRUE),       # not used
            rpm_tif_to_dist = sum(rpm_tif_to_dist, na.rm=TRUE),     # not used
            tif_share = mean(tif_share, na.rm=TRUE),                # not used
  )  %>% 
  mutate(propclass_1dig = str_sub(class, 1, 1))

head(pin14_bills_2006)

sapply(pin14_bills_2006, function(x) sum(is.na(x)))

rm(taxbills_2006)


# finds all pins within a municipality
pin_data <- lookup_pin(2006, cook_pins$pin) %>%
  left_join(cook_pins, by = c("pin", "class"))


# change variable type to character so the join works.
class_dict$class_code <- as.character(class_dict$class_code)



# use the property class to make the major property types
# joins the class_dict file to the pin_data classes
pin_data <- class_dict %>%
  #select(-c(assessment_level:reporting_group, class_desc:max_size)) %>%
  right_join(pin_data, by = c("class_code" = "class"))


## summarize pin level data to the tax code level for each type of property class
exemptions_inCook_perTC <- pin_data %>%
  group_by(tax_code_num, class_code
           #, major_class_code, major_class_type
  ) %>%
  summarize(av = sum(av, na.rm = TRUE),
            eav=sum(eav, na.rm=TRUE),
            exe_homeowner = sum(exe_homeowner, na.rm=TRUE),
            exe_senior = sum(exe_senior, na.rm=TRUE),
            exe_freeze = sum(exe_freeze, na.rm=TRUE),
            exe_longtime_homeowner = sum(exe_longtime_homeowner, na.rm=TRUE),
            exe_disabled = sum(exe_disabled, na.rm=TRUE),
            exe_vet_returning = sum(exe_vet_returning, na.rm=TRUE),
            exe_vet_dis = sum(exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70, na.rm=TRUE),
            exe_abate = sum(exe_abate, na.rm=TRUE),
            pin_count = n() # number of pins within each tax code and property combo
            
  )



## Add exemption types and values to the tax bill data at PIN level
joined_pin_data <- left_join(pin14_bills_2006, pin_data, by = c("pin", "class" = "class_code" ))

## Add tax code tax rate to PIN level data
pin_data <- left_join(joined_pin_data, tax_codes, by = c("tax_code" = "tax_code_num"))

head(pin_data)
#pin_data2 <- left_join(pin_data, taxcode_taxrates, by = c("tax_code" = "tax_code"))


pin_data2 <- pin_data %>% left_join(muni_tax_codes) 

head(pin_data2)

pin_data2 <- pin_data2 %>% left_join(muni_agency_names)

write_csv()
