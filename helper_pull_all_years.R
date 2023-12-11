######### 2006-2021 PTAXSIM Data Pull #########
## 12/11/2023 MVH/AWM                 ##
## PTAXSIM DB v.2021.0.4              ##
## Incentives/Exemptions Over Time    ##
## CMAP Project                       ##

######## Libraries/DB Connection ########


######## Prep ##############

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
# ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2021.0.4.db")

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "G:/My Drive/PhD/Cook County Property Taxes/Data Analysis/Working Directory/ptaxsim.db")

class_dict <- read_csv("./Necessary_Files/class_dict.csv")
nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")


#### 2006 Data Pull ####

# has EAV values, extensions by agency_num
agency_dt <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT *
  FROM agency
  WHERE year = 2006
  "
)

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

## Municipality taxing agencies only + Cicero
muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
  FROM agency_info
  WHERE minor_type = 'MUNI'
  OR agency_num = '020060000'
  "
)
## All tax codes that are taxed by an Incorporated Municipality
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
)

## All tax codes.
## tax codes within municipalities have additional info
tc_muninames <- tax_codes %>%
  left_join(muni_tax_codes) %>%
  left_join(muni_agency_names) %>%
  select(-agency_rate) %>%
  left_join(nicknames) %>%
  select(-c(minor_type, short_name, `Column1`, `Most recent reassessed`, agency_number))
# 2917 in 2006


# There are 1,864,594 pins taxed by Cook County in 2021.
# There are 1,771,314 pins in 2006
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



# combine taxing agency names and agency type to data table that has eav and extension values
agency_data <- right_join(agency_dt, muni_agency_names) %>%
  # get rid of unneeded columns to make table outputs smaller
  select(-c(cty_dupage_eav:cty_livingston_eav)) %>% # drop some of the unused variables
  arrange(agency_num)

# 22,730,572 pin-taxing agency combinations for tax bills
taxbills_2006 <- tax_bill(2006,  cook_pins$pin,
                             #  pin_dt = exe_dt, # default option, change for "no exemption" simulation
                             simplify = FALSE)


sapply(taxbills_2006, function(x) sum(is.na(x)))


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


rm(taxbills_2006)


# finds all pins within Cook county and data on their exemptions
# joins tax code variable in 2006 by pin
exemption_data <- lookup_pin(2006, cook_pins$pin) %>%
  left_join(cook_pins, by = c("pin", "class")) %>%
  mutate(all_exemptions = exe_homeowner + exe_senior + exe_freeze + exe_longtime_homeowner +
           exe_disabled + exe_vet_returning + exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70 + exe_abate) %>%
  mutate(zero_bill = ifelse(eav <= all_exemptions, 1, 0),
         has_HO_exemp = ifelse(exe_homeowner > 0, 1, 0))


# change variable type to character so the join works.
class_dict$class_code <- as.character(class_dict$class_code)

# use the property class to make the major property types
# joins the class_dict file to the pin_data classes
exemption_data <- class_dict %>%
  right_join(exemption_data, by = c("class_code" = "class"))


## summarize pin level data to the tax code level for each type of property class
exemptions_inCook_perTC <- exemption_data %>%
  group_by(tax_code_num, class_code
           #, major_class_code, major_class_type
  ) %>%
  summarize(av = sum(av, na.rm = TRUE),
            eav=sum(eav, na.rm=TRUE),
            all_exemptions = sum(all_exemptions),
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

exemptions_inCook_perTC <- exemptions_inCook_perTC %>%
  left_join(tc_muninames)


## Add exemption types and values to the tax bill data at PIN level
joined_pin_data <- left_join(pin14_bills_2006, exemption_data, by = c("pin", "class" = "class_code" ))  %>%
  rename(av = av.x,
         eav =  eav.x,
         equalized_av = eav.y)

## Add tax code tax rate to PIN level data
pin_data <- left_join(joined_pin_data, tc_muninames, by = c("tax_code" = "tax_code_num"))

head(pin_data)

#write_csv(pin_data, "joined_pin_bills_and_exemps_2006.csv")



## 2006 Composite Tax Rates for Municipalities
muni_taxrates <- pin_data %>%
  group_by(clean_name)  %>%

  summarize(
    av = sum(av, na.rm = TRUE),
    eav = sum(eav, na.rm = TRUE),
    equalized_av = sum(equalized_av, na.rm = TRUE),
    pins_in_muni = n(),
    all_exemptions = sum(all_exemptions, na.rm = TRUE),
    HO_exemps = sum(exe_homeowner, na.rm = TRUE),
    tax_code_rate = mean(tax_code_rate, na.rm = TRUE), # Changed from first() to mean() on Nov 1
    final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE), # used as LEVY amount!!
    final_tax_to_tif = sum(final_tax_to_tif, na.rm = TRUE),
    tax_amt_exe = sum(tax_amt_exe, na.rm = TRUE),
    tax_amt_pre_exe = sum(tax_amt_pre_exe, na.rm = TRUE),
    tax_amt_post_exe = sum(tax_amt_post_exe, na.rm = TRUE),
    rpm_tif_to_cps = sum(rpm_tif_to_cps, na.rm = TRUE), # not used
    rpm_tif_to_rpm = sum(rpm_tif_to_rpm, na.rm=TRUE), # not used
    rpm_tif_to_dist = sum(rpm_tif_to_dist, na.rm=TRUE), # not used
    tif_share = mean(tif_share, na.rm=TRUE), # not used
  ) %>%

  mutate(total_bill_current = final_tax_to_dist + final_tax_to_tif) %>%
  rename(cur_comp_TC_rate = tax_code_rate) %>%
  mutate(current_taxable_eav = final_tax_to_dist/(cur_comp_TC_rate/100),
         new_taxable_eav = final_tax_to_dist/(cur_comp_TC_rate/100) + HO_exemps) %>%
  mutate(new_comp_TC_rate = (final_tax_to_dist / new_taxable_eav)*100) %>%
  mutate(new_comp_TC_rate = ifelse(is.nan(new_comp_TC_rate), cur_comp_TC_rate, new_comp_TC_rate)) %>%
  select(clean_name, cur_comp_TC_rate, new_comp_TC_rate, current_taxable_eav, new_taxable_eav, everything())



tc_mc_summaries <- pin_data %>%
  group_by(tax_code_num, major_class_code) %>%
  summarize(TC_MC_PC = n(),
            TC_MC_AV = sum(av),
            TC_MC_EAV = sum(eav),
            Total_Exemptions = sum(exe_homeowner+exe_senior +exe_freeze + exe_longtime_homeowner +exe_disabled + exe_vet_returning + exe_vet_dis_lt50+ exe_vet_dis_50_69 + exe_vet_dis_ge70 + exe_abate, na.rm = TRUE),
            GHE_only = sum(exe_homeowner, na.rm = TRUE),
            ) %>%
  left_join(tc_muninames)


tc_class_summaries <- pin_data %>%
  group_by(tax_code_num, major_class_code, class) %>%
  summarize(TC_class_PC = n(),
            TC_class_AV = sum(av),
            TC_class_EAV = sum(eav),
            Total_Exemptions = sum(exe_homeowner+exe_senior +exe_freeze + exe_longtime_homeowner +exe_disabled + exe_vet_returning + exe_vet_dis_lt50+ exe_vet_dis_50_69 + exe_vet_dis_ge70 + exe_abate, na.rm = TRUE),
            GHE_only = sum(exe_homeowner, na.rm = TRUE)) %>%
  left_join(tc_muninames)



prop_class_sums <- pin_data %>%
  group_by(clean_name, major_class_code, major_class_type )  %>%
  summarize(
    av = sum(av, na.rm = TRUE),
    eav = sum(eav, na.rm = TRUE),
    equalized_AV = sum(equalized_av, na.rm = TRUE),
    pins_in_class = n(),
    current_exemptions = sum(all_exemptions, na.rm = TRUE),
    HO_exemps = sum(exe_homeowner, na.rm = TRUE),
    tax_code_rate = mean(tax_code_rate, na.rm = TRUE), # Changed from first() to mean() on Nov 1
    final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE), # used as LEVY amount!!
    final_tax_to_tif = sum(final_tax_to_tif, na.rm = TRUE),
    tax_amt_exe = sum(tax_amt_exe, na.rm = TRUE),
    tax_amt_pre_exe = sum(tax_amt_pre_exe, na.rm = TRUE),
    tax_amt_post_exe = sum(tax_amt_post_exe, na.rm = TRUE),
    rpm_tif_to_cps = sum(rpm_tif_to_cps, na.rm = TRUE), # not used
    rpm_tif_to_rpm = sum(rpm_tif_to_rpm, na.rm=TRUE), # not used
    rpm_tif_to_dist = sum(rpm_tif_to_dist, na.rm=TRUE), # not used
    tif_share = mean(tif_share, na.rm=TRUE), # not used
  ) %>%

  mutate(total_bill_current = final_tax_to_dist + final_tax_to_tif) %>%
  rename(cur_comp_TC_rate = tax_code_rate) %>%
  mutate(current_taxable_eav = final_tax_to_dist/(cur_comp_TC_rate/100),
         new_taxable_eav = final_tax_to_dist/(cur_comp_TC_rate/100) + HO_exemps) %>%
  mutate(new_comp_TC_rate = (final_tax_to_dist / new_taxable_eav)*100) %>%
  mutate(new_comp_TC_rate = ifelse(is.nan(new_comp_TC_rate), cur_comp_TC_rate, new_comp_TC_rate),
         year = "2006") %>%
  select(clean_name, major_class_code, HO_exemps, current_exemptions, pins_in_class, current_taxable_eav, new_taxable_eav,  everything())

prop_class_sums2 <- prop_class_sums %>%
  group_by(clean_name) %>%
  mutate(muni_PC = sum(pins_in_class,na.rm=TRUE),
         muni_taxable_eav = sum(current_taxable_eav, na.rm=TRUE),
         muni_equalized_av = sum(equalized_AV, na.rm=TRUE),
         muni_av = sum(av, na.rm=TRUE),
         pct_pins = pins_in_class / muni_PC,
         pct_taxable_eav = current_taxable_eav / muni_taxable_eav,
         pct_eq_eav = equalized_AV / muni_equalized_av,
         pct_av = av / muni_av,
         year = "2006"
  ) %>%
  mutate_at(vars(pct_pins, pct_taxable_eav, pct_eq_eav, pct_av), funs(round(.,3)))

write_csv(prop_class_sums2, "./Output/all_years_pull/TC_MC_2006.csv")


##### Summarized by Class-TC combos ######

prop_class_sums <- pin_data %>%
  group_by(clean_name, class)  %>%

  summarize(
    av = sum(av, na.rm = TRUE),
    eav = sum(eav, na.rm = TRUE),
    equalized_AV = sum(equalized_av, na.rm = TRUE),
    pins_in_class = n(),
    current_exemptions = sum(all_exemptions, na.rm = TRUE),
    HO_exemps = sum(exe_homeowner, na.rm = TRUE),
    tax_code_rate = mean(tax_code_rate, na.rm = TRUE), # Changed from first() to mean() on Nov 1
    final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE), # used as LEVY amount!!
    final_tax_to_tif = sum(final_tax_to_tif, na.rm = TRUE),
    tax_amt_exe = sum(tax_amt_exe, na.rm = TRUE),
    tax_amt_pre_exe = sum(tax_amt_pre_exe, na.rm = TRUE),
    tax_amt_post_exe = sum(tax_amt_post_exe, na.rm = TRUE),
    rpm_tif_to_cps = sum(rpm_tif_to_cps, na.rm = TRUE), # not used
    rpm_tif_to_rpm = sum(rpm_tif_to_rpm, na.rm=TRUE), # not used
    rpm_tif_to_dist = sum(rpm_tif_to_dist, na.rm=TRUE), # not used
    tif_share = mean(tif_share, na.rm=TRUE), # not used
  ) %>%

  mutate(total_bill_current = final_tax_to_dist + final_tax_to_tif) %>%
  rename(cur_comp_TC_rate = tax_code_rate) %>%
  mutate(current_taxable_eav = final_tax_to_dist/(cur_comp_TC_rate/100),
         new_taxable_eav = final_tax_to_dist/(cur_comp_TC_rate/100) + HO_exemps) %>%
  mutate(new_comp_TC_rate = (final_tax_to_dist / new_taxable_eav)*100) %>%
  mutate(new_comp_TC_rate = ifelse(is.nan(new_comp_TC_rate), cur_comp_TC_rate, new_comp_TC_rate),
         year = "2006") %>%
  select(clean_name, class, HO_exemps, current_exemptions, pins_in_class, current_taxable_eav, new_taxable_eav, everything())



prop_class_sums2 <- prop_class_sums %>%
  group_by(clean_name) %>%
  mutate(muni_PC = sum(pins_in_class,na.rm=TRUE),
         muni_taxable_eav = sum(current_taxable_eav, na.rm=TRUE),
         muni_equalized_av = sum(equalized_AV, na.rm=TRUE),
         muni_av = sum(av, na.rm=TRUE),
         pct_pins = pins_in_class / muni_PC,
         pct_taxable_eav = current_taxable_eav / muni_taxable_eav,
         pct_eq_eav = equalized_AV / muni_equalized_av,
         pct_av = av / muni_av,
         year = "2006"
  ) %>%
  mutate_at(vars(pct_pins, pct_taxable_eav, pct_eq_eav, pct_av), funs(round(.,3)))

write_csv(prop_class_sums2, "./Output/all_years_pull/TC_class_2006.csv")



#### 2007 Data Pull ####



#### 2008 Data Pull ####
