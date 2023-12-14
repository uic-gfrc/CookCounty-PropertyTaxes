# File Prep ----------------------------------------
## AWM & MVH ##

library(tidyverse)
library(DBI)
library(data.table)
library(gstat)
library(here)
library(httr)
library(jsonlite)
library(ptaxsim)
library(stars)
library(glue)

# Create the DB connection with the default name expected by PTAXSIM functions
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2021.0.4.db")

class_dict <- read_csv("./Necessary_Files/class_dict.csv")
nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")
years <-(2006:2007)


muni_level_summary <- NULL
muni_MC_summary <- NULL
muni_class_summary <- NULL

tc_mc_summaries <- NULL
tc_class_summaries <- NULL

for(i in years){

  year_variable = i

# PTAXSIM tables ---------------------------------------
  
  ## Municipality taxing agencies only + Cicero
  muni_agency_names <- DBI::dbGetQuery(
    ptaxsim_db_conn,
    "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'  
    "
  )

  
  agency_dt<- dbGetQuery(ptaxsim_db_conn, paste('SELECT * FROM agency WHERE year = ', i, ';'))
  tax_codes <- dbGetQuery(ptaxsim_db_conn, paste('SELECT DISTINCT tax_code_num, tax_code_rate FROM tax_code WHERE year = ', i, ';'))
  

  
  sql <- "SELECT * FROM tax_code WHERE agency_num IN ({muni_agency_names$agency_num*}) AND year = ?year"
  query <- sqlInterpolate(ptaxsim_db_conn, sql, year = i)
  muni_tax_codes<- dbGetQuery(ptaxsim_db_conn, glue_sql(query, .con = ptaxsim_db_conn))
  

  
  
  ## All tax codes. 
  ## tax codes within municipalities have additional info 
  tc_muninames <- tax_codes %>% 
    left_join(muni_tax_codes) %>%
    left_join(muni_agency_names) %>% 
    select(-agency_rate) %>% 
    left_join(nicknames) %>% 
    select(-c(minor_type, short_name, `Column1`, `Most recent reassessed`, agency_number))
  

  
  
  sql <- "SELECT DISTINCT pin, class, tax_code_num FROM pin WHERE tax_code_num IN ({tax_codes$tax_code_num*}) AND year = ?year"
  query <- sqlInterpolate(ptaxsim_db_conn, sql, year = i)
  cook_pins <- dbGetQuery(ptaxsim_db_conn, glue_sql(query, .con = ptaxsim_db_conn))
  
  
  
  # combine taxing agency names and agency type to data table that has eav and extension values
  agency_data <- right_join(agency_dt, muni_agency_names) %>% 
    select(-c(cty_dupage_eav:cty_livingston_eav)) %>% # drop some of the unused variables
    arrange(agency_num)
  
  
  

# Tax Bills ---------------------------------------------------------------

  
  # pin-taxing agency combinations for tax bills
  taxbills <- tax_bill(year_variable,  cook_pins$pin, 
                               #  pin_dt = exe_dt, # default option, change for "no exemption" simulation
                               simplify = FALSE)
  
  
  
  #  billed properties with 14-digit PINs  
  pin14_bills <- taxbills %>%
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
  
  rm(taxbills)

    

# Exemptions at PIN level --------------------------------------------------

  
  # finds all pins within Cook county and data on their exemptions
  # joins tax code variable by pin
  exemption_data <- lookup_pin(i, cook_pins$pin) %>%
    left_join(cook_pins, by = c("pin", "class")) %>%
    mutate(all_exemptions = exe_homeowner + exe_senior + exe_freeze + exe_longtime_homeowner + 
             exe_disabled + exe_vet_returning + exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70 ,
           abatements = exe_abate) %>%
    mutate(zero_bill = ifelse(eav <= all_exemptions, 1, 0),
           has_HO_exemp = ifelse(exe_homeowner > 0, 1, 0))
  
  rm(cook_pins)
  
  
  # change variable type to character so the join works.
  class_dict$class_code <- as.character(class_dict$class_code)
  
  # use the property class to make the major property types
  # joins the class_dict file to the pin_data classes
  exemption_data <- class_dict %>%
    right_join(exemption_data, by = c("class_code" = "class"), 
               relationship = "many-to-many")
  

# Summarize PIN Exemptions ------------------------------------------------

  ## summarize pin level data to the tax code level for each type of property class
  exemptions_inCook_perTC <- exemption_data %>%
    group_by(tax_code_num, class_code) %>%
    summarize(year = first(year), 
              av = sum(av, na.rm = TRUE),
              eav=sum(eav, na.rm=TRUE),
              all_exemptions = sum(all_exemptions),
              exe_homeowner = sum(exe_homeowner, na.rm=TRUE),
              exe_senior = sum(exe_senior, na.rm=TRUE),
              exe_freeze = sum(exe_freeze, na.rm=TRUE),
              exe_longtime_homeowner = sum(exe_longtime_homeowner, na.rm=TRUE),
              exe_disabled = sum(exe_disabled, na.rm=TRUE),
              exe_vet_returning = sum(exe_vet_returning, na.rm=TRUE),
              exe_vet_dis_lt50 = sum(exe_vet_dis_lt50, na.rm=TRUE),
              exe_vet_dis_50_69 = sum(exe_vet_dis_50_69, na.rm=TRUE),
              exe_vet_dis_ge70 = sum(exe_vet_dis_ge70, na.rm=TRUE),
              exe_vet_dis = sum(exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70, na.rm=TRUE),
              exe_abate = sum(exe_abate, na.rm=TRUE),
              pin_count = n() # number of pins within each tax code and property combo
              
    )
  
  
  exemptions_inCook_perTC <- exemptions_inCook_perTC %>% 
    left_join(tc_muninames)
  
  
# Join Bills and Exemptions  ----------------------------------------------

  ## Add exemption types and values to the tax bill data at PIN level
  joined_pin_data <- left_join(pin14_bills, exemption_data, by = c("pin", "class" = "class_code" ))  %>%
    rename(av = av.x,
           eav =  eav.x,
           equalized_av = eav.y)
  
  rm(exemption_data)
  
  
  ## Add tax code tax rate to PIN level data
  joined_pin_data <- left_join(joined_pin_data, tc_muninames, by = c("tax_code" = "tax_code_num"))
  
  

# Summarize ---------------------------------------------------------------


## Muni Level --------------------------------------------------------------


### Composite Tax Rates for Municipalities ---------------------------------

  muni_level_summary2 <- joined_pin_data %>%
    group_by(clean_name)  %>%

    summarize(
      av = sum(av, na.rm = TRUE),
      eav = sum(eav, na.rm = TRUE),
      equalized_av = sum(equalized_av, na.rm = TRUE),
      pins_in_muni = n(),
      all_exemptions = sum(all_exemptions, na.rm = TRUE),
      HO_exemps = sum(exe_homeowner, na.rm = TRUE),
      tax_code_rate = mean(tax_code_rate, na.rm = TRUE),          # Changed from first() to mean() on Nov 1
      final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE),   # used as LEVY amount!!
      final_tax_to_tif = sum(final_tax_to_tif, na.rm = TRUE),     # TIF increment
      tax_amt_exe = sum(tax_amt_exe, na.rm = TRUE),
      tax_amt_pre_exe = sum(tax_amt_pre_exe, na.rm = TRUE),
      tax_amt_post_exe = sum(tax_amt_post_exe, na.rm = TRUE),
      rpm_tif_to_cps = sum(rpm_tif_to_cps, na.rm = TRUE), # not used
      rpm_tif_to_rpm = sum(rpm_tif_to_rpm, na.rm=TRUE),   # not used
      rpm_tif_to_dist = sum(rpm_tif_to_dist, na.rm=TRUE), # not used
      tif_share = mean(tif_share, na.rm=TRUE),
    ) %>%
    mutate(total_bill_current = final_tax_to_dist + final_tax_to_tif) %>%
    rename(cur_comp_muni_rate = tax_code_rate) %>%
    mutate(current_taxable_eav = final_tax_to_dist/(cur_comp_muni_rate/100),
           new_taxable_eav = final_tax_to_dist/(cur_comp_muni_rate/100) + HO_exemps) %>%
    mutate(new_comp_muni_rate = (final_tax_to_dist / new_taxable_eav)*100) %>%
    mutate(new_comp_muni_rate = ifelse(is.na(new_comp_muni_rate), cur_comp_muni_rate, new_comp_muni_rate),
           year = year_variable) %>%
    select(year, clean_name, cur_comp_muni_rate, new_comp_muni_rate, current_taxable_eav, new_taxable_eav, everything())


  # bind muni level yearly data together
  if(is.data.frame(muni_level_summary)){muni_level_summary <- rbind(muni_level_summary, muni_level_summary2)}else{muni_level_summary <- muni_level_summary2}
  rm(muni_level_summary2)


### Muni-MC Summary ---------------------------------------------------------


  muni_MC_summary2 <- joined_pin_data %>%
    group_by(clean_name, major_class_code)  %>%

    summarize(
      av = sum(av, na.rm = TRUE),
      eav = sum(eav, na.rm = TRUE),
      equalized_av = sum(equalized_av, na.rm = TRUE),
      pins_in_muni = n(),
      all_exemptions = sum(all_exemptions, na.rm = TRUE),
      HO_exemps = sum(exe_homeowner, na.rm = TRUE),
      tax_code_rate = mean(tax_code_rate, na.rm = TRUE),          # Changed from first() to mean() on Nov 1
      final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE),   # used as LEVY amount!!
      final_tax_to_tif = sum(final_tax_to_tif, na.rm = TRUE),     # TIF increment
      tax_amt_exe = sum(tax_amt_exe, na.rm = TRUE),
      tax_amt_pre_exe = sum(tax_amt_pre_exe, na.rm = TRUE),
      tax_amt_post_exe = sum(tax_amt_post_exe, na.rm = TRUE),
      rpm_tif_to_cps = sum(rpm_tif_to_cps, na.rm = TRUE), # not used
      rpm_tif_to_rpm = sum(rpm_tif_to_rpm, na.rm=TRUE),   # not used
      rpm_tif_to_dist = sum(rpm_tif_to_dist, na.rm=TRUE), # not used
      tif_share = mean(tif_share, na.rm=TRUE),
    ) %>%

    mutate(total_bill_current = final_tax_to_dist + final_tax_to_tif) %>%
    rename(cur_comp_muni_rate = tax_code_rate) %>%
    mutate(current_taxable_eav = final_tax_to_dist/(cur_comp_muni_rate/100),
           new_taxable_eav = final_tax_to_dist/(cur_comp_muni_rate/100) + HO_exemps) %>%
    mutate(new_comp_muni_rate = (final_tax_to_dist / new_taxable_eav)*100) %>%
    mutate(new_comp_muni_rate = ifelse(is.nan(new_comp_muni_rate), cur_comp_muni_rate, new_comp_muni_rate),
           year = year_variable
          ) %>%
    select(year, clean_name, major_class_code, cur_comp_muni_rate, new_comp_muni_rate, current_taxable_eav, new_taxable_eav, everything())


  # bind muni level yearly data together
  if(is.data.frame(muni_MC_summary)){muni_MC_summary <- rbind(muni_MC_summary, muni_MC_summary2)}else{muni_MC_summary <- muni_MC_summary2}
  rm(muni_MC_summary2)



### Muni-Class Summary ---------------------------------------------------------


  muni_class_summary2 <- joined_pin_data %>%
    group_by(clean_name, class)  %>%

    summarize(
      av = sum(av, na.rm = TRUE),
      eav = sum(eav, na.rm = TRUE),
      equalized_av = sum(equalized_av, na.rm = TRUE),
      pins_in_muni = n(),
      all_exemptions = sum(all_exemptions, na.rm = TRUE),
      HO_exemps = sum(exe_homeowner, na.rm = TRUE),
      tax_code_rate = mean(tax_code_rate, na.rm = TRUE),          # Changed from first() to mean() on Nov 1
      final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE),   # used as LEVY amount!!
      final_tax_to_tif = sum(final_tax_to_tif, na.rm = TRUE),     # TIF increment
      tax_amt_exe = sum(tax_amt_exe, na.rm = TRUE),
      tax_amt_pre_exe = sum(tax_amt_pre_exe, na.rm = TRUE),
      tax_amt_post_exe = sum(tax_amt_post_exe, na.rm = TRUE),
      rpm_tif_to_cps = sum(rpm_tif_to_cps, na.rm = TRUE), # not used
      rpm_tif_to_rpm = sum(rpm_tif_to_rpm, na.rm=TRUE),   # not used
      rpm_tif_to_dist = sum(rpm_tif_to_dist, na.rm=TRUE), # not used
      tif_share = mean(tif_share, na.rm=TRUE),
    ) %>%

    mutate(total_bill_current = final_tax_to_dist + final_tax_to_tif) %>%
    rename(cur_comp_muni_rate = tax_code_rate) %>%
    mutate(current_taxable_eav = final_tax_to_dist/(cur_comp_muni_rate/100),
           new_taxable_eav = final_tax_to_dist/(cur_comp_muni_rate/100) + HO_exemps) %>%
    mutate(new_comp_muni_rate = (final_tax_to_dist / new_taxable_eav)*100 ,
           year = year_variable) %>%
    mutate(new_comp_muni_rate = ifelse(is.nan(new_comp_muni_rate), cur_comp_muni_rate, new_comp_muni_rate)) %>%
    select(year, clean_name, class, cur_comp_muni_rate, new_comp_muni_rate, current_taxable_eav, new_taxable_eav, everything())


  # bind muni level yearly data together
  if(is.data.frame(muni_class_summary)){muni_class_summary <- rbind(muni_class_summary, muni_class_summary2)}else{muni_class_summary <- muni_class_summary2}
  rm(muni_class_summary2)


# ## Tax Code Level Summaries -------------------------------------------------
  
### Tax code-Major Class Summaries ------------------------------------------
  tc_mc_summaries2 <- joined_pin_data %>%
    group_by(tax_code_num, major_class_code) %>%
    summarize(
              TC_MC_PC = n(),
              TC_MC_AV = sum(av),
              TC_MC_EAV = sum(eav),
              Total_Exemptions = sum(exe_homeowner+exe_senior +exe_freeze + 
                                       exe_longtime_homeowner + exe_disabled + 
                                       exe_vet_returning + exe_vet_dis_lt50 + 
                                       exe_vet_dis_50_69 + exe_vet_dis_ge70 + 
                                       exe_abate, na.rm = TRUE),
              GHE_only = sum(exe_homeowner, na.rm = TRUE)
              ) %>% 
    left_join(tc_muninames) %>%
    mutate(year = year_variable) %>%
    select(year, clean_name, tax_code_num, major_class_code, everything())

# bind yearly data together
  if(is.data.frame(tc_mc_summaries)){tc_mc_summaries <- rbind(tc_mc_summaries, tc_mc_summaries2)}else{tc_mc_summaries <- tc_mc_summaries2}
  rm(tc_mc_summaries2)



### Taxcode-Class Summaries ----------------------------------------------
  tc_class_summaries2 <- joined_pin_data %>%
  group_by(tax_code_num, class) %>%
    summarize(
              TC_MC_PC = n(),
              TC_MC_AV = sum(av),
              TC_MC_EAV = sum(eav),
              Total_Exemptions = sum(exe_homeowner+exe_senior +exe_freeze + exe_longtime_homeowner +exe_disabled + exe_vet_returning + exe_vet_dis_lt50+ exe_vet_dis_50_69 + exe_vet_dis_ge70 + exe_abate, na.rm = TRUE),
              GHE_only = sum(exe_homeowner, na.rm = TRUE),
    ) %>%
    left_join(tc_muninames) %>%
    mutate(year = year_variable) %>%
    select(year, clean_name, tax_code_num, class, everything())



  # bind yearly data together
  if(is.data.frame(tc_class_summaries)){tc_class_summaries <- rbind(tc_class_summaries, tc_class_summaries2)}else{tc_class_summaries <- tc_class_summaries2}
  rm(tc_class_summaries2)


} 

# Export CSVs ------------------------------------------------------------

write_csv(muni_level_summary, "./Output/ptaxsim_muni_level_2006-2021.csv")
write_csv(muni_MC_summary, "./Output/ptaxsim_muni_MC_2006-2021.csv")
write_csv(muni_class_summary, "./Output/ptaxsim_muni_class_summaries_2006-2021.csv")


write_csv(tc_mc_summaries, "./Output/ptaxsim_TC_MC_summaries_2006-2021.csv")
write_csv(tc_class_summaries, "./Output/ptaxsim_TC_Class_summaries_2006-2021.csv")


