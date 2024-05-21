# Loop for summarizing Assessor Neighborhood variables 

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


# Create the DB connection with the default name expected by PTAXSIM functions ---------

# AWM filepath:
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

# MVH filepath:
# ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "ptaxsim.db")

# Load supplemental files w/ "clean" muni names and detail re: class codes.

class_dict <- read_csv("./Necessary_Files/class_dict_expanded.csv")
ccao_loa <- read_csv("./Inputs/ccao_loa.csv") %>% 
  mutate(
  class_code = as.character(class_code)
)

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")

# Set years for loop to run.

years <-(2022)

# Create empty dataframes for the loop to populate.


nbh_MC_summary <- NULL


# Loop Start --------------------------------------------------------------



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
  
  
  # has eav values for each tax code
  tif_distrib <- DBI::dbGetQuery(
    ptaxsim_db_conn, paste(

  'SELECT *
  FROM tif_distribution
  WHERE year = ', i, ';')
          
    )
  
  
  ## All tax codes. 
  ## tax codes within municipalities have additional info 
  tc_muninames <- tax_codes %>% 
    mutate(tif_taxcode = ifelse(tax_code_num %in% tif_distrib$tax_code_num, 1, 0)) %>%
    left_join(muni_tax_codes) %>%
    left_join(muni_agency_names) %>% 
    select(-agency_rate) %>% 
    left_join(nicknames) %>% 
    select(-c(minor_type, short_name, 
              agency_number))
  
  
  
  
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
    left_join(ccao_loa, by = c("year", "class" = "class_code")) %>%
    
    mutate(all_exemptions = exe_homeowner + exe_senior + exe_freeze + exe_longtime_homeowner + 
             exe_disabled + exe_vet_returning + exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70 ,
           abatements = exe_abate,
           fmv = av / loa,
           parcel = str_sub(pin, 1, 10)) %>%
   
    mutate(zero_bill = ifelse(eav <= all_exemptions, 1, 0),
           has_HO_exemp = ifelse(exe_homeowner > 0, 1, 0),
           has_SF_exemp = ifelse(exe_senior > 0, 1, 0),
           has_FR_exemp = ifelse(exe_freeze > 0, 1, 0),
           has_LTHO_exemp = ifelse(exe_longtime_homeowner > 0, 1, 0),
           has_DI_exemp = ifelse(exe_disabled > 0, 1, 0),
           has_VR_exemp = ifelse(exe_vet_returning > 0, 1, 0),
           has_DV_exemp = ifelse(exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70 > 0, 1, 0),
           has_AB_exemp = ifelse(exe_abate > 0, 1, 0),
           in_tif = ifelse(tax_code_num %in% tif_distrib$tax_code_num, 1, 0)
    )
  
  
  rm(cook_pins)
  
  
  # change variable type to character so the join works.
  class_dict$class_code <- as.character(class_dict$class_code)
  
  # use the property class to make the major property types
  # joins the class_dict file to the pin_data classes
  exemption_data <- class_dict %>%
    right_join(exemption_data, by = c("class_code" = "class"), 
               relationship = "many-to-many")
  
  
  # Summarize PIN Exemptions ------------------------------------------------
  
  # ## summarize pin level data to the tax code level for each type of property class
  # exemptions_inCook_perTC <- exemption_data %>%
  #   group_by(tax_code_num, class_code) %>%
  #   summarize(year = first(year), 
  #             av = sum(av, na.rm = TRUE),
  #             eav=sum(eav, na.rm=TRUE),
  #             fmv = sum(fmv, na.rm=TRUE),
  #             pin_count = n(),  # number of pins within each tax code and property combo
  #             parcel_count = n_distinct(parcel),
  #             pins_inTIF = sum(in_tif),
  #             
  #             all_exemptions = sum(all_exemptions),
  #             
  #             exe_homeowner = sum(exe_homeowner, na.rm=TRUE),
  #             exe_senior = sum(exe_senior, na.rm=TRUE),
  #             exe_freeze = sum(exe_freeze, na.rm=TRUE),
  #             exe_longtime_homeowner = sum(exe_longtime_homeowner, na.rm=TRUE),
  #             exe_disabled = sum(exe_disabled, na.rm=TRUE),
  #             exe_vet_returning = sum(exe_vet_returning, na.rm=TRUE),
  #             exe_vet_dis_lt50 = sum(exe_vet_dis_lt50, na.rm=TRUE),
  #             exe_vet_dis_50_69 = sum(exe_vet_dis_50_69, na.rm=TRUE),
  #             exe_vet_dis_ge70 = sum(exe_vet_dis_ge70, na.rm=TRUE),
  #             exe_vet_dis = sum(exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70, na.rm=TRUE),
  #             exe_abate = sum(exe_abate, na.rm=TRUE),
  #             
  #             zero_bills = sum(zero_bill),
  #             has_HO_exemp = sum(has_HO_exemp),
  #             has_SF_exemp = sum(has_SF_exemp),
  #             has_FR_exemp = sum(has_FR_exemp),
  #             has_LTHO_exemp = sum(has_LTHO_exemp),
  #             has_DI_exemp = sum(has_DI_exemp),
  #             has_VR_exemp = sum(has_VR_exemp),
  #             has_DV_exemp = sum(has_DV_exemp),
  #             has_AB_exemp = sum(has_AB_exemp),
  #             year = year_variable
  #   )
  # 
  # 
  # exemptions_inCook_perTC <- exemptions_inCook_perTC %>% 
  #   left_join(tc_muninames)
  
  
  # Join Bills and Exemptions  ----------------------------------------------
  
  ## Add exemption types and values to the tax bill data at PIN level
  joined_pin_data <- left_join(pin14_bills, exemption_data, by = c("pin", "class" = "class_code" ))  %>%
    rename(av = av.x,
           eav =  eav.x,
           equalized_av = eav.y)
  
  rm(exemption_data)
  
  
  ## Add tax code tax rate to PIN level data
  joined_pin_data <- left_join(joined_pin_data, tc_muninames, by = c("tax_code" = "tax_code_num"))
  

# Assessor Neighborhoods --------------------------------------------------

base_url <- "https://datacatalog.cookcountyil.gov/resource/tx2p-k2g9.json"

nbh_pins <- GET(
  base_url,
  query = list(
    tax_year = year_variable,
    `$select` = paste0(c("pin", # "pin10", 
                         "class", 
                         "township_code", "township_name",
                         "nbhd_code", "census_puma_geoid",
                         "triad_name" 
    ),
    collapse = ","),
    `$limit` = 500000000L
  )
)

nbh_pins2 <- fromJSON(rawToChar(nbh_pins$content))
head(nbh_pins2)



nbh_MC_summary <-  joined_pin_data %>%
  left_join(nbh_pins2) %>%
  mutate(nbhd_3 = str_sub(nbhd_code, 3,5)) %>%
  
  group_by(nbhd_code, major_class_code, nbhd_3, triad_name, township_name, township_code) %>%
  filter(!is.na(nbhd_code)) %>%
  
  summarize(nbh_final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE), # amount billed by munis with current exemptions in place
            nbh_tif_rev = sum(final_tax_to_tif, na.rm=TRUE),
            nbh_nonTIF_EAV_post_exemps = sum(final_tax_to_dist/(tax_code_rate/100), na.rm = TRUE),
            nbh_TIF_increment_EAV = sum(final_tax_to_tif/(tax_code_rate/100), na.rm=TRUE),  
            nbh_exe_and_abates_frombills = sum(tax_amt_exe/(tax_code_rate/100), na.rm=TRUE),  # Combines all exemptions and abatements!
            Total_EAV = sum((tax_amt_exe + final_tax_to_dist + final_tax_to_tif)/(tax_code_rate/100), na.rm = TRUE),
            
            nbh_MC_PC = n(),
            nbh_MC_parcels = n_distinct(parcel),
            nbh_MC_TIF_PC = sum(in_tif),
            #nbh_MC_TIF_parcels = sum(ifelse((in_tif ==1 & n_distinct(parcel)), 1,0)),
            
            nbh_MC_AV = sum(av),
            nbh_MC_EAV = sum(eav),
            nbh_MC_FMV = sum(fmv, na.rm=TRUE),
            nbh_MC_res_exe = sum(exe_homeowner + exe_senior + exe_freeze + 
                                  exe_longtime_homeowner + exe_disabled + 
                                  exe_vet_returning + exe_vet_dis_lt50 + 
                                  exe_vet_dis_50_69 + exe_vet_dis_ge70, na.rm = TRUE),
            nbh_mc_exe_homeowner = sum(exe_homeowner, na.rm=TRUE),
            nbh_mc_exe_senior = sum(exe_senior, na.rm=TRUE),
            nbh_mc_exe_freeze = sum(exe_freeze, na.rm=TRUE),
            nbh_mc_exe_longtime_homeowner = sum(exe_longtime_homeowner, na.rm=TRUE),
            nbh_mc_exe_disabled = sum(exe_disabled, na.rm=TRUE),
            nbh_mc_exe_vet_returning = sum(exe_vet_returning, na.rm=TRUE),
            nbh_mc_exe_vet_dis_lt50 = sum(exe_vet_dis_lt50, na.rm=TRUE),
            nbh_mc_exe_vet_dis_50_69 = sum(exe_vet_dis_50_69, na.rm=TRUE),
            nbh_mc_exe_vet_dis_ge70 = sum(exe_vet_dis_ge70, na.rm=TRUE),
            
            nbh_mc_exe_abate = sum(exe_abate, na.rm=TRUE),
            
            zero_bills = sum(zero_bill),
            has_HO_exemp = sum(has_HO_exemp),
            has_SF_exemp = sum(has_SF_exemp),
            has_FR_exemp = sum(has_FR_exemp),
            has_LTHO_exemp = sum(has_LTHO_exemp),
            has_DI_exemp = sum(has_DI_exemp),
            has_VR_exemp = sum(has_VR_exemp),
            has_DV_exemp = sum(has_DV_exemp),
            has_AB_exemp = sum(has_AB_exemp),
            year = year_variable
  ) %>%
  
  mutate(tax_rate_current = nbh_final_tax_to_dist/nbh_nonTIF_EAV_post_exemps) %>% 
  
  select(year, nbhd_code, nbh_current_rate=tax_rate_current, everything()) %>% 
  arrange(desc(nbhd_code))


nbh_summary <-  joined_pin_data %>%
  left_join(nbh_pins2) %>%
  mutate(nbhd_3 = str_sub(nbhd_code, 3,5)) %>%
  
  group_by(nbhd_code, nbhd_3, triad_name, township_name, township_code) %>%
  filter(!is.na(nbhd_code)) %>%
  
  summarize(nbh_final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE), # amount billed by munis with current exemptions in place
            nbh_tif_rev = sum(final_tax_to_tif, na.rm=TRUE),
            nbh_nonTIF_EAV_post_exemps = sum(final_tax_to_dist/(tax_code_rate/100), na.rm = TRUE),
            nbh_TIF_increment_EAV = sum(final_tax_to_tif/(tax_code_rate/100), na.rm=TRUE),  
            nbh_exe_and_abates_frombills = sum(tax_amt_exe/(tax_code_rate/100), na.rm=TRUE),  # Combines all exemptions and abatements!
            Total_EAV = sum((tax_amt_exe + final_tax_to_dist + final_tax_to_tif)/(tax_code_rate/100), na.rm = TRUE),
            
            nbh_PC = n(),
            nbh_parcels = n_distinct(parcel),
            nbh_TIF_PC = sum(in_tif),
            nbh_AV = sum(av),
            nbh_EAV = sum(eav),
            nbh_FMV = sum(fmv),
            nbh_res_exe = sum(exe_homeowner + exe_senior + exe_freeze + 
                                exe_longtime_homeowner + exe_disabled + 
                                exe_vet_returning + exe_vet_dis_lt50 + 
                                exe_vet_dis_50_69 + exe_vet_dis_ge70, na.rm = TRUE),
            nbh_exe_homeowner = sum(exe_homeowner, na.rm=TRUE),
            nbh_exe_senior = sum(exe_senior, na.rm=TRUE),
            nbh_exe_freeze = sum(exe_freeze, na.rm=TRUE),
            nbh_exe_longtime_homeowner = sum(exe_longtime_homeowner, na.rm=TRUE),
            nbh_exe_disabled = sum(exe_disabled, na.rm=TRUE),
            nbh_exe_vet_returning = sum(exe_vet_returning, na.rm=TRUE),
            nbh_exe_vet_dis_lt50 = sum(exe_vet_dis_lt50, na.rm=TRUE),
            nbh_exe_vet_dis_50_69 = sum(exe_vet_dis_50_69, na.rm=TRUE),
            nbh_exe_vet_dis_ge70 = sum(exe_vet_dis_ge70, na.rm=TRUE),
            
            nbh_exe_abate = sum(exe_abate, na.rm=TRUE),
            
            zero_bills = sum(zero_bill),
            has_HO_exemp = sum(has_HO_exemp),
            has_SF_exemp = sum(has_SF_exemp),
            has_FR_exemp = sum(has_FR_exemp),
            has_LTHO_exemp = sum(has_LTHO_exemp),
            has_DI_exemp = sum(has_DI_exemp),
            has_VR_exemp = sum(has_VR_exemp),
            has_DV_exemp = sum(has_DV_exemp),
            has_AB_exemp = sum(has_AB_exemp),
            year = year_variable
  ) %>%
  
  mutate(tax_rate_current = nbh_final_tax_to_dist/nbh_nonTIF_EAV_post_exemps) %>% 
  
  select(year, nbhd_code, nbh_current_rate=tax_rate_current, everything()) %>% 
  arrange(desc(nbhd_code))


# nbh_parcel_summary <- joined_pin_data %>%
#   left_join(nbh_pins2) %>%
#   mutate(nbhd_3 = str_sub(nbhd_code, 3,5)) %>%
#   group_by(parcel, nbhd_code, nbhd_3, triad_name, township_name, township_code) %>%
#   filter(!is.na(nbhd_code)) %>%
#   
#   summarize(nbh_final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE), # amount billed by munis with current exemptions in place
#             nbh_tif_rev = sum(final_tax_to_tif, na.rm=TRUE),
#                         nbh_nonTIF_EAV_post_exemps = sum(final_tax_to_dist/(tax_code_rate/100), na.rm = TRUE),
#                         nbh_TIF_increment_EAV = sum(final_tax_to_tif/(tax_code_rate/100), na.rm=TRUE),  
#                         nbh_exe_and_abates_frombills = sum(tax_amt_exe/(tax_code_rate/100), na.rm=TRUE),  # Combines all exemptions and abatements!
#                         Total_EAV = sum((tax_amt_exe + final_tax_to_dist + final_tax_to_tif)/(tax_code_rate/100), na.rm = TRUE),
#                         
#                         nbh_PC = n(),
#                         nbh_parcels = n_distinct(parcel),
#                         nbh_TIF_PC = sum(in_tif),
#                         nbh_AV = sum(av),
#                         nbh_EAV = sum(eav),
#                         nbh_FMV = sum(fmv),
#                         nbh_res_exe = sum(exe_homeowner + exe_senior + exe_freeze + 
#                                             exe_longtime_homeowner + exe_disabled + 
#                                             exe_vet_returning + exe_vet_dis_lt50 + 
#                                             exe_vet_dis_50_69 + exe_vet_dis_ge70, na.rm = TRUE),
#                         nbh_exe_homeowner = sum(exe_homeowner, na.rm=TRUE),
#                         nbh_exe_senior = sum(exe_senior, na.rm=TRUE),
#                         nbh_exe_freeze = sum(exe_freeze, na.rm=TRUE),
#                         nbh_exe_longtime_homeowner = sum(exe_longtime_homeowner, na.rm=TRUE),
#                         nbh_exe_disabled = sum(exe_disabled, na.rm=TRUE),
#                         nbh_exe_vet_returning = sum(exe_vet_returning, na.rm=TRUE),
#                         nbh_exe_vet_dis_lt50 = sum(exe_vet_dis_lt50, na.rm=TRUE),
#                         nbh_exe_vet_dis_50_69 = sum(exe_vet_dis_50_69, na.rm=TRUE),
#                         nbh_exe_vet_dis_ge70 = sum(exe_vet_dis_ge70, na.rm=TRUE),
#                         
#                         nbh_exe_abate = sum(exe_abate, na.rm=TRUE),
#                         
#                         zero_bills = sum(zero_bill),
#                         has_HO_exemp = sum(has_HO_exemp),
#                         has_SF_exemp = sum(has_SF_exemp),
#                         has_FR_exemp = sum(has_FR_exemp),
#                         has_LTHO_exemp = sum(has_LTHO_exemp),
#                         has_DI_exemp = sum(has_DI_exemp),
#                         has_VR_exemp = sum(has_VR_exemp),
#                         has_DV_exemp = sum(has_DV_exemp),
#                         has_AB_exemp = sum(has_AB_exemp),
#                         year = year_variable
#               ) %>%
#               
#               mutate(tax_rate_current = nbh_final_tax_to_dist/nbh_nonTIF_EAV_post_exemps,
#                      parcel_inTIF = ifelse(nbh_TIF_PC > 1, 1, 0)) %>% 
#               
#               select(year, parcel, nbhd_code, nbh_current_rate=tax_rate_current, everything())
# 

}

write_csv(nbh_MC_summary, "./Output/ptaxsim_nbh_MC_summary_2022.csv")
write_csv(nbh_summary, "./Output/ptaxsim_nbh_summary_2022.csv")



## summations of fmv ---------------------------

# county wide FMV
sum(nbh_summary$nbh_FMV, na.rm=TRUE)


## Commercial and industrial FMV
nbh_MC_summary %>% ungroup() %>% 
  filter(major_class_code %in% c("5A", "5B", "6", "7", "8")) %>% 
  summarize(sum_fmv = sum(nbh_MC_FMV, na.rm=TRUE))

nbh_MC_summary %>% ungroup() %>% 
  filter(major_class_code %in% c("5A", "5B", "6", "7", "8")) %>% 
  summarize(sum_av = sum(nbh_MC_AV, na.rm=TRUE))
