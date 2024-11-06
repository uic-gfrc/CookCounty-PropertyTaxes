# File & Loop Prep ----------------------------------------
## AWM & MVH ##
## 12-30-23 ##

library(tidyverse)
library(DBI)
library(data.table)
library(jsonlite)
library(ptaxsim)
library(glue)

# Create the DB connection with the default name expected by PTAXSIM functions

# AWM filepath:
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")

# Load supplemental files w/ "clean" muni names and detail re: class codes.

class_dict <- read_csv("./Necessary_Files/class_dict_expanded.csv") %>% select(-loa_2022)


nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")

 # Set years for loop to run.

years <-(2006:2023)

# Create empty dataframes for the loop to populate.
agency_summary <- NULL

commercial_classes <- c(401:435, 490, 491, 492, 496:499,
                        500:535,590, 591, 592, 597:599,
                        700:799,
                        800:835, 891, 892, 897, 899)   %>% as.character()

industrial_classes <- c(480:489,493,
                        550:589, 593,
                        600:699,
                        850:890, 893 ) %>% as.character()



is.integer64 <- function(x){
  class(x)=="integer64"
}


# Loop Start --------------------------------------------------------------



for(i in years){

  year_variable = i

  # PTAXSIM tables ---------------------------------------

  # ##  taxing agencies only + Cicero
  # agency_names <- DBI::dbGetQuery(
  #   ptaxsim_db_conn,
  #   "SELECT DISTINCT agency_num, agency_name, minor_type
  #   FROM agency_info
  #   WHERE minor_type = 'MUNI'
  #   OR agency_num = '020060000'
  #   "
  # )


  agency_dt<- dbGetQuery(ptaxsim_db_conn, paste('SELECT * FROM agency WHERE year = ', i, ';'))
  agency_dt <- agency_dt %>% mutate_if(is.integer64, as.double)

  tax_codes <- dbGetQuery(ptaxsim_db_conn, paste('SELECT DISTINCT tax_code_num, tax_code_rate FROM tax_code WHERE year = ', i, ';'))



 # sql <- "SELECT * FROM tax_code WHERE agency_num IN ({agency_names$agency_num*}) AND year = ?year"
 # query <- sqlInterpolate(ptaxsim_db_conn, sql, year = i)
#  muni_tax_codes<- dbGetQuery(ptaxsim_db_conn, glue_sql(query, .con = ptaxsim_db_conn))

  tif_distrib <- DBI::dbGetQuery(ptaxsim_db_conn, paste('SELECT * FROM tif_distribution WHERE year = ', i, ';'))


  ## All tax codes.
  ## tax codes within municipalities have additional info
 # tc_muninames <- tax_codes %>%
  tc_agency_names <- tax_codes# %>%
   # left_join(muni_tax_codes) %>%
  #  left_join(agency_names) 
  # 



  sql <- "SELECT DISTINCT pin, class, tax_code_num FROM pin WHERE tax_code_num IN ({tax_codes$tax_code_num*}) AND year = ?year"
  query <- sqlInterpolate(ptaxsim_db_conn, sql, year = i)
  cook_pins <- dbGetQuery(ptaxsim_db_conn, glue_sql(query, .con = ptaxsim_db_conn))



  # combine taxing agency names and agency type to data table that has eav and extension values
  agency_data <- agency_dt %>%
   # right_join(agency_dt, agency_names) %>%
    select(-c(cty_dupage_eav:cty_livingston_eav)) %>% # drop some of the unused variables
    arrange(agency_num)


  eq_factor <- read_csv("./Necessary_Files/eq_factor.csv") %>%
    filter(year == i) %>%
    select(eq_factor_final) %>%
    as.numeric()

  ccao_loa <- read_csv("./inputs/ccao_loa.csv") %>%
    mutate(class_code = as.character(class_code)) %>%
    filter(year == i) %>%
    select(-year) %>%
    mutate(loa = as.numeric(loa)) %>%
    mutate(loa = ifelse(loa == 0, NA, loa))

  # Tax Bills ---------------------------------------------------------------


  # pin-taxing agency combinations for tax bills
  taxbills <- tax_bill(year_variable,  cook_pins$pin,
                       simplify = FALSE)

  taxbills <- taxbills %>% 
    left_join(ccao_loa, by = c("class" = "class_code"))

  #  billed properties with 14-digit PINs
  agency_summary2 <- taxbills %>%
    mutate_if(is.integer64, as.double) %>% 

    mutate(
      incent_prop = ifelse(between(class, 600, 899), 1, 0),
      res_prop = ifelse(between(class, 200, 399), 1, 0),
      c2_prop = ifelse(between(class, 200, 299), 1, 0),
      class_group = str_sub(class, 1,1),
      class_group = case_when(
        (class_group == 5 & class %in% commercial_classes) ~ "5A",
        (class_group == 5 & class %in% industrial_classes) ~ "5B",
        class_group == 7 &  class < 742 ~ "7A",    # commercial developments less than $2 million
        class_group == 7 &  class >= 742 ~ "7B",   # commercial developments greater than $2 million
        (class_group == 8 & class %in% commercial_classes ) ~ "8A",
        (class_group == 8 & class %in% industrial_classes ) ~ "8B",
        TRUE ~ as.character(class_group)),
      equalized_av = av*eq_factor,
 )  %>%
    group_by(agency_num, agency_name, agency_tax_rate, agency_total_eav, agency_total_ext, agency_minor_type, agency_major_type) %>%
    
    mutate(
              taxed_eav = final_tax_to_dist / agency_tax_rate,
              total_value_eav = ((final_tax_to_dist + final_tax_to_tif)/ agency_tax_rate + exe_total),
              
              total_taxed_av =  (taxed_eav / eq_factor),     # current value that taxing agencies can tax for their levies
              in_tif = (ifelse(tax_code %in% tif_distrib$tax_code_num, 1, 0)),
              
              
              taxed_av =  taxed_eav / eq_factor, # current value that taxing agencies can tax for their levies
              

              ## taxable AV = equalized assessed value net TIF increments, gross exemptions.
              ## Used for calculating untaxable value further below
              taxable_av = (final_tax_to_dist / agency_tax_rate *100 + exe_total)/ eq_factor,
              
              ## FMV * assessment rate = AV
              taxed_fmv = taxed_av / loa,
              fmv = av / loa,
              fmv = ifelse(is.na(fmv), 0, fmv),
              
              ## untaxable value = exempt EAV from abatements and exemptions plus TIF increment plus dif in incentive prop values from loa rates
              untaxable_value_eav = exe_total +
                
                ## TIF increment EAV above frozen EAV, which becomes TIF revenue
                (final_tax_to_tif /  agency_tax_rate) +
                
                ## difference between 25% and reduced level of assessment for incentive class properties. Excludes TIF increment when calculating the difference!
                ifelse(incent_prop == 1,
                       (taxable_av - taxed_av)*eq_factor, 0),
              
              #  manually adjust untaxable value of class 239 properties
              untaxable_value_eav = ifelse(class == 239,
                                           equalized_av-taxed_eav, untaxable_value_eav),
              
              untaxable_value_av = untaxable_value_eav / eq_factor,
              untaxable_value_fmv = untaxable_value_av / loa,
              untaxable_value_fmv = ifelse(is.nan(untaxable_value_av), 0, untaxable_value_av),
              
              exempt_eav_inTIF = ifelse(in_tif == 1,
                                            exe_total, 0),
              exempt_eav = exe_total,
              exempt_fmv = exempt_eav / eq_factor / loa,
              
              fmv_inTIF = ifelse(in_tif==1,
                                 av/loa, 0),
              
              fmv_tif_increment = ifelse(final_tax_to_tif > 0,
                                         ((final_tax_to_tif / (agency_tax_rate)) / eq_factor ) / loa, 0),
              
              fmv_incents_inTIF = ifelse(incent_prop == 1 & in_tif == 1,
                                         fmv, 0),
              fmv_incents_tif_increment = ifelse(incent_prop == 1 & final_tax_to_tif > 0 ,
                                                 ((final_tax_to_tif / (agency_tax_rate)) / eq_factor ) / loa, 0),
              
              eav_incents_inTIF = fmv_incents_inTIF * loa * eq_factor) %>%
    summarize(
              fmv_exemptions = sum(exe_total/eq_factor/loa, na.rm=TRUE),
              fmv_residential = sum(ifelse(res_prop == 1, fmv, 0), na.rm = TRUE),
              fmv_industrial = sum(ifelse(class %in% industrial_classes, fmv, 0), na.rm = TRUE),
              fmv_commercial = sum(ifelse(class %in% commercial_classes, fmv, 0), na.rm = TRUE),
              avg_C2_bill_noexe = mean(ifelse(c2_prop == 1 & exe_total == 0, (final_tax_to_dist + final_tax_to_tif), NA), na.rm=TRUE),
              # for ALL exemption types, not just GHE
              avg_C2_bill_withexe = mean(ifelse(c2_prop == 1 & exe_total > 0, (final_tax_to_dist + final_tax_to_tif), NA), na.rm=TRUE),
              
      av = sum(av, na.rm = TRUE),            # PINs appear multiple times, uses first av appearance
      eq_av = sum(equalized_av, na.rm = TRUE),      # equalized AV = potential taxable value for homeowners
      eav = sum(eav, na.rm = TRUE),          # EAV after exemptions are subtracted.
      pins_in_tax_district = n(), # number of taxing agencies that tax the pin
      final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE), # portion of all levies paid by the pin
      final_tax_to_tif = sum(final_tax_to_tif, na.rm = TRUE),
      tax_amt_exe = sum(tax_amt_exe, na.rm = TRUE),           # revenue lost due to exemptions
      tax_amt_pre_exe = sum(tax_amt_pre_exe, na.rm = TRUE),   # total rev before all exemptions (at existing tax rate)
      tax_amt_post_exe = sum(tax_amt_post_exe, na.rm = TRUE), # total rev after all exemptions
      rpm_tif_to_cps = sum(rpm_tif_to_cps, na.rm = TRUE),     # not used currently
      rpm_tif_to_rpm = sum(rpm_tif_to_rpm, na.rm=TRUE),       # not used currently
      rpm_tif_to_dist = sum(rpm_tif_to_dist, na.rm=TRUE),     # not used currently
      tif_share = mean(tif_share, na.rm=TRUE),                # not used currently
    
      PC_residential = sum(ifelse(res_prop == 1, 1, 0), na.rm = TRUE),
      PC_industrial  = sum(ifelse(class %in% industrial_classes, 1, 0), na.rm = TRUE),
      PC_commercial = sum(ifelse(class %in% commercial_classes, 1, 0), na.rm = TRUE),
      PC_inTIF = sum(in_tif, na.rm=TRUE),
      PC_withincents = sum(incent_prop == 1, na.rm = TRUE),
      PC_incents_inTIFs = sum(ifelse(incent_prop == 1 & in_tif == 1, 1, 0), na.rm = TRUE),
      PC_claimed_exe = sum(ifelse(exe_total > 0, 1, 0), na.rm=TRUE),
      fmv_incentive = sum(ifelse(incent_prop == 1, fmv, 0), na.rm = TRUE),
      fmv_taxed =  sum(taxed_fmv, na.rm=TRUE),
      fmv_incents_inTIFs = sum(ifelse(incent_prop == 1 & in_tif == 1, fmv, 0), na.rm = TRUE),
      fmv_inTIF = sum(fmv_inTIF, na.rm=TRUE),
      fmv_incents_tif_increment = sum(fmv_incents_tif_increment, na.rm=TRUE),
      fmv_tif_increment = sum(fmv_tif_increment, na.rm=TRUE),
      fmv_untaxable_value = sum(untaxable_value_fmv , na.rm=TRUE),
      fmv = sum(fmv, na.rm=TRUE),
     all_exemptions = sum(exe_total, na.rm = TRUE),
     av_taxed = sum(taxed_av, na.rm=TRUE),
     eav_taxed = sum(taxed_eav, na.rm=TRUE),
     untaxable_value_av = sum(untaxable_value_av, na.rm=TRUE),

    ) %>%
    mutate(
      pct_fmv_untaxable = fmv_untaxable_value / fmv,
      pct_fmv_taxed = fmv_taxed / fmv,
      pct_av_taxed = av_taxed / av,
      pct_av_untaxable = untaxable_value_av/av,
      year = year_variable) %>%
      select(agency_num, agency_name, final_tax_to_dist, final_tax_to_tif, 
             agency_tax_rate, eav, av, everything())
  
  rm(taxbills)
    


  # bind muni level yearly data together
  if(is.data.frame(agency_summary)){agency_summary <- rbind(agency_summary, agency_summary2)}else{agency_summary <- agency_summary2}
  rm(agency_summary2)

 }

# Export CSVs ------------------------------------------------------------


write_csv(agency_summary, "./Output/ptaxsim_taxing_agency_summaries_2006to2023.csv")
