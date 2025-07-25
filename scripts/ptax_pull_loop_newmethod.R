# File & Loop Prep ----------------------------------------
## AWM##
## 1-14-2025 ##

library(tidyverse)
library(DBI)
library(data.table)
library(ptaxsim)
library(glue)

# Create the DB connection with the default name expected by PTAXSIM functions

# AWM filepath:
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")

# MVH filepath:
# ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "ptaxsim.db")

# Load supplemental files w/ "clean" muni names and detail re: class codes.

class_dict <- read_csv("./Necessary_Files/class_dict_expanded.csv") %>% 
  select(class_code, major_class_code)  |> 
  mutate(class = as.character(class_code))
  
options(scipen = 999)  # Park Forest has weird formatting if you don't do this. No, I never figured out WHY it has weird formatting.

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx") |>
 mutate(agency_number = as.character(as.numeric(agency_number)),
       agency_number = str_pad(string = agency_number, width = 9, side = "left", pad ="0"))

 # Set years for loop to run.

years <-(2006:2023)
# Create empty dataframes for the loop to populate.
county_sums <- NULL

county_MC_sums <- NULL

muni_level_summary <- NULL

muni_MC_summary <- NULL


commercial_classes <- c(401:435, 490, 491, 492, 496:499,
                        500:535,590, 591, 592, 597:599,
                        700:799,
                        800:835, 891, 892, 897, 899)   %>% as.character()

industrial_classes <- c(480:489,493,
                        550:589, 593,
                        600:699,
                        850:890, 893)  %>% as.character()

is.integer64 <- function(x){
  class(x)=="integer64"
}

q = c(.25, .5, .75)

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
  agency_dt <- agency_dt %>%  mutate_if(is.integer64, as.double)

  tax_codes <- dbGetQuery(ptaxsim_db_conn, paste('SELECT DISTINCT tax_code_num, tax_code_rate FROM tax_code WHERE year = ', i, ';'))



  sql <- "SELECT * FROM tax_code WHERE agency_num IN ({muni_agency_names$agency_num*}) AND year = ?year"
  query <- sqlInterpolate(ptaxsim_db_conn, sql, year = i)
  muni_tax_codes <- dbGetQuery(ptaxsim_db_conn, glue_sql(query, .con = ptaxsim_db_conn)) |> 
    select(-year, -agency_rate, - tax_code_rate)
  
  # identify tax codes that collect tif revenue:
  tif_distrib <- DBI::dbGetQuery(ptaxsim_db_conn, paste('SELECT * FROM tif_distribution WHERE year = ', i, ';')) |>
    select(tax_code_num, tax_code_distribution_pct, tax_code_frozen_eav, tax_code_eav) %>%
    mutate(tax_code_distribution_pct = tax_code_distribution_pct/100, # the tif share
    ) 

  ## All tax codes.
  ## tax codes within municipalities have additional info
  # Combine tax code information
  tc_muninames <- tax_codes %>%
    left_join(muni_tax_codes, by = c("tax_code_num")) %>%
    left_join(muni_agency_names, by = "agency_num") %>%
    mutate(agency_num = as.character(agency_num)) |>
    left_join(nicknames, by = c("agency_num" = "agency_number"))  |>  
    
    mutate(tax_code_rate = tax_code_rate/100)



  sql <- "SELECT * FROM pin WHERE tax_code_num IN ({tax_codes$tax_code_num*}) AND year = ?year"
  query <- sqlInterpolate(ptaxsim_db_conn, sql, year = i)
  cook_pins <- dbGetQuery(ptaxsim_db_conn, glue_sql(query, .con = ptaxsim_db_conn))

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


  # Summarize PIN Exemptions ------------------------------------------------

  # Summarize ---------------------------------------------------------------

  ## Make variables  that appear in taxbill() command
  ## Different than taxbill because it checks to see if the tax_bill_total == $0 
  ## from the `pin` table, since that is the most accurate,
  ## and if zero, it changes its taxed value to zero instead of using the 
  ## `final_tax_to_dist` variable previously created by taxbill()  
  ## (which took the equalized AV and subtracted exemptions, but if the exemptions 
  ## were not correct, like the missing disabled veteran exemptions, 
  ## than the final tax to district was also wrong, and all the taxbase calculations were off slightly)
  ## mattered for some municipalities way more than others.
  ## did not change over all implications of research, but made it more accurate.
 
   pin_data <- cook_pins |>
    mutate_if(is.integer64, as.double ) %>%
    left_join(ccao_loa, by = c("class" = "class_code")) %>%
    left_join(tc_muninames, by = c("tax_code_num")) |>
    left_join(tif_distrib, by ="tax_code_num") |>
    mutate(tax_code_distribution_pct = ifelse(is.na(tax_code_distribution_pct), 0, tax_code_distribution_pct)) |>
    mutate(
      
      incent_prop = ifelse(between(class, 600, 899), 1, 0),
      res_prop = ifelse(between(class, 200, 399), 1, 0),
      c2_prop = ifelse(between(class, 200, 299), 1, 0),
      parcels = str_sub(pin, 1, 10), 
      in_tif = ifelse(tax_code_num %in% tif_distrib$tax_code_num, 1, 0),
      tif_tax_code_frozen_eav = ifelse(is.na(tax_code_frozen_eav), 0, tax_code_frozen_eav),
      tif_tax_code_eav = ifelse(is.na(tax_code_eav), 0, tax_code_eav), # only TIF taxcodes
      tif_tax_code_increment_eav = tif_tax_code_eav - tif_tax_code_frozen_eav,
      tif_tax_code_increment_eav = ifelse(tif_tax_code_increment_eav < 0, 0, tif_tax_code_increment_eav),
      in_tif_andpays_revtotif = ifelse(in_tif == 1 & tif_tax_code_eav > tif_tax_code_frozen_eav, 1, 0 ),
    ) |>
    
    mutate( 
      eq_av = av_clerk*eq_factor,
      exe_total_old = rowSums(across(starts_with("exe_"))),
      exe_total_old = ifelse(exe_total_old > eq_av, eq_av, exe_total_old),
      
      # create variables that appear from taxbill() function
      taxed_eav_old = av_clerk*eq_factor - exe_total_old,      # but exe_total was missing some exemptions.
      
      flag_missingdata = ifelse(taxed_eav_old > 1000 & tax_bill_total == 0 & c2_prop == 1, 1, 0)) |>
    
    mutate(exe_missing_disvet = ifelse(taxed_eav_old > 1000 & tax_bill_total == 0 & c2_prop == 1, taxed_eav_old, 0)) |>
    
    mutate(taxed_eav_adj = ifelse(taxed_eav_old > 1000 & flag_missingdata == 1, 0 , taxed_eav_old),
           total_taxed_eav_AWM = tax_bill_total / tax_code_rate,  # EAV that was taxed by TIFs and taxing districts
           taxed_eav_TIFincrement = total_taxed_eav_AWM *tax_code_distribution_pct,
           taxed_eav_nonTIF = total_taxed_eav_AWM*(1-tax_code_distribution_pct)) |>
    
    mutate(exe_total_adj = rowSums(across(starts_with("exe_")))- exe_total_old) |> # don't double count the old total value when summing the values
    
    mutate(# zero_bill = ifelse(eav <= all_exemptions, 1, 0),  ## old way of creating zero bill but has missing vet exemptions data problem
      has_HO_exemp = ifelse(exe_homeowner > 0, 1, 0),
      has_SR_exemp = ifelse(exe_senior > 0, 1, 0),
      has_FR_exemp = ifelse(exe_freeze > 0, 1, 0),
      has_LTHO_exemp = ifelse(exe_longtime_homeowner > 0, 1, 0),
      has_DI_exemp = ifelse(exe_disabled > 0, 1, 0),
      has_VR_exemp = ifelse(exe_vet_returning > 0, 1, 0),
      
      # includes missing disabled veteran exemption counts
      has_DV_exemp = ifelse(exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70 + exe_missing_disvet > 0, 1, 0),
      has_AB_exemp = ifelse(exe_abate > 0, 1, 0),
    )|>
  
    mutate( 
      tax_amt_exe = exe_total_adj*tax_code_rate,    # calculate tax bill reduction in dollars. Exempt EAV * current tax rate. "Naive" tax savings.
      tax_amt_pre_exe = av_clerk*eq_factor*tax_code_rate,
      tax_amt_post_exe = tax_amt_pre_exe - tax_amt_exe,
      tax_amt_post_exe = ifelse(tax_amt_post_exe < 0, 0, tax_amt_post_exe),
      
      final_tax_to_tif = taxed_eav_TIFincrement*tax_code_rate,
      final_tax_to_dist = taxed_eav_nonTIF*tax_code_rate,
      
  
      # NOTE: the number of $0 tax bills identified when using the tax_bill() command from ptaxsim is different than using the tax bill total value directly from the pin db table
      zero_bill = ifelse(tax_bill_total == 0, 1, 0),
  
      # for A and B property types of commercial and industrial properties
      class_1dig = str_sub(class, 1, 1),
      class_group = case_when(
        (class_1dig == 5 & class %in% commercial_classes) ~ "5A",
        (class_1dig == 5 & class %in% industrial_classes) ~ "5B",
        class_1dig == 7 &  class < 742 ~ "7A",
        class_1dig == 7 &  class >= 742 ~ "7B",
        (class_1dig == 8 & class %in% commercial_classes ) ~ "8A",
        (class_1dig == 8 & class %in% industrial_classes ) ~ "8B",
        TRUE ~ as.character(class_1dig)))
    

  # Create other variables used in summary files -----------------------------
  pin_data <- pin_data |>
    
    # taxed_eav is the eav that is taxable by the nonTIF taxing districts
    rename(taxed_eav = taxed_eav_nonTIF) |>
    
    mutate(
      av = av_clerk,
      # taxing district revenue = taxable eav * tax rate so rearrange the formula:
    #  taxed_eav = final_tax_to_dist / tax_code_rate*100,
    
     # total_value_eav = (final_tax_to_dist + final_tax_to_tif)/ tax_code_rate * 100 + exe_total_adj + abatements,

      taxed_av =  taxed_eav / eq_factor, # current value that taxing agencies can tax for their levies

      ## taxable AV = equalized assessed value net TIF increments, gross exemptions.
      ## Used for calculating untaxable value further below
    #  taxable_av = (final_tax_to_dist / tax_code_rate  + exe_total_adj)/ eq_factor,

      ## FMV * assessment rate = AV
      taxed_fmv = taxed_av / loa,
      #taxed_fmv = ifelse(is.nan(taxed_fmv), 0, taxed_fmv),

      fmv = av_clerk / loa,
      fmv = ifelse(is.na(fmv), 0, fmv),
      ## untaxable value = exempt EAV from abatements and exemptions
      untaxable_value_eav = exe_total_adj +

        ## TIF increment EAV above frozen EAV, which becomes TIF revenue
        (final_tax_to_tif /  tax_code_rate) +
      
      ## difference between 25% and reduced level of assessment for incentive class properties. Excludes TIF increment when calculating the difference!
        ifelse(incent_prop==1, (taxed_av/loa*0.25 - taxed_av)*eq_factor, 0),

      #  manually adjust untaxable value of class 239 properties
      untaxable_value_eav = ifelse(class == 239,
                                  eq_av-taxed_eav, untaxable_value_eav),

      untaxable_value_av = untaxable_value_eav / eq_factor,
      untaxable_value_fmv = untaxable_value_av / loa,
      untaxable_value_fmv = ifelse(is.nan(untaxable_value_av), 0, untaxable_value_av),

      exempt_eav_inTIF = ifelse(in_tif == 1,
                                exe_total_adj, 0),
      exempt_eav= exe_total_adj,
      exempt_fmv = exempt_eav / eq_factor / loa,

      fmv_inTIF = ifelse(in_tif==1,
                         av/loa, 0),
      fmv_tif_increment = ifelse(final_tax_to_tif > 0,
                                 ((final_tax_to_tif / (tax_code_rate)) / eq_factor ) / loa, 0),

    fmv_incents_inTIF = ifelse(incent_prop == 1 & in_tif == 1,
                               fmv, 0),
    fmv_incents_tif_increment = ifelse(incent_prop == 1 & final_tax_to_tif > 0 ,
                                       ((final_tax_to_tif / (tax_code_rate)) / eq_factor ) / loa, 0),
    eav_incents_inTIF = fmv_incents_inTIF * loa * eq_factor
    ) %>%
    select(tax_code_num, class, pin, taxed_fmv,
           untaxable_value_fmv, fmv_inTIF, fmv_tif_increment, fmv_incents_tif_increment, fmv, tax_bill_total, final_tax_to_dist, final_tax_to_tif, tax_code_rate, taxed_eav, eq_av, av, everything())

  ## County Level Summmaries -----------------------------------------------------------


  county_sums2 <- pin_data %>%
    ungroup() %>%
    summarize(
      cty_PC = n(),
      cty_parcels =n_distinct(parcels),
      cty_PC_residential = sum(ifelse(res_prop == 1, 1, 0), na.rm = TRUE),
      cty_PC_industrial  = sum(ifelse(class %in% industrial_classes, 1, 0), na.rm = TRUE),
      cty_PC_commercial = sum(ifelse(class %in% commercial_classes, 1, 0), na.rm = TRUE),
      cty_PC_com_incent = sum(ifelse(class %in% commercial_classes & incent_prop == 1, 1, 0), na.rm = TRUE),
      cty_PC_com_incent_inTIF = sum(ifelse(class %in% commercial_classes & incent_prop == 1 & in_tif == 1, 1, 0), na.rm = TRUE),
      cty_PC_ind_incent = sum(ifelse(class %in% industrial_classes & incent_prop == 1, 1, 0), na.rm = TRUE),
      cty_PC_ind_incent_inTIF = sum(ifelse(class %in% industrial_classes & incent_prop == 1 & in_tif == 1, 1, 0), na.rm = TRUE),
      cty_PC_inTIF = sum(in_tif, na.rm=TRUE),
      cty_PC_withincents = sum(ifelse(incent_prop == 1, 1, 0), na.rm = TRUE),
      cty_PC_incents_inTIFs = sum(ifelse(incent_prop == 1 & in_tif == 1, 1, 0), na.rm = TRUE),
      cty_PC_claimed_exe = sum(ifelse(exe_total_adj > 0, 1, 0), na.rm=TRUE),
      cty_PC_comandind =  sum(ifelse((class %in% commercial_classes | class %in% industrial_classes), 1, 0)),
      cty_PC_comandind_inTIF =  sum(ifelse((class %in% commercial_classes | class %in% industrial_classes) & in_tif == 1, 1, 0)),
      
      
      cty_fmv_incentive = sum(ifelse(incent_prop == 1, fmv, 0), na.rm = TRUE),
      cty_fmv_taxed =  sum(taxed_fmv, na.rm=TRUE),
      cty_fmv_incents_inTIFs = sum(ifelse(incent_prop == 1 & in_tif == 1, fmv, 0), na.rm = TRUE),
      cty_fmv_inTIF = sum(fmv_inTIF, na.rm=TRUE),
 
      cty_fmv_incents_tif_increment = sum(fmv_incents_tif_increment, na.rm=TRUE),
      cty_fmv_tif_increment = sum(fmv_tif_increment, na.rm=TRUE),
      cty_fmv_untaxable_value = sum(untaxable_value_fmv , na.rm=TRUE),
      cty_fmv = sum(fmv, na.rm=TRUE),
      cty_fmv_exemptions = sum((exe_total_adj-exe_abate)/eq_factor/loa, na.rm=TRUE),
      cty_fmv_abatements = sum((exe_abate/eq_factor)/loa, na.rm=TRUE),
      cty_zero_bill = sum(zero_bill, na.rm=TRUE),
      cty_fmv_residential = sum(ifelse(res_prop == 1, fmv, 0), na.rm = TRUE),
      cty_fmv_c2 = sum(ifelse(c2_prop == 1, fmv, 0), na.rm = TRUE),
      cty_fmv_industrial = sum(ifelse(class %in% industrial_classes, fmv, 0), na.rm = TRUE),
      cty_fmv_indwithincent = sum(ifelse(class %in% industrial_classes & incent_prop == 1, fmv, 0), na.rm = TRUE),
      cty_fmv_ind_incent_inTIF= sum(ifelse(class %in% industrial_classes & incent_prop == 1 & in_tif == 1, fmv, 0), na.rm = TRUE),
      
      cty_fmv_commercial = sum(ifelse(class %in% commercial_classes, fmv, 0), na.rm = TRUE),
      cty_fmv_comwithincent = sum(ifelse(class %in% commercial_classes & incent_prop == 1, fmv, 0), na.rm = TRUE),
      cty_fmv_com_incent_inTIF= sum(ifelse(class %in% commercial_classes & incent_prop == 1 & in_tif == 1, fmv, 0), na.rm = TRUE),
      cty_fmv_comandind = sum(ifelse(class %in% c(commercial_classes, industrial_classes), fmv, 0), na.rm = TRUE),
      
      cty_levy = sum(final_tax_to_dist, na.rm=TRUE),
      cty_current_rate_avg = mean(tax_code_rate, na.rm=TRUE),
      cty_avg_C2_bill_noexe = mean(ifelse(c2_prop == 1 & exe_total_adj == 0, (final_tax_to_dist+ final_tax_to_tif), NA), na.rm=TRUE),
      cty_avg_C2_bill_withexe = mean(ifelse(c2_prop == 1 & exe_total_adj > 0, (final_tax_to_dist+ final_tax_to_tif), NA), na.rm=TRUE),
      
      cty_av_taxed = sum(taxed_av, na.rm = TRUE),
      cty_av_taxed_commerc = sum(ifelse(class %in% commercial_classes, taxed_av, 0), na.rm = TRUE),
      cty_av_taxed_indust = sum(ifelse(class %in% industrial_classes, taxed_av, 0), na.rm = TRUE),
      cty_untaxable_value_av = sum(untaxable_value_av, na.rm=TRUE),
      cty_av = sum(av, na.rm=TRUE),
      
      cty_final_tax_to_dist = sum(final_tax_to_dist, na.rm=TRUE),
      cty_final_tax_to_tif = sum(final_tax_to_tif, na.rm=TRUE),
      
      cty_taxed_eav = sum(taxed_eav, na.rm=TRUE),
      cty_taxed_eav_commerc = sum(ifelse(class %in% commercial_classes, taxed_eav, 0), na.rm = TRUE),
      cty_taxed_eav_indust = sum(ifelse(class %in% industrial_classes, taxed_eav, 0), na.rm = TRUE),
      
      exempt_allexemptions_eav = sum(exe_total_adj,na.rm=TRUE),
      
      exempt_GHE_eav = sum(exe_homeowner, na.rm=TRUE),
      exempt_SR_eav = sum(exe_senior, na.rm=TRUE), # senior exemption
      exempt_FR_eav = sum(exe_freeze, na.rm=TRUE), #  senior freeze exemption
      exempt_LTHO_eav = sum(exe_longtime_homeowner, na.rm=TRUE),
      exempt_DIS_eav = sum(exe_disabled, na.rm=TRUE),
      # combined veteran exemptions and the missing veteran exemptions
      exempt_VET_eav = sum(exe_vet_returning + exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70 + exe_missing_disvet, na.rm=TRUE),
      
      total_value_eav = sum(eq_av, na.rm=TRUE),
      untaxable_value_eav = sum(untaxable_value_eav, na.rm=TRUE),
      has_HO_exemp = sum(has_HO_exemp, na.rm=TRUE),
      has_SR_exemp = sum(has_SR_exemp, na.rm=TRUE),
      has_FR_exemp = sum(has_FR_exemp, na.rm=TRUE),
      has_LTHO_exemp = sum(has_LTHO_exemp, na.rm=TRUE),
      has_DI_exemp = sum(has_DI_exemp, na.rm=TRUE),
      has_VR_exemp = sum(has_VR_exemp, na.rm=TRUE),
      has_DV_exemp = sum(has_DV_exemp, na.rm=TRUE),
      has_AB_exemp = sum(has_AB_exemp, na.rm=TRUE)
      ) %>%
    mutate(
      cty_taxable_value_fmv = cty_fmv - cty_fmv_untaxable_value,
      cty_pct_fmv_untaxable = cty_fmv_untaxable_value / cty_fmv,
      cty_pct_fmv_taxed = cty_fmv_taxed / cty_fmv,
      cty_pct_fmv_incentinTIF = cty_fmv_incents_inTIFs / cty_fmv_incentive,
      cty_pct_fmv_incents_tif_increment = cty_fmv_incents_tif_increment / cty_fmv_incentive,
      
      cty_pct_av_taxed = cty_av_taxed / cty_av,
      cty_pct_av_untaxable = cty_untaxable_value_av/cty_av,
      
      cty_pct_incent_oftotalPC = cty_PC_withincents / cty_PC,  # incentive pins / all PINs
      cty_pct_incent_ofcomPC = cty_PC_com_incent / cty_PC_commercial,  # incentive pins / commercial PINs
      cty_pct_incent_ofindPC = cty_PC_ind_incent / cty_PC_industrial,  # incentive pins / commercial PINs
      cty_pct_PC_incent_inTIF = cty_PC_incents_inTIFs / cty_PC_withincents,
      year = year_variable) %>%
    select(year, everything())

if(is.data.frame(county_sums)){county_sums <- rbind(county_sums, county_sums2)}else{county_sums <- county_sums2}

  rm(county_sums2)

  ## County Major Class Summaries ---------------------------------------------
  
  county_MC_sums2 <- pin_data %>%
    ungroup() %>%
    group_by(class_group) |>
    summarize(
      cty_mc_PC = n(),
      cty_mc_parcels =n_distinct(parcels),
      
      cty_mc_PC_residential = sum(ifelse(res_prop == 1, 1, 0), na.rm = TRUE),
      cty_mc_PC_industrial  = sum(ifelse(class %in% industrial_classes, 1, 0), na.rm = TRUE),
      cty_mc_PC_commercial = sum(ifelse(class %in% commercial_classes, 1, 0), na.rm = TRUE),
      cty_mc_PC_com_incent = sum(ifelse(class %in% commercial_classes & incent_prop == 1, 1, 0), na.rm = TRUE),
      cty_mc_PC_com_incent_inTIF = sum(ifelse(class %in% commercial_classes & incent_prop == 1 & in_tif == 1, 1, 0), na.rm = TRUE),
      cty_mc_PC_ind_incent = sum(ifelse(class %in% industrial_classes & incent_prop == 1, 1, 0), na.rm = TRUE),
      cty_mc_PC_ind_incent_inTIF = sum(ifelse(class %in% industrial_classes & incent_prop == 1 & in_tif == 1, 1, 0), na.rm = TRUE),
      cty_mc_PC_inTIF = sum(in_tif, na.rm=TRUE),
      cty_mc_PC_withincents = sum(ifelse(incent_prop == 1, 1, 0), na.rm = TRUE),
      cty_mc_PC_incents_inTIFs = sum(ifelse(incent_prop == 1 & in_tif == 1, 1, 0), na.rm = TRUE),
      cty_mc_PC_claimed_exe = sum(ifelse(exe_total_adj > 0, 1, 0), na.rm=TRUE),
      cty_mc_PC_comandind =  sum(ifelse((class %in% commercial_classes | class %in% industrial_classes), 1, 0)),
      cty_mc_PC_comandind_inTIF =  sum(ifelse((class %in% commercial_classes | class %in% industrial_classes) & in_tif == 1, 1, 0)),
      
      
      cty_mc_fmv_incentive = sum(ifelse(incent_prop == 1, fmv, 0), na.rm = TRUE),
      cty_mc_fmv_taxed =  sum(taxed_fmv, na.rm=TRUE),
      cty_mc_fmv_incents_inTIFs = sum(ifelse(incent_prop == 1 & in_tif == 1, fmv, 0), na.rm = TRUE),
      cty_mc_fmv_inTIF = sum(fmv_inTIF, na.rm=TRUE),
      
      cty_mc_fmv_incents_tif_increment = sum(fmv_incents_tif_increment, na.rm=TRUE),
      cty_mc_fmv_tif_increment = sum(fmv_tif_increment, na.rm=TRUE),
      cty_mc_fmv_untaxable_value = sum(untaxable_value_fmv , na.rm=TRUE),
      cty_mc_fmv = sum(fmv, na.rm=TRUE),
      cty_mc_fmv_exemptions = sum((exe_total_adj-exe_abate)/eq_factor/loa, na.rm=TRUE),
      cty_mc_fmv_abatements = sum((exe_abate/eq_factor)/loa, na.rm=TRUE),
      cty_mc_zero_bill = sum(zero_bill, na.rm=TRUE),
      cty_mc_fmv_residential = sum(ifelse(res_prop == 1, fmv, 0), na.rm = TRUE),
      cty_mc_fmv_c2 = sum(ifelse(c2_prop == 1, fmv, 0), na.rm = TRUE),
      cty_mc_fmv_industrial = sum(ifelse(class %in% industrial_classes, fmv, 0), na.rm = TRUE),
      cty_mc_fmv_indwithincent = sum(ifelse(class %in% industrial_classes & incent_prop == 1, fmv, 0), na.rm = TRUE),
      cty_mc_fmv_ind_incent_inTIF= sum(ifelse(class %in% industrial_classes & incent_prop == 1 & in_tif == 1, fmv, 0), na.rm = TRUE),
      
      cty_mc_fmv_commercial = sum(ifelse(class %in% commercial_classes, fmv, 0), na.rm = TRUE),
      cty_mc_fmv_comwithincent = sum(ifelse(class %in% commercial_classes & incent_prop == 1, fmv, 0), na.rm = TRUE),
      cty_mc_fmv_com_incent_inTIF= sum(ifelse(class %in% commercial_classes & incent_prop == 1 & in_tif == 1, fmv, 0), na.rm = TRUE),
      cty_mc_fmv_comandind = sum(ifelse(class %in% c(commercial_classes, industrial_classes), fmv, 0), na.rm = TRUE),
      
      cty_mc_levy = sum(final_tax_to_dist, na.rm=TRUE),
      cty_mc_current_rate_avg = mean(tax_code_rate, na.rm=TRUE),
      cty_mc_avg_C2_bill_noexe = mean(ifelse(c2_prop == 1 & exe_total_adj == 0, (final_tax_to_dist+ final_tax_to_tif), NA), na.rm=TRUE),
      cty_mc_avg_C2_bill_withexe = mean(ifelse(c2_prop == 1 & exe_total_adj > 0, (final_tax_to_dist+ final_tax_to_tif), NA), na.rm=TRUE),
      
      cty_mc_av_taxed = sum(taxed_av, na.rm = TRUE),
      cty_mc_av_taxed_commerc = sum(ifelse(class %in% commercial_classes, taxed_av, 0), na.rm = TRUE),
      cty_mc_av_taxed_indust = sum(ifelse(class %in% industrial_classes, taxed_av, 0), na.rm = TRUE),
      cty_mc_untaxable_value_av = sum(untaxable_value_av, na.rm=TRUE),
      cty_mc_av = sum(av, na.rm=TRUE),
      
      cty_mc_final_tax_to_dist = sum(final_tax_to_dist, na.rm=TRUE),
      cty_mc_final_tax_to_tif = sum(final_tax_to_tif, na.rm=TRUE),
      
      cty_mc_taxed_eav = sum(taxed_eav, na.rm=TRUE),
      cty_mc_taxed_eav_commerc = sum(ifelse(class %in% commercial_classes, taxed_eav, 0), na.rm = TRUE),
      cty_mc_taxed_eav_indust = sum(ifelse(class %in% industrial_classes, taxed_eav, 0), na.rm = TRUE),
      
      exempt_allexemptions_eav = sum(exe_total_adj,na.rm=TRUE),

      exempt_GHE_eav = sum(exe_homeowner, na.rm=TRUE),
      exempt_SR_eav = sum(exe_senior, na.rm=TRUE), # senior exemption
      exempt_FR_eav = sum(exe_freeze, na.rm=TRUE), #  senior freeze exemption
      exempt_LTHO_eav = sum(exe_longtime_homeowner, na.rm=TRUE),  # can eventually be combined with GHE exemption in graphs and tables
      exempt_DIS_eav = sum(exe_disabled, na.rm=TRUE),
      # combined veteran exemptions and the missing veteran exemptions
      exempt_VET_eav = sum(exe_vet_returning + exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70 + exe_missing_disvet, na.rm=TRUE),
      
      
      total_value_eav = sum(eq_av, na.rm=TRUE),
      untaxable_value_eav = sum(untaxable_value_eav, na.rm=TRUE),
      has_HO_exemp = sum(has_HO_exemp, na.rm=TRUE),
      has_SR_exemp = sum(has_SR_exemp, na.rm=TRUE),
      has_FR_exemp = sum(has_FR_exemp, na.rm=TRUE),
      has_LTHO_exemp = sum(has_LTHO_exemp, na.rm=TRUE),
      has_DI_exemp = sum(has_DI_exemp, na.rm=TRUE),
      has_VR_exemp = sum(has_VR_exemp, na.rm=TRUE),
      has_DV_exemp = sum(has_DV_exemp, na.rm=TRUE),
      has_AB_exemp = sum(has_AB_exemp, na.rm=TRUE)
    ) %>%
    mutate(
      cty_mc_taxable_value_fmv = cty_mc_fmv - cty_mc_fmv_untaxable_value,
      cty_mc_pct_fmv_untaxable = cty_mc_fmv_untaxable_value / cty_mc_fmv,
      cty_mc_pct_fmv_taxed = cty_mc_fmv_taxed / cty_mc_fmv,
      cty_mc_pct_fmv_incentinTIF = cty_mc_fmv_incents_inTIFs / cty_mc_fmv_incentive,
      cty_mc_pct_fmv_incents_tif_increment = cty_mc_fmv_incents_tif_increment / cty_mc_fmv_incentive,
      
      cty_mc_pct_av_taxed = cty_mc_av_taxed / cty_mc_av,
      cty_mc_pct_av_untaxable = cty_mc_untaxable_value_av/cty_mc_av,
      
      cty_mc_pct_incent_oftotalPC = cty_mc_PC_withincents / cty_mc_PC,  # incentive pins / all PINs
      cty_mc_pct_incent_ofcomPC = cty_mc_PC_com_incent / cty_mc_PC_commercial,  # incentive pins / commercial PINs
      cty_mc_pct_incent_ofindPC = cty_mc_PC_ind_incent / cty_mc_PC_industrial,  # incentive pins / commercial PINs
      cty_mc_pct_PC_incent_inTIF = cty_mc_PC_incents_inTIFs / cty_mc_PC_withincents,
      year = year_variable) %>%
    select(year, everything())
  
  if(is.data.frame(county_MC_sums)){county_MC_sums <- rbind(county_MC_sums, county_MC_sums2)}else{county_MC_sums <- county_MC_sums2}
  
  rm(county_MC_sums2)

  ## Muni Level --------------------------------------------------------------

  # municipalities that are normally dropped from analysis have less than 75% 
  # of their taxed EAV in Cook County
  # Frankfort, Homer Glen, Oak Brook, East Dundee, University Park, Bensenville- For sure drop these, less than 5% of taxed EAV in Cook County
  # Municipalities that are frequently dropped but maybe somebody wants them left in sometimes:
  # Hinsdale, Roselle, Deerfield, Elgin, Buffalo Grove - have less than 20ish % of EAV in Cook 
  # Bartlett, Burr Ridge, Hanover Park, Steger, Barrington Hills, Barrington - have around 50% of EAV in Cook County
  # Park Forest, Tinley Park - have around 75% of EAV in Cook

  muni_level_summary2 <- pin_data %>%
    ungroup() %>%
    group_by(clean_name) %>%
    arrange(fmv) %>%
    summarize(
      mean_fmv_all = mean(fmv),
      median_fmv_all = median(fmv),
      min_fmv_all = min(fmv),
      quant25_all_fmv = round(quantile(fmv, probs = q[1])),
      quant50_all_fmv = round(quantile(fmv, probs = q[2])),
      quant75_all_fmv = round(quantile(fmv, probs = q[3])),
      max_fmv_all = max(fmv),

      mean_av_c2 = mean(ifelse(c2_prop == 1, av, NA), na.rm=TRUE),
      # median_av_c2 = median(ifelse(c2_prop == 1, av, NA), na.rm=TRUE),
      # min_av_c2 = min(ifelse(c2_prop == 1, av, NA), na.rm=TRUE),
      # quant25_c2_av = round(quantile(av, probs = q[1])[c2_prop==1]), ## breaks code
      # quant50_c2_av = round(quantile(av, probs = q[2])[c2_prop == 1]),
      # quant75_c2_av = round(quantile(av, probs = q[3])[c2_prop==1]),
      # max_av_c2 = max(ifelse(c2_prop == 1, av, NA)),

      muni_PC_total = n(),
      muni_parcels =n_distinct(parcels),
      
      muni_PC_residential = sum(ifelse(res_prop==1, 1, 0), na.rm = TRUE),
      muni_PC_industrial  = sum(ifelse(class %in% industrial_classes, 1, 0), na.rm = TRUE),
      muni_PC_commercial = sum(ifelse(class %in% commercial_classes, 1, 0), na.rm = TRUE),
      muni_PC_inTIF = sum(in_tif, na.rm=TRUE),
      muni_PC_withincents = sum(ifelse(incent_prop == 1, 1, 0), na.rm = TRUE),
      muni_PC_incents_inTIFs = sum(ifelse(incent_prop == 1 & in_tif == 1, 1, 0), na.rm = TRUE),
      muni_PC_claimed_exe = sum(ifelse(exe_total_adj-exe_abate > 0, 1, 0)),
      muni_fmv_incentive = sum(ifelse(incent_prop == 1, fmv, 0), na.rm = TRUE),
      muni_fmv_taxed = sum(taxed_fmv, na.rm=TRUE),
      muni_fmv_inTIF = sum(fmv_inTIF, na.rm=TRUE),
      muni_fmv_exempt = sum((exe_total_adj-exe_abate)/eq_factor/loa, na.rm=TRUE),
      muni_fmv_abated = sum(exe_abate/eq_factor/loa, na.rm = TRUE),
      muni_fmv_tif_increment = sum(fmv_tif_increment, na.rm=TRUE),
      muni_fmv_abates_inTIF = sum(ifelse(incent_prop == 1 & in_tif == 1 & exe_abate >0 , fmv, 0), na.rm = TRUE),
      muni_fmv_incents_inTIF = sum(ifelse(incent_prop == 1 & in_tif == 1, fmv, 0), na.rm = TRUE),
      muni_fmv_untaxable_value = sum(untaxable_value_fmv , na.rm=TRUE),

      muni_fmv = sum(fmv, na.rm=TRUE),
      muni_fmv_c2_res = sum(ifelse(c2_prop == 1, fmv, 0), na.rm = TRUE),
      muni_fmv_residential = sum(ifelse(res_prop==1, fmv, 0), na.rm = TRUE),
      muni_fmv_industrial = sum(ifelse(class %in% industrial_classes, fmv, 0), na.rm = TRUE),
      muni_fmv_indust_incent = sum(ifelse(class %in% industrial_classes & incent_prop == 1, fmv, 0), na.rm = TRUE),
      muni_fmv_commercial = sum(ifelse(class %in% commercial_classes, fmv, 0), na.rm = TRUE),
      muni_fmv_commerc_incent = sum(ifelse(class %in% commercial_classes & incent_prop == 1, fmv, 0), na.rm = TRUE),

      muni_zero_bill = sum(zero_bill, na.rm=TRUE),
      muni_levy = sum(final_tax_to_dist, na.rm=TRUE),
      muni_current_rate_avg = mean(tax_code_rate, na.rm=TRUE),
      muni_min_TC_rate = min(tax_code_rate),
      muni_max_TC_rate = max(tax_code_rate),
      muni_avg_C2_bill_noexe = mean(ifelse(c2_prop == 1 & exe_total_adj == 0, (final_tax_to_dist + final_tax_to_tif), NA), na.rm=TRUE),
      muni_avg_C2_bill_withexe = mean(ifelse(c2_prop == 1 & exe_total_adj > 0, (final_tax_to_dist+ final_tax_to_tif), NA), na.rm=TRUE),
      # muni_median_C2_bill_noexe = median(final_tax_to_dist + final_tax_to_tif)[c2_prop == 1][exe_total_adj == 0],
      # muni_median_C2_bill_withexe = median(final_tax_to_dist + final_tax_to_tif)[c2_prop == 1][exe_total_adj > 0],

      muni_final_tax_to_dist = sum(final_tax_to_dist, na.rm=TRUE),
      muni_final_tax_to_tif = sum(final_tax_to_tif, na.rm=TRUE),
      untaxable_value_av = sum(untaxable_value_av, na.rm=TRUE),
      muni_av = sum(av, na.rm=TRUE),
      muni_av_taxed = sum(taxed_av, na.rm = TRUE),
      muni_av_taxed_commerc = sum(ifelse(class %in% commercial_classes, taxed_av, 0), na.rm = TRUE),
      muni_av_taxed_indust = sum(ifelse(class %in% industrial_classes, taxed_av, 0), na.rm = TRUE),

      muni_eav_taxed = sum(taxed_eav, na.rm=TRUE),
      muni_c2_taxed = sum(ifelse(c2_prop ==1, taxed_eav, 0), na.rm=TRUE),
      muni_res_taxed = sum(ifelse(res_prop ==1, taxed_eav, 0), na.rm=TRUE),
      muni_eav_taxed_commerc = sum(ifelse(class %in% commercial_classes, taxed_eav, 0), na.rm = TRUE),
      muni_eav_taxed_indust = sum(ifelse(class %in% industrial_classes, taxed_eav, 0), na.rm = TRUE),

      exempt_allexemptions_eav = sum(exe_total_adj,na.rm=TRUE),
      exempt_GHE_eav = sum(exe_homeowner, na.rm=TRUE),
      exempt_SR_eav = sum(exe_senior, na.rm=TRUE), # senior exemption
      exempt_FR_eav = sum(exe_freeze, na.rm=TRUE), #  senior freeze exemption
      exempt_LTHO_eav = sum(exe_longtime_homeowner, na.rm=TRUE),
      exempt_DIS_eav = sum(exe_disabled, na.rm=TRUE),
      # combined veteran exemptions and the missing veteran exemptions
      exempt_VET_eav = sum(exe_vet_returning + exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70 + exe_missing_disvet, na.rm=TRUE),
      
      
      total_value_eav = sum(eq_av, na.rm=TRUE),
      untaxable_value_eav = sum(untaxable_value_eav, na.rm=TRUE),
      zero_bills = sum(zero_bill, na.rm=TRUE),
      has_HO_exemp = sum(has_HO_exemp, na.rm=TRUE),
      has_SR_exemp = sum(has_SR_exemp, na.rm=TRUE),
      has_FR_exemp = sum(has_FR_exemp, na.rm=TRUE),
      has_LTHO_exemp = sum(has_LTHO_exemp, na.rm=TRUE),
      has_DI_exemp = sum(has_DI_exemp, na.rm=TRUE),
      has_VR_exemp = sum(has_VR_exemp, na.rm=TRUE),
      has_DV_exemp = sum(has_DV_exemp, na.rm=TRUE),
      has_AB_exemp = sum(has_AB_exemp, na.rm=TRUE)) %>%


    mutate(
      year = year_variable,
      muni_range_TC_rate = muni_max_TC_rate - muni_min_TC_rate,
      muni_effective_rate =  muni_levy / muni_fmv * 100,
      muni_pct_eav_taxed = muni_levy / muni_eav_taxed,

      pct_fmv_taxed = muni_fmv_taxed / muni_fmv,
      pct_fmv_w_incentclass = muni_fmv_incentive / muni_fmv,
      pct_fmv_inTIF = muni_fmv_inTIF / muni_fmv,
      pct_fmv_in_tif_increment = muni_fmv_tif_increment / muni_fmv,
      pct_fmv_untaxable_value = muni_fmv_untaxable_value / muni_fmv,
      pct_fmv_incents_inTIFs = muni_fmv_incents_inTIF / muni_fmv ) %>%
    mutate(across(starts_with("muni_fmv_"), round, digits = 0)) %>%

    mutate(across(contains(c("rate", "pct","bill")), round, digits = 3) ) %>%

  select(year, clean_name, everything())


# bind muni level yearly data together
if(is.data.frame(muni_level_summary)){muni_level_summary <- rbind(muni_level_summary, muni_level_summary2)}else{muni_level_summary <- muni_level_summary2}
rm(muni_level_summary2)


## Muni-MC Summary ---------------------------------------------------------


  muni_MC_summary2 <- pin_data %>%
  left_join(class_dict) |>
    group_by(clean_name, major_class_code)  %>%
  summarize(
    mean_fmv_all = mean(fmv),
    median_fmv_all = median(fmv),
    min_fmv_all = min(fmv),
    quant25_all_fmv = round(quantile(fmv, probs = q[1])),
    quant50_all_fmv = round(quantile(fmv, probs = q[2])),
    quant75_all_fmv = round(quantile(fmv, probs = q[3])),
    max_fmv_all = max(av),
    PC_major_class = n(),
    parcels =n_distinct(parcels),
    
    PC_residential = sum(ifelse(class %in% c(200:399), 1, 0), na.rm = TRUE),
    PC_industrial  = sum(ifelse(class %in% industrial_classes, 1, 0), na.rm = TRUE),
    PC_commercial = sum(ifelse(class %in% commercial_classes, 1, 0), na.rm = TRUE),
    PC_inTIF = sum(in_tif, na.rm=TRUE),
    PC_withincents = sum(ifelse(incent_prop == 1, 1, 0), na.rm = TRUE),
    PC_incents_inTIFs = sum(ifelse(incent_prop == 1 & in_tif == 1, 1, 0), na.rm = TRUE),
    PC_claimed_exe = sum(ifelse(exe_total_adj > 0, 1, 0)),
    fmv_incentive = sum(ifelse(incent_prop == 1, fmv, 0), na.rm = TRUE),
    fmv_taxed = sum(taxed_fmv, na.rm=TRUE),
    fmv_inTIF = sum(fmv_inTIF, na.rm=TRUE),
    fmv_exempt = sum((exe_total_adj-exe_abate)/eq_factor/loa, na.rm=TRUE),
    fmv_abated = sum(exe_abate/eq_factor/loa, na.rm = TRUE),
    fmv_tif_increment = sum(fmv_tif_increment, na.rm=TRUE),
    fmv_abates_inTIF = sum(ifelse(incent_prop == 1 & in_tif == 1 & exe_abate >0 , fmv, 0), na.rm = TRUE),
    fmv_incents_inTIF = sum(ifelse(incent_prop == 1 & in_tif == 1, fmv, 0), na.rm = TRUE),
    fmv_untaxable_value = sum(untaxable_value_fmv , na.rm=TRUE),

    fmv_residential = sum(ifelse(res_prop == 1, fmv, 0), na.rm = TRUE),
    fmv_c2_res = sum(ifelse(c2_prop == 1, fmv, 0), na.rm=TRUE),
    fmv_industrial = sum(ifelse(class %in% industrial_classes, fmv, 0), na.rm = TRUE),
    fmv_indust_incent = sum(ifelse(class %in% industrial_classes & incent_prop == 1, fmv, 0), na.rm = TRUE),
    fmv_commercial = sum(ifelse(class %in% commercial_classes, fmv, 0), na.rm = TRUE),
    fmv_commerc_incent = sum(ifelse(class %in% commercial_classes & incent_prop == 1, fmv, 0), na.rm = TRUE),
    fmv = sum(fmv, na.rm=TRUE), # HAS TO BE NEAR THE BOTTOM so that it doesn't use the summed values in the lines beneath it!!

    zero_bill = sum(zero_bill, na.rm=TRUE),
    levy = sum(final_tax_to_dist, na.rm=TRUE),
    current_rate_avg = mean(tax_code_rate, na.rm=TRUE),
    
    eav_taxed = sum(taxed_eav, na.rm=TRUE),
    eav_c2_taxed = sum(ifelse(c2_prop ==1, taxed_eav, 0), na.rm=TRUE),
    eav_res_taxed = sum(ifelse(res_prop ==1, taxed_eav, 0), na.rm=TRUE),
    eav_taxed_commerc = sum(ifelse(class %in% commercial_classes, taxed_eav, 0), na.rm = TRUE),
    eav_taxed_indust = sum(ifelse(class %in% industrial_classes, taxed_eav, 0), na.rm = TRUE),

    
    min_TC_rate = min(tax_code_rate),
    max_TC_rate = max(tax_code_rate),
    avg_C2_bill_noexe = mean(ifelse(c2_prop == 1 & exe_total_adj == 0, (final_tax_to_dist + final_tax_to_tif), NA), na.rm=TRUE),
    avg_C2_bill_withexe = mean(ifelse(c2_prop == 1 & exe_total_adj > 0, (final_tax_to_dist+ final_tax_to_tif), NA), na.rm=TRUE),
    final_tax_to_dist = sum(final_tax_to_dist, na.rm=TRUE),
    final_tax_to_tif = sum(final_tax_to_tif, na.rm=TRUE),
    zero_bills = sum(zero_bill, na.rm=TRUE),
    
    exempt_allexemptions_eav = sum(exe_total_adj,na.rm=TRUE),
    
    exempt_GHE_eav = sum(exe_homeowner, na.rm=TRUE),
    exempt_SR_eav = sum(exe_senior, na.rm=TRUE), # senior exemption
    exempt_FR_eav = sum(exe_freeze, na.rm=TRUE), #  senior freeze exemption
    exempt_LTHO_eav = sum(exe_longtime_homeowner, na.rm=TRUE),
    exempt_DIS_eav = sum(exe_disabled, na.rm=TRUE),
    # combined veteran exemptions and the missing veteran exemptions
    exempt_VET_eav = sum(exe_vet_returning + exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70 + exe_missing_disvet, na.rm=TRUE),
    
    
    has_HO_exemp = sum(has_HO_exemp, na.rm=TRUE),
    has_SR_exemp = sum(has_SR_exemp, na.rm=TRUE),
    has_FR_exemp = sum(has_FR_exemp, na.rm=TRUE),
    has_LTHO_exemp = sum(has_LTHO_exemp, na.rm=TRUE),
    has_DI_exemp = sum(has_DI_exemp, na.rm=TRUE),
    has_VR_exemp = sum(has_VR_exemp, na.rm=TRUE),
    has_DV_exemp = sum(has_DV_exemp, na.rm=TRUE),
    has_AB_exemp = sum(has_AB_exemp, na.rm=TRUE)) %>%


  mutate(
    year = year_variable,
    total_billed = final_tax_to_dist + final_tax_to_tif,
    range_TC_rate = max_TC_rate - min_TC_rate,
    effective_rate =  levy / fmv * 100,
    pct_eav_taxed = levy / eav_taxed,

    pct_fmv_taxed = fmv_taxed / fmv,
    pct_fmv_w_incentclass = fmv_incentive / fmv,
    pct_fmv_inTIF = fmv_inTIF / fmv,
    pct_fmv_in_tif_increment = fmv_tif_increment / fmv,
    pct_fmv_untaxable_value = fmv_untaxable_value / fmv,
    pct_fmv_incents_inTIFs = fmv_incents_inTIF / fmv ) %>%
  mutate(across(starts_with("fmv_"), round, digits = 0)) %>%

  mutate(across(contains(c("rate", "pct","bill")), round, digits = 3) ) %>%
  select(year, clean_name, major_class_code, everything()) %>%
  setNames(paste0('muni_mc_', names(.)))

    # summarize(
    #   av = sum(av, na.rm = TRUE),
    #   eav = sum(eav, na.rm = TRUE),
    #  eq_av = sum(equalized_av, na.rm = TRUE),
    #   pins_in_muni = n(),
    #
    #   all_exemptions = sum(all_exemptions, na.rm = TRUE),
    #z 
    #   exe_homeowner = sum(exe_homeowner, na.rm=TRUE),
    #   exe_senior = sum(exe_senior, na.rm=TRUE),
    #   exe_freeze = sum(exe_freeze, na.rm=TRUE),
    #   exe_longtime_homeowner = sum(exe_longtime_homeowner, na.rm=TRUE),
    #   exe_disabled = sum(exe_disabled, na.rm=TRUE),
    #   exe_vet_returning = sum(exe_vet_returning, na.rm=TRUE),
    #   exe_vet_dis_lt50 = sum(exe_vet_dis_lt50, na.rm=TRUE),
    #   exe_vet_dis_50_69 = sum(exe_vet_dis_50_69, na.rm=TRUE),
    #   exe_vet_dis_ge70 = sum(exe_vet_dis_ge70, na.rm=TRUE),
    #   exe_abate = sum(exe_abate, na.rm=TRUE),
    #
    #   exe_vet_dis = sum(exe_vet_dis_lt50 + exe_vet_dis_50_69 +
    #                       exe_vet_dis_ge70, na.rm=TRUE),  # all vet_dis variables added together
    #
    #   tax_code_rate = mean(tax_code_rate, na.rm = TRUE),          # Changed from first() to mean() on Nov 1
    #   final_tax_to_dist = sum(final_tax_to_dist, na.rm = TRUE),   # used as LEVY amount!!
    #   final_tax_to_tif = sum(final_tax_to_tif, na.rm = TRUE),     # TIF increment
    #   tax_amt_exe = sum(tax_amt_exe, na.rm = TRUE),
    #   tax_amt_pre_exe = sum(tax_amt_pre_exe, na.rm = TRUE),
    #   tax_amt_post_exe = sum(tax_amt_post_exe, na.rm = TRUE),
    #   rpm_tif_to_cps = sum(rpm_tif_to_cps, na.rm = TRUE), # not used
    #   rpm_tif_to_rpm = sum(rpm_tif_to_rpm, na.rm=TRUE),   # not used
    #   rpm_tif_to_dist = sum(rpm_tif_to_dist, na.rm=TRUE), # not used
    #   tif_share = mean(tif_share, na.rm=TRUE),
    #
    #   zero_bills = sum(zero_bill, na.rm=TRUE),
    #   has_HO_exemp = sum(has_HO_exemp, na.rm=TRUE),
    #   has_SR_exemp = sum(has_SR_exemp, na.rm=TRUE),
    #   has_FR_exemp = sum(has_FR_exemp, na.rm=TRUE),
    #   has_LTHO_exemp = sum(has_LTHO_exemp, na.rm=TRUE),
    #   has_DI_exemp = sum(has_DI_exemp, na.rm=TRUE),
    #   has_VR_exemp = sum(has_VR_exemp, na.rm=TRUE),
    #   has_DV_exemp = sum(has_DV_exemp, na.rm=TRUE),
    #   has_AB_exemp = sum(has_AB_exemp, na.rm=TRUE)
    #
    # ) %>%
    #
    # mutate(total_bill_current = final_tax_to_dist + final_tax_to_tif) %>%
    # rename(cur_comp_muni_rate = tax_code_rate) %>%
    # mutate(current_taxable_eav = final_tax_to_dist/(cur_comp_muni_rate/100),
    #        year = year_variable ) %>%
    # select(year, clean_name, major_class_code, cur_comp_muni_rate, current_taxable_eav, everything()) %>%
    # setNames(paste0('muni_mc_', names(.)))


  # bind muni level yearly data together
  if(is.data.frame(muni_MC_summary)){muni_MC_summary <- rbind(muni_MC_summary, muni_MC_summary2)}else{muni_MC_summary <- muni_MC_summary2}
  rm(muni_MC_summary2)



 }


# Export CSVs ------------------------------------------------------------


write_csv(county_sums, "./Output/ptaxsim_cook_level_2006to2023_new.csv")
write_csv(county_MC_sums, "./Output/ptaxsim_cook_MC_level_2006to2023_new.csv")



write_csv(muni_level_summary, "./Output/ptaxsim_muni_level_2006to2023_new.csv")

write_csv(muni_MC_summary, "./Output/ptaxsim_muni_MC_2006to2023_new.csv")

