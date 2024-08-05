# File & Loop Prep ----------------------------------------
## AWM & MVH ##
## 12-30-23 ##

library(tidyverse)
library(DBI)
library(data.table)
library(gstat)
library(here)
library(httr)
library(jsonlite)
library(ptaxsim)
#library(stars)
library(glue)

# Create the DB connection with the default name expected by PTAXSIM functions

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

# MVH filepath:
# ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "ptaxsim.db")

# Load supplemental files w/ "clean" muni names and detail re: class codes.

class_dict <- read_csv("./Necessary_Files/class_dict_expanded.csv") %>% select(-loa_2022)


nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")

# Set years for loop to run.

years <-(2006:2022)

# Create empty dataframe for the loop to populate.

muni_class_summary <- NULL

# Define commercial and industrial classes

commercial_classes <- c(401:435, 490, 491, 492, 496:499,
                        500:535,590, 591, 592, 597:599,
                        700:799,
                        800:835, 891, 892, 897, 899)   %>% as.character()

industrial_classes <- c(480:489,493,
                        550:589, 593,
                        600:699,
                        850:890, 893)  %>% as.character()

# function to fix ptaxsim.db output

is.integer64 <- function(x){
  class(x)=="integer64"
}

# quartiles...

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


  agency_dt<- dbGetQuery(ptaxsim_db_conn,
                         paste('SELECT *
                               FROM agency
                               WHERE year = ', i, ';'))

  agency_dt <- agency_dt |>
    mutate_if(is.integer64, as.double)

  tax_codes <- dbGetQuery(ptaxsim_db_conn,
                          paste('SELECT DISTINCT tax_code_num, tax_code_rate
                                FROM tax_code
                                WHERE year = ', i, ';'))

  sql <- "SELECT *
  FROM tax_code
  WHERE agency_num IN ({muni_agency_names$agency_num*})
  AND year = ?year"

  query <- sqlInterpolate(ptaxsim_db_conn, sql, year = i)

  muni_tax_codes<- dbGetQuery(ptaxsim_db_conn, glue_sql(query, .con = ptaxsim_db_conn))

  tif_distrib <- DBI::dbGetQuery(ptaxsim_db_conn,
                                 paste('SELECT *
                                       FROM tif_distribution
                                       WHERE year = ', i, ';'))


  ## All tax codes within municipalities have additional info (ask Alea)
  tc_muninames <- tax_codes %>%
    left_join(muni_tax_codes) %>%
    left_join(muni_agency_names) %>%
    select(-agency_rate) %>%
    left_join(nicknames) %>%
    select(-c(minor_type, short_name,
              agency_number))


  sql <- "SELECT DISTINCT pin, class, tax_code_num
  FROM pin
  WHERE tax_code_num IN ({tax_codes$tax_code_num*})
  AND year = ?year"

  query <- sqlInterpolate(ptaxsim_db_conn, sql, year = i)

  cook_pins <- dbGetQuery(ptaxsim_db_conn, glue_sql(query, .con = ptaxsim_db_conn))

  # combine taxing agency names and agency type to data table that has eav and extension values

  agency_data <- right_join(agency_dt, muni_agency_names) %>%
    select(-c(cty_dupage_eav:cty_livingston_eav)) %>% # drop several unused variables
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


  #  billed properties with 14-digit PINs
  pin14_bills <- taxbills %>%
    mutate_if(is.integer64, as.double) %>%
    group_by(tax_code, class, pin) %>%

    mutate(total_bill = final_tax_to_dist + final_tax_to_tif, # from each taxing agency
           eq_av = av/eq_factor) %>%

    group_by(tax_code, class, pin) %>%

    summarize(total_billed = sum(total_bill, na.rm = TRUE), # total on someone's property tax bill
              av = first(av),            # PINs appear multiple times, uses first av appearance
              eq_av = first(eq_av),      # equalized AV = potential taxable value for homeowners
              eav = first(eav),          # EAV after exemptions are subtracted.
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

  ## Add tax code tax rate to PIN level data
  joined_pin_data <- left_join(joined_pin_data, tc_muninames, by = c("tax_code" = "tax_code_num"))



  # Summarize ---------------------------------------------------------------
  joined_pin_data <- joined_pin_data %>%
    mutate(class_group = str_sub(class, 1,1),
           class_group = case_when(
             (class_group == 5 & class %in% commercial_classes) ~ "5A",
             (class_group == 5 & class %in% industrial_classes) ~ "5B",
             class_group == 7 &  class < 742 ~ "7A",
             class_group == 7 &  class >= 742 ~ "7B",
             (class_group == 8 & class %in% commercial_classes ) ~ "8A",
             (class_group == 8 & class %in% industrial_classes ) ~ "8B",
             TRUE ~ as.character(class_group))) %>%
    mutate(
      # taxing district revenue = taxable eav * tax rate so rearrange the formula:
      taxed_eav = final_tax_to_dist / tax_code_rate*100,
      total_value_eav = (final_tax_to_dist + final_tax_to_tif)/ tax_code_rate * 100 + all_exemptions + abatements,

      taxed_av =  taxed_eav / eq_factor, # current value that taxing agencies can tax for their levies

      ## taxable AV = equalized assessed value net TIF increments, gross exemptions.
      ## Used for calculating untaxable value further below
      taxable_av = (final_tax_to_dist / tax_code_rate *100 + all_exemptions + abatements)/ eq_factor,

      ## FMV * assessment rate = AV
      taxed_fmv = taxed_av / loa,
      #taxed_fmv = ifelse(is.nan(taxed_fmv), 0, taxed_fmv),

      fmv = av / loa,
      fmv = ifelse(is.na(fmv), 0, fmv),

      ## untaxable value = exempt EAV from abatements and exemptions
      untaxable_value_eav = all_exemptions + abatements +

        ## TIF increment EAV above frozen EAV, which becomes TIF revenue
        (final_tax_to_tif /  tax_code_rate*100) +

        ## difference between 25% and reduced level of assessment for incentive class properties. Excludes TIF increment when calculating the difference!
        ifelse(between(class, 600, 900),
               (taxable_av - taxed_av)*eq_factor, 0),

      #  manually adjust untaxable value of class 239 properties
      untaxable_value_eav = ifelse(class == 239,
                                   equalized_av-taxed_eav, untaxable_value_eav),

      untaxable_value_av = untaxable_value_eav / eq_factor,
      untaxable_value_fmv = untaxable_value_av / loa,
      untaxable_value_fmv = ifelse(is.nan(untaxable_value_av), 0, untaxable_value_av),

      exempt_eav_inTIF = ifelse(in_tif == 1,
                                all_exemptions, 0),
      exempt_eav= all_exemptions + abatements,
      exempt_fmv = exempt_eav / eq_factor / loa,
      #  exempt_fmv = ifelse(is.nan(exempt_fmv), 0 , exempt_fmv),


      fmv_inTIF = ifelse(in_tif==1,
                         av/loa, 0),
      # fmv_inTIF = ifelse(is.nan(fmv_inTIF), 0 , fmv_inTIF),

      fmv_tif_increment = ifelse(final_tax_to_tif > 0,
                                 ((final_tax_to_tif / (tax_code_rate/100)) / eq_factor ) / loa, 0),
      #    fmv_tif_increment = ifelse(is.nan(fmv_tif_increment), 0 , fmv_tif_increment),


      fmv_incents_inTIF = ifelse(between(class, 600, 900) & in_tif == 1,
                                 fmv, 0),
      fmv_incents_tif_increment = ifelse(between(class, 600, 900) & final_tax_to_tif > 0 ,
                                         ((final_tax_to_tif / (tax_code_rate/100)) / eq_factor ) / loa, 0),
      eav_incents_inTIF = fmv_incents_inTIF * loa * eq_factor
    ) %>%
    select(tax_code, class, pin, taxed_fmv,
           untaxable_value_fmv, fmv_inTIF, fmv_tif_increment, fmv_incents_tif_increment, fmv, total_billed, final_tax_to_dist, final_tax_to_tif, tax_code_rate, eav, equalized_av, av, everything())

  joined_pin_data <- joined_pin_data %>%
    mutate(loa = ifelse(loa==0, NA, loa))
}

# Remove, rename, and reorder variables----------------------------------

df <- joined_pin_data |>
  select(year = year.x, pin, tax_code, agency_num, clean_name, tax_code_rate, class, in_tif, fmv, taxed_fmv, untaxable_value_fmv, fmv_inTIF,
         fmv_tif_increment, fmv_incents_tif_increment, total_billed, final_tax_to_dist,
         final_tax_to_tif, eav, av, taxing_agency_count, tif_share, propclass_1dig, assess_ratio,
         improvement_ind, vacant_ind, land, land_use = Alea_cat, loa, triad = Triad, fmv_incents_inTIF)

df |>
  select(clean_name) |>
  filter(is.na(clean_name)) |>
  group_by(tax_code) |>
  reframe(tax_code, clean_name, n = n()) |>
  arrange(desc(n)) |>
  distinct()


# Export CSV ------------------------------------------------------------

write_csv(joined_pin_data, "./apsa/apsa_df.csv")
