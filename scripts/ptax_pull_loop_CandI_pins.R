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
# ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2024.0.0.db")

# MVH filepath:
# ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "ptaxsim.db")

# Load supplemental files w/ "clean" muni names and detail re: class codes.

class_dict <- read_csv("./Necessary_Files/class_dict_expanded.csv") %>%
  select(class_code, major_class_code)  |>
  mutate(class = as.character(class_code))

options(scipen = 999)  # Park Forest has weird formatting if you don't do this. No, I never figured out WHY it has weird formatting.

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx") |>
  mutate(agency_number = as.character(as.numeric(agency_number)),
    agency_number = str_pad(string = agency_number, width = 9, side = "left", pad = "0"))

# Set years for loop to run.
years <- (2006:2024)


commercial_classes <- c(401:435, 490, 491, 492, 496:499,
  500:535, 590, 591, 592, 597:599,
  700:799,
  800:835, 891, 892, 897, 899)   %>% as.character()

industrial_classes <- c(480:489, 493,
  550:589, 593,
  600:699,
  850:890, 893)  %>% as.character()

is.integer64 <- function(x) {
  class(x) == "integer64"
}

q <- c(.25, .5, .75)


# Distinct PINs ever commercial/industrial -------------------------------

comm_ind_classes <- c(commercial_classes, industrial_classes)

distinct_comm_ind_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "
    SELECT DISTINCT pin
    FROM pin
    WHERE class IN ({comm_ind_classes*})
    ",
    .con = ptaxsim_db_conn
  )
) |>
  mutate(pin = as.character(pin))

comm_ind_pins_all <- NULL


# Loop Start --------------------------------------------------------------

for (i in years) {

  year_variable <- i

  # PTAXSIM tables ---------------------------------------
  comm_ind_pins_year  <- DBI::dbGetQuery(
    ptaxsim_db_conn,
    glue_sql(
      "
    SELECT *
    FROM pin
    WHERE year = {i}
      AND pin IN ({distinct_comm_ind_pins$pin*})
    ",
      .con = ptaxsim_db_conn
    )
  )

  # TIF info ---------------------------------------------------------------

  if (i < 2024) {
    # Old method: tax-code-level TIF distribution
    tif_info <- DBI::dbGetQuery(ptaxsim_db_conn, paste("SELECT * FROM tif_distribution WHERE year = ", i, ";")) |>
      mutate_if(is.integer64, as.double) |>
      transmute(
        year,
        tax_code_num,
        tif_eav = tax_code_eav,
        tif_frozen_eav = tax_code_frozen_eav,
        tif_revenue = tax_code_revenue,
        tif_distribution_pct = tax_code_distribution_pct / 100
      )

  } else {
    # New method: PIN-level TIF distribution
    tif_info <- DBI::dbGetQuery(ptaxsim_db_conn, paste("SELECT * FROM pin_tif_distribution WHERE year = ", i, ";")
    ) |>
      mutate_if(is.integer64, as.double) |>
      transmute(
        year,
        pin,
        tax_code_num,
        tif_eav = pin_eav,
        tif_frozen_eav = pin_frozen_eav,
        tif_revenue = pin_revenue,
        tif_increment_eav = pin_increment_eav,
        tif_distribution_pct = pin_distribution_pct / 100,
        transit_tif_to_cps,
        transit_tif_to_tif,
        transit_tif_to_dist,
        is_transit_tif
      )
  }

  ## Municipality taxing agencies only + Cicero
  muni_agency_names <- DBI::dbGetQuery(
    ptaxsim_db_conn,
    "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'
    "
  )


  agency_dt <- dbGetQuery(ptaxsim_db_conn, paste("SELECT * FROM agency WHERE year = ", i, ";"))
  agency_dt <- agency_dt %>%  mutate_if(is.integer64, as.double)

  tax_codes <- dbGetQuery(ptaxsim_db_conn, paste("SELECT DISTINCT tax_code_num, tax_code_rate FROM tax_code WHERE year = ", i, ";"))

  sql <- "SELECT * FROM tax_code WHERE agency_num IN ({muni_agency_names$agency_num*}) AND year = ?year"
  query <- sqlInterpolate(ptaxsim_db_conn, sql, year = i)
  muni_tax_codes <- dbGetQuery(ptaxsim_db_conn, glue_sql(query, .con = ptaxsim_db_conn)) |>
    select(-year, -agency_rate, -tax_code_rate)



  ## All tax codes.
  ## tax codes within municipalities have additional info
  # Combine tax code information
  tc_muninames <- tax_codes %>%
    left_join(muni_tax_codes, by = c("tax_code_num")) %>%
    left_join(muni_agency_names, by = "agency_num") %>%
    mutate(agency_num = as.character(agency_num)) |>
    left_join(nicknames, by = c("agency_num" = "agency_number"))  |>

    mutate(tax_code_rate = tax_code_rate / 100)


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


  comm_ind_pins_year  <- if (i < 2024) {
    comm_ind_pins_year <- comm_ind_pins_year |>
      mutate_if(is.integer64, as.double) |>
      left_join(ccao_loa, by = c("class" = "class_code")) |>
      left_join(tc_muninames, by = c("tax_code_num")) |>
      left_join(tif_info, by = c("year", "tax_code_num"))
  } else {
    comm_ind_pins_year <- comm_ind_pins_year |>
      mutate_if(is.integer64, as.double) |>
      left_join(ccao_loa, by = c("class" = "class_code")) |>
      left_join(tc_muninames, by = c("tax_code_num")) |>
      left_join(tif_info, by = c("year", "pin", "tax_code_num"))
  }

  comm_ind_pins_year  <- comm_ind_pins_year  |>
    mutate(tif_distribution_pct = ifelse(is.na(tif_distribution_pct), 0, tif_distribution_pct)) |>
    mutate(
      incent_prop = ifelse(between(class, 600, 899), 1, 0),
      res_prop = ifelse(between(class, 200, 399), 1, 0),
      c2_prop = ifelse(between(class, 200, 299), 1, 0),
      parcels = str_sub(pin, 1, 10),
      in_tif = ifelse(tax_code_num %in% tif_info$tax_code_num, 1, 0),
      in_tif_andpays_revtotif = ifelse(in_tif == 1 & tif_eav > tif_frozen_eav, 1, 0),
    ) |>

    mutate(
      eq_av = av_clerk * eq_factor,
      exe_total_old = rowSums(across(starts_with("exe_"))),
      exe_total_old = ifelse(exe_total_old > eq_av, eq_av, exe_total_old),

      # create variables that appear from taxbill() function
      taxed_eav_old = av_clerk * eq_factor - exe_total_old,      # but exe_total was missing some exemptions.

      flag_missingdata = ifelse(taxed_eav_old > 1000 & tax_bill_total == 0 & c2_prop == 1, 1, 0)) |>

    mutate(exe_missing_disvet = ifelse(taxed_eav_old > 1000 & tax_bill_total == 0 & c2_prop == 1, taxed_eav_old, 0)) |>

    mutate(
      taxed_eav_adj = ifelse(taxed_eav_old > 1000 & flag_missingdata == 1, 0, taxed_eav_old),
      total_taxed_eav_AWM = tax_bill_total / tax_code_rate,  # EAV that was taxed by TIFs and taxing districts
      taxed_eav_TIFincrement = total_taxed_eav_AWM * tif_distribution_pct,
      taxed_eav_nonTIF = total_taxed_eav_AWM * (1 - tif_distribution_pct)) |>

    mutate(
      exe_total_adj = rowSums(across(starts_with("exe_"))) - exe_total_old,

      tax_amt_exe = exe_total_adj * tax_code_rate,    # calculate tax bill reduction in dollars. Exempt EAV * current tax rate. "Naive" tax savings.
      tax_amt_pre_exe = av_clerk * eq_factor * tax_code_rate,
      tax_amt_post_exe = tax_amt_pre_exe - tax_amt_exe,
      tax_amt_post_exe = ifelse(tax_amt_post_exe < 0, 0, tax_amt_post_exe),

      final_tax_to_tif = taxed_eav_TIFincrement * tax_code_rate,
      final_tax_to_dist = taxed_eav_nonTIF * tax_code_rate,


      # NOTE: the number of $0 tax bills identified when using the tax_bill() command from ptaxsim is different than using the tax bill total value directly from the pin db table
      zero_bill = ifelse(tax_bill_total == 0, 1, 0),

      # for A and B property types of commercial and industrial properties
      class_1dig = str_sub(class, 1, 1),
      class_group = case_when(
        (class_1dig == 5 & class %in% commercial_classes) ~ "5A",
        (class_1dig == 5 & class %in% industrial_classes) ~ "5B",
        class_1dig == 7 &  class < 742 ~ "7A",
        class_1dig == 7 &  class >= 742 ~ "7B",
        (class_1dig == 8 & class %in% commercial_classes) ~ "8A",
        (class_1dig == 8 & class %in% industrial_classes) ~ "8B",
        TRUE ~ as.character(class_1dig)))


  # Create other variables used in summary files -----------------------------
  comm_ind_pins_year  <- comm_ind_pins_year  |>

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
      # taxed_fmv = ifelse(is.nan(taxed_fmv), 0, taxed_fmv),

      fmv = av_clerk / loa,
      fmv = ifelse(is.na(fmv), 0, fmv),
      ## untaxable value = exempt EAV from abatements and exemptions
      untaxable_value_eav = exe_total_adj +

        ## TIF increment EAV above frozen EAV, which becomes TIF revenue
        (final_tax_to_tif /  tax_code_rate) +

        ## difference between 25% and reduced level of assessment for incentive class properties. Excludes TIF increment when calculating the difference!
        ifelse(incent_prop == 1, (taxed_av / loa * 0.25 - taxed_av) * eq_factor, 0),

      #  manually adjust untaxable value of class 239 properties
      untaxable_value_eav = ifelse(class == 239,
        eq_av - taxed_eav, untaxable_value_eav),

      untaxable_value_av = untaxable_value_eav / eq_factor,
      untaxable_value_fmv = untaxable_value_av / loa,
      untaxable_value_fmv = ifelse(is.nan(untaxable_value_av), 0, untaxable_value_av),

      exempt_eav_inTIF = ifelse(in_tif == 1,
        exe_total_adj, 0),
      exempt_eav = exe_total_adj,
      exempt_fmv = exempt_eav / eq_factor / loa,

      fmv_inTIF = ifelse(in_tif == 1,
        av / loa, 0),
      fmv_tif_increment = ifelse(final_tax_to_tif > 0,
        ((final_tax_to_tif / (tax_code_rate)) / eq_factor) / loa, 0),

      fmv_incents_inTIF = ifelse(incent_prop == 1 & in_tif == 1,
        fmv, 0),
      fmv_incents_tif_increment = ifelse(incent_prop == 1 & final_tax_to_tif > 0,
        ((final_tax_to_tif / (tax_code_rate)) / eq_factor) / loa, 0),
      eav_incents_inTIF = fmv_incents_inTIF * loa * eq_factor
    ) %>%
    select(tax_code_num, class, pin, taxed_fmv,
      untaxable_value_fmv, fmv_inTIF, fmv_tif_increment, fmv_incents_tif_increment, fmv, tax_bill_total, final_tax_to_dist, final_tax_to_tif, tax_code_rate, taxed_eav, eq_av, av, everything())


  if (is.data.frame(comm_ind_pins_all)) {
    comm_ind_pins_all <- bind_rows(comm_ind_pins_all, comm_ind_pins_year)
  } else {
    comm_ind_pins_all <- comm_ind_pins_year
  }

  rm(comm_ind_pins_year)
}



# Export CSVs ------------------------------------------------------------


write_csv(
  comm_ind_pins_all,
  "./Output/comm_ind_PINs_ever_2006to2024.csv"
)
