### Code for pulling unique tax codes from PTAXSIM ###
### for tax year 2021  
### Identifies the municipality name that taxes each tax code ###
### Joins "clean names", agency_num, etc. to unique tax codes

library(tidyverse)
library(DBI)
library(data.table)
library(httr)
library(ptaxsim)
library(glue)

# Create the DB connection with the default name expected by PTAXSIM functions
# file_path <- "C:/Users/aleaw/"
# 
# if (file.exists(file_path)){
#   ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/aleaw/OneDrive/Documents/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db")
# } else {
#   ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")
# 
# }
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/aleaw/OneDrive/Documents/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db")
#
current_dir = getwd()

if (grepl("website", current_dir)) {
  dots = "../"
} else {
  dots = "../"
}


options(digits=4, scipen = 999)

path <- paste0(dots, "Necessary_Files/muni_shortnames.xlsx")
nicknames <- readxl::read_excel(path)

#taxyear = as.data.frame(taxyear)
#nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")




## `agency_dt` has all taxing agencies (but not TIFs) that existed each year and 
## includes their total taxable base (cty_cook_eav), their levy, taxing rate, 
## binary variables for if a municipality is home rule or not, as well as many 
## other variables. 

## tax_bill() uses this table for the taxable EAV that is used 
## to  calculate the tax rates in the tax bills. For simulations, you must alter 
## the taxable EAV or levy or other variables and then tell tax_bill() function 
## to use the modified agency data table for simulated tax bills.

# has EAV values, extensions by agency_num
agency_dt <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT *
  FROM agency
  WHERE year IN ({taxyear$taxyear*})
      ",
      .con = ptaxsim_db_conn
 ))


# cook_agency_names <- DBI::dbGetQuery(
#   ptaxsim_db_conn,
#   "SELECT DISTINCT agency_num, agency_name
#   FROM agency_info
#   "
# )


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
  AND year IN ({taxyear$taxyear*})
  ",
  .con = ptaxsim_db_conn
  )
)  


#  ## has all tax codes and the composite tax rate for the tax code 
tax_codes <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT tax_code_num, tax_code_rate
  FROM tax_code
  WHERE year IN ({taxyear$taxyear*})
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
  select(-c(minor_type, short_name, 
           # `Column1`, `Most recent reassessed`, 
            agency_number))
