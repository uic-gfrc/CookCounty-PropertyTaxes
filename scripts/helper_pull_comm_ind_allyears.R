library(tidyverse)
library(ptaxsim)
library(DBI) 
library(glue)

# Instantiate DB connection.

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

# Read in class dictionary
cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  mutate(class = as.character(class_code)) |>  # rename to match other data frames
  select(-c(loa_2022, class_desc, land, last2dig, Res_nonRes, assessment_level, used_in2021, class_code))
  

# Bring in the Level of Assessments for each year. They have changed over time!! 
ccao_loa <- read_csv("./inputs/ccao_loa.csv") %>% 
  mutate(class = as.character(class_code)) %>%
  filter(year > 2005) %>% 
  select(-class_code)


## Pull Muni Taxing Agency Names from agency_info table
muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'
    "
)


# muni_tax_codes <- DBI::dbGetQuery(
#   ptaxsim_db_conn,
#   glue_sql("
#   SELECT year, agency_num, tax_code_num
#   FROM tax_code
#   WHERE agency_num IN ({muni_agency_names$agency_num*})
#   ",
#            .con = ptaxsim_db_conn
#   ))

# # Identify tax codes associated with relevant agencies
# Pull in the tax code rates
tax_codes <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT year, agency_num, tax_code_num, tax_code_rate
  FROM tax_code
  WHERE agency_num IN ({muni_agency_names$agency_num*})
  ",
           .con = ptaxsim_db_conn
  )
)

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx") 

## Pulls ALL distinct PINs that existed between 2006 and 2022.
## Syntax: "*" means "all the things" "pin" references the table w/in PTAXSIM DB
        ## OLD COMMENT: Takes a while to run. (~1 min w/ 64GB RAM or 28GB M2 Chip)
        ## OLD COMMENT: ~31.47 million obs. when including all PINs each year (PIN-YEAR combos)
## 1,661,125 PINs when only including classes 400 to 899
## Change to numeric to merge w/ CDE

cook_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT year, pin, class, tax_code_num, tax_bill_total, av_mailed, av_certified, av_board, av_clerk, exe_abate
  FROM pin
  WHERE class > 399 
  AND class < 900
  ",
  .con = ptaxsim_db_conn
  )) 




# 119k distinct pins were a commercial or industrial property for at least 1 year
distinct_pins <- cook_pins |>
  select(pin) |>
  distinct(pin)

# 3124 distinct tax codes
# distinct_tc <- comm_ind_pins |>
#   select(tax_code_num) |>
#   distinct()

## But we want those PINs as obs. for all years, even if they weren't classified
## as an industrial or commercial property every year.

## Use unique PIN list to get all obs. for all years they existed.
## ~1.8mil PINs
## Note: Alea_cat is a variable created by Alea in the class_dict file that indicates land use type.
## Land use type is based off of the description provided for property classes.


## Excluding the Exemption variables except for abatements.
comm_ind_pins_ever <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT year, pin, class, tax_code_num, tax_bill_total, av_mailed, av_certified, av_board, av_clerk, exe_abate
   FROM pin
   WHERE pin IN ({distinct_pins$pin*})
  ",
  .con = ptaxsim_db_conn
  )) |>
  mutate(class = as.character(class)) |>
  left_join(cde, by = "class") |> 
  left_join(ccao_loa, by = c("year", "class")) |>
  mutate(comparable_props = as.character(comparable_props))



tif_distrib <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT *
  FROM tif_distribution
  ", 
    .con = ptaxsim_db_conn)
  )

## Merge PIN data with muni names

# comm_ind_tc_names <- comm_ind_tc_ever |>
#   filter(agency_num %in% muni_agency_names$agency_num) |>
#   rename(agency_number = agency_num) |>
#   mutate(agency_number = as.numeric(agency_number)) |>
#   left_join(nicknames)

comm_ind_pins_ever <- comm_ind_pins_ever |>
  left_join(comm_ind_tc_names, by = c("year", "tax_code_num"))

comm_ind_pins_ever <- comm_ind_pins_ever |>
  as.data.frame()

## Write CSV to Output Folder

# write_csv(comm_ind_pins_ever, "./Output/comm_ind_PINs_2006-2022.csv")

write_csv(com_ind_descriptives, "./Output/com_ind_descriptives.csv", na = "")