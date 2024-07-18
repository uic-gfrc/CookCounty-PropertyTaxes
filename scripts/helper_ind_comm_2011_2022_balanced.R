### Helper File: Generate Balanced Panel ###
### Commercial and Industrial PINs in Cook County
### Years 2011 - 2022 ###

# Goals-----------------------------------------------------

# 1. Identify ALL PINs that were EVER an industrial or commercial property between 2011-2022
# 2. Remove border-crossers
# 3. Ensure PINs in unincorporated areas are omitted from analysis
# 4. Convert remaining observations into a balanced panel

# Pre-prep ------------------------------------------------------

##  Load packages -----------------------------------------------

library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)

# No one likes scientific notation

options(scipen = 999)

## Create function to modify ptaxsim.db output -------------------

is.integer64 <- function(x){
  class(x)=="integer64"
}

## Define industrial and commercial properties-------------------

commercial_classes <- c(401:435, 490, 491, 492, 496:499,
                        500:535,590, 591, 592, 597:599,
                        700:799,
                        800:835, 891, 892, 897, 899
)

industrial_classes <- c(480:489,493,
                        550:589, 593,
                        600:699,
                        850:890, 893
)

## Define county border-crossers

cross_county_lines <- c("030440000", "030585000", "030890000", "030320000", "031280000",
                        "030080000", "030560000", "031120000", "030280000", "030340000",
                        "030150000","030050000", "030180000","030500000", "031210000")

## Instantiate DB connection. ------------------------------------------------------

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2022.0.0.db")

# Pull and Prep Data -----------------------------------------------------

## Import necessary files-----------------------------------------------------

### Expanded class dictionary-----------------------------------------------------

cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  mutate(class = as.character(class_code)) |>  # rename to match other data frames
  select(-c(loa_2022, Option2, class_desc, land, vacant_ind, last2dig,
            Res_nonRes, assessment_level, used_in2021, class_code)) %>%
  mutate_at(.vars = c("improvement_ind", "incent_prop", "class_1dig", "major_class_code"), .funs = as.character
  )

### Levels of assessment (which change over time) -----------------------------------------------------

ccao_loa <- read_csv("./inputs/ccao_loa.csv") %>%
  mutate(class = as.character(class_code)) %>%
  filter(year > 2005) %>%
  select(-class_code) %>%
  mutate(loa = as.numeric(loa)) %>%
  mutate(loa = ifelse(loa == 0, NA, loa) # avoid dividing by zero error
  )

### Nicknames-----------------------------------------------------

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")  %>%
  select(agency_number, clean_name, Triad, Township) %>%
  mutate(agency_number = str_pad(agency_number, width = 9, side = "left", pad = "0")) %>%
  mutate(agency_number = as.character(agency_number))

## Access PTAXSIM.DB-----------------------------------------------------

## Identify all municipalities AND DON'T FORGET CICERO

muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'
    "
)

## Identify all taxcodes that go with those municipalities

tax_codes_muni <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT year, agency_num, tax_code_num, tax_code_rate
  FROM tax_code
  WHERE agency_num IN ({muni_agency_names$agency_num*}
  )
  ",
           .con = ptaxsim_db_conn
  ))

## Use those tax code numbers to pull all ind/comm PINs in time frame 
## within municipality tax codes

muni_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT year, pin, class, tax_code_num
  FROM pin
  WHERE class > 399 AND class < 900
  AND tax_code_num IN ({tax_codes_muni$tax_code_num*})
  ",
    .con = ptaxsim_db_conn
  ))

## Pull in clean names for munis
# 
# tax_codes_muni <- tax_codes_muni %>%
#   left_join(nicknames, by = c("agency_num" = "agency_number"))

### Identify all distinct comm-ind PINs between 2011-2022--------------------

distinct_pins_2011_2022 <- muni_pins |>
  select(pin) |>
  distinct(pin)

## Pull all observations for those PINs between 2011-2022

comm_ind_pins_2011_2022_raw <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT year, pin, class, tax_code_num, tax_bill_total, av_mailed, av_certified, av_board, av_clerk, exe_abate
   FROM pin
   WHERE pin IN ({distinct_pins_2011_2022$pin*}) AND
   year >= 2011 
  ",
    .con = ptaxsim_db_conn
  )) |>
  mutate_if(is.integer64, as.double) %>%
  mutate(class = as.character(class)) |>
  left_join(cde, by = "class") |>
  left_join(ccao_loa, by = c("year", "class")) |>
  mutate(comparable_props = as.character(comparable_props)
  )

## List of tax codes that are in TIFs and the proportion of value in the tax_code that goes to the TIF

tif_distrib <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
    "SELECT DISTINCT year, tax_code_num, tax_code_distribution_pct
  FROM tif_distribution
  ",
    .con = ptaxsim_db_conn)
)

# At this point, we have 1,284,971 mil PIN-Year obs.
# 

# Manipulate Extracted Data ---------------------------------------------

comm_ind_pins_2011_2022 <- comm_ind_pins_2011_2022_raw %>%
  rename(land_use = Alea_cat) |>
  arrange(pin) %>%
  left_join(tax_codes_muni, by = c("year", "tax_code_num")) %>%
  left_join(tif_distrib) %>%
  mutate(
    has_AB_exemp = as.character(ifelse(exe_abate > 0, 1, 0)),
    fmv = av_clerk / loa,
    fmv_NA_indicator = ifelse(is.na(fmv),1,0),
    fmv = ifelse(is.na(fmv), 0, fmv),
    in_tif = as.character(ifelse(tax_code_num %in% tif_distrib$tax_code_num, 1, 0)),
    class_group = str_sub(class, 1,1),
    class_group = case_when(
      (class_group == 5 & class %in% commercial_classes) ~ "5A",
      (class_group == 5 & class %in% industrial_classes) ~ "5B",
      class_group == 7 &  class < 742 ~ "7A",    # commercial developments less than $2 million
      class_group == 7 &  class >= 742 ~ "7B",   # commercial developments greater than $2 million
      (class_group == 8 & class %in% commercial_classes ) ~ "8A",
      (class_group == 8 & class %in% industrial_classes ) ~ "8B",
      TRUE ~ as.character(class_group))
  )

# write_csv(comm_ind_pins_ever, "./Output/comm_ind_inmunis_timeseries_2011to2022.csv")



# # Create 2006 PIN level Panel Data ----------------------------------------
#
# comm_ind_pins_2006 <- comm_ind_pins_ever |>
#   group_by(pin) |>
#   mutate(
#     years_existed = n(),
#     incentive_years = sum(incent_prop == "Incentive"),
#     landuse_change =
#       ifelse(sum(land_use == "Commercial") == 17, "Always Commercial",
#              ifelse(sum(land_use == "Industrial") == 17, "Always Industrial",
#                     "Changes Land Use" )),
#     base_year_fmv_2006 = ifelse(min(year)==2006, fmv[year == 2006], NA),
#     fmv_growth_2006 = fmv/base_year_fmv_2006,
#   )  %>%
#   ungroup() %>%
#   mutate(incent_change = case_when(
#     incentive_years == 17 ~ "Always Incentive",
#     incentive_years == 0 ~ "Never Incentive",
#     TRUE ~ "Changes Sometime")
#   )
#
# ## Examine dropped PINs from 2006 to 2022 panel data -----------------------
#
#
# ## View the PINs that will be dropped in future step
# ## 219,187 PINs are will be dropped from the 2006-2022 panel data
# dropped_pins1 <- comm_ind_pins_2006 %>%
#   filter(
#     years_existed < 17  |      ## 196,780 PINs do not exist all 17 years
#       is.na(clean_name)  |       ## 6,837 PINs do not have municipality names
#       agency_num %in% cross_county_lines   ## 19,554 PINs located in Municipalities that have a majority of their EAV in other counties
#   )
#
# write_csv(dropped_pins1, "dropped_frompanel_2006to2022.csv")
#
# ### Drop PINs and export File -----------------------------------------------
#
#
# ## This step "balances the panel" - all PINs left in the sample exist during every year.
# ## Also drops PINs in unincorporated areas of Cook County
# comm_ind_pins_2006 <- comm_ind_pins_2006 %>%
#   filter(
#     years_existed == 17  &
#       !is.na(clean_name)  &
#       !agency_num %in% cross_county_lines
#   )
# ## 1,598,968 obs remain July 12 2024 - AWM
#
#
# ## 96,193 PINs exist every year - old
# ## 95,355 PINs as of July 11 2024
# ## 94,243 PINs as of July 12
# comm_ind_pins_2006 %>%
#   select(year, land_use, incent_prop, fmv_growth_2006) %>%
#   filter(year == 2022)
#
# write_csv(comm_ind_pins_2006, "./Output/comm_ind_PINs_2006to2022_balanced.csv")
#
#
#
# ## Unique PINs and their property classes over time ------------------------------------
#
# pin_classes <- comm_ind_pins_ever %>%
#   group_by(clean_name, pin, class) %>%
#   summarize(count = n(),
#             first_year = first(year),
#             last_year = last(year)) %>%
#   ungroup() %>%
#   arrange(pin, first_year)
#
#
# unique_ptax_w_class <- pin_classes %>% group_by(pin) %>%
#   mutate(var2 = cumsum(row_number() == 1 | (class != dplyr::lag(class))))
#
#
# unique_ptax_wide <- unique_ptax_w_class %>%
#   pivot_wider(id_cols = c("pin", "clean_name"),
#               names_from = var2,
#               values_from = c(class, count, first_year, last_year))
#
# ## 119,993 Unique PINs and any class change they experienced:
# write_csv(unique_ptax_wide, "./Output/pin_class_changes.csv")


# Create 2011-2022 PIN level Panel Data -----------------------------------

df <- comm_ind_pins_2011_2022 %>%
  group_by(pin) |> # 113,255 pin-muni combos
  arrange(desc(year)) |>
  mutate(multi_muni = n_distinct(clean_name),
         multimuni_ind = ifelse(sum(multi_muni) > 1, 1, 0)) |>  
  
  
  ## fill in muni names for the 700+ PINs that had multiple muni names or did not have a muni name for all years
  ## 471 were the Harvey/Markham Amazon PINs.
  mutate(clean_name = first(clean_name, na_rm = TRUE)) %>% 
 
   mutate(
     multi_muni = sum(n_distinct(clean_name)),
    years_existed = n(),
    incentive_years = sum(incent_prop == "Incentive"),
    tif_years = sum(in_tif==1),
    landuse_change =
      ifelse(
        sum(land_use == "Commercial") == 12, "Always Commercial",
        ifelse(sum(land_use == "Industrial") == 12, "Always Industrial",
               # some properties had an incentive class before 2011 and then were tax exempt. Dropped from panel.
               ifelse(sum(land_use == "Exempt") == 12, "Drop Me",   # created to remove PINs if they were tax exempt every year between 2011 and 2022.
                      "Changes Land Use" ))),
    incent_change = case_when(
      incentive_years == 12 ~ "Always Incentive",
      incentive_years == 0 ~ "Never Incentive",
      TRUE ~ "Changes Sometime"),
    tif_change = case_when(
      tif_years == 12 ~ "Always TIF",
      tif_years == 0 ~ "Never TIF",
      TRUE ~ "Changes")
  ) |>
  ungroup() |>
  # keep pins from Munis that have > 50% of their taxed EAV within Cook County
  filter(!(agency_num %in% cross_county_lines))   



# Deal with PINs that had multiple muni names -----------------------------

# df |>
#   group_by(years_existed) |>
#   summarize(n = n())
# 
# df |> distinct(pin) # 112,459 unique pins
# 
# df |> distinct(pin, clean_name) # 113,255 pin-muni combos before using first(clean_name)
# 
# df |> 
#   group_by(pin) |> # 113,255 pin-muni combos
#   mutate(multi_muni = n_distinct(clean_name) ) |>  
#   arrange(desc(multi_muni)) |> 
#   filter(multi_muni > 1)|> 
#   distinct(pin, clean_name
#            )
# 
# muni_swappers <- df |> 
#   group_by(pin) |> # 113,255 pin-muni combos
#   mutate(multi_muni = n_distinct(clean_name) ) |>  
#   arrange(desc(multi_muni)) |> 
#   filter(multi_muni > 1)|> 
#   select(pin, year, clean_name
#   ) |> 
#   pivot_wider(id_cols = "pin", names_from = "year", values_from = "clean_name")
# # 793 PINs have more than 1 muni?
# 
# df |> 
#   group_by(pin) |> # 113,255 pin-muni combos
#   mutate(multi_muni = n_distinct(clean_name) ) |>  
#   arrange(desc(multi_muni)) |> 
#   filter(multi_muni > 1)|> 
#   ungroup() |>
#   distinct(clean_name)
# 
# df |> 
#   group_by(pin) |> # 113,255 pin-muni combos
#   arrange(desc(year)) |>
#   mutate(clean_name = first(clean_name)) %>%
#   distinct(pin,clean_name)

# Balance Panel --------------------------




### We now have 1,271,065 mil PIN-Year obs. in our sample after RD decisions
### We expect to drop all but 1,202,378

## Drop PINs that don't exist all years --> 68678 PIN-year obs. are dropped

dropped_pins <-  df |>
  filter(years_existed < 12)

write.csv(dropped_pins, "dropped_pins_balance_2011_2022.csv")

## We retain 1.2 obs.

missingnames <- df_balanced |>
  filter(is.na(clean_name))

df_balanced <- df_balanced |>
  filter(!is.na(clean_name)) |>    # drops 252 MORE obs.
  group_by(pin)  |> 
  mutate(years_existed2 = n()) |>  # check again to see if they still exist every year
  filter(years_existed2 == 12)     # drops more observations now!! order of filtering matters! 
# 1,202,040 observations now that exist every year

# Write Balanced File -----------------------------------------------

df_balanced <- df_balanced %>% select(pin, year, av_clerk, fmv)

panel <- pdata.frame(df_balanced, index = c("pin", "year"))

is.pbalanced(panel)

write_csv(df_balanced, "balanced_panel_2011_2022.csv")


# Test missingness

install.packages("finalfit")
library(finalfit)

dropped_pins |>
  group_by(year) |>
  summarize(fmv = sum(fmv))

df_balanced |>
  group_by(year) |>
  summarize(sum(fmv))

## Are things coded right?

ff_glimpse(df_balanced)

## Turn to panel data

library(plm)
library(prediction)

panel <- pdata.frame(df_balanced, index = c("clean_name", "year"))

is.pbalanced(panel)

df_balanced |>
  summarize(is.na(clean_name)) |>
  group_by(year) |>
  summarize(n = n())

fe_naive <- plm(fmv ~ incent_prop,
                model = "within",
                effect = "twoways",
                data = panel)

