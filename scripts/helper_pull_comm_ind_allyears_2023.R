### Helper File: Pull all Comm. and Ind. PINs in Cook County ###
### Years 2006 - 2022 ###
### Two multiple CSVs:
####   one has all PINs all years, one only has PINs that existed each year.

library(tidyverse)
library(ptaxsim)
library(DBI)
library(glue)


# Pull and Prep Data ------------------------------------------------------

is.integer64 <- function(x){
  class(x)=="integer64"
}

# Instantiate DB connection.

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")

# Read in class dictionary
cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  mutate(class = as.character(class_code)) |>  # rename to match other data frames
  select(-c(loa_2022, Option2, class_desc, land, vacant_ind, last2dig,
            Res_nonRes, assessment_level, used_in2021, class_code)) %>%
  mutate_at(.vars = c("improvement_ind", "incent_prop", "class_1dig", "major_class_code"), .funs = as.character
  )

# Bring in the Level of Assessments for each year. They have changed over time!!
ccao_loa <- read_csv("./inputs/ccao_loa.csv") %>%
  mutate(class = as.character(class_code)) %>%
  filter(year > 2005) %>%
  select(-class_code) %>%
  mutate(loa = as.numeric(loa)) %>%
  mutate(loa = ifelse(loa == 0, NA, loa) # avoid dividing by zero errors
         )

# Cook County agency number is 010010000


## Pull Muni Taxing Agency Names from agency_info table
muni_agency_names <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT DISTINCT agency_num, agency_name, minor_type
    FROM agency_info
    WHERE minor_type = 'MUNI'
    OR agency_num = '020060000'
    "
)

## PINS in UNINCORPORATED AREAS ARE NOT included in this data pull!!!

## Pulls ALL distinct PINs that existed between 2006 and 2022 in munis
## Syntax: "*" means "all the things" "pin" references the table w/in PTAXSIM DB
## 1,661,125 PINs when only including classes 400 to 899

tax_codes_muni <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
  SELECT DISTINCT year, agency_num, tax_code_num, tax_code_rate
  FROM tax_code
  WHERE agency_num IN ({muni_agency_names$agency_num*})
  ",
           .con = ptaxsim_db_conn
  ))


# 1,643,662 PINs in municipalities
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

options(scipen = 999)

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")  %>%
  select(agency_number, clean_name, Triad, Township) %>%
  mutate(agency_number = str_pad(agency_number, width = 9, side = "left", pad = "0")) %>%

  mutate(agency_number = as.character(agency_number))


tax_codes_muni <- tax_codes_muni %>%
  left_join(nicknames, by = c("agency_num" = "agency_number"))


# 119,030 distinct PINs in  Municipalities were comm or ind. property for at least 1 year
distinct_pins <- muni_pins |>
  select(pin) |>
  distinct(pin)

## Use unique PIN list to get all obs. for all years they existed.
## ~1.8mil PINs
## Note: Alea_cat is a variable created by Alea in the class_dict file that indicates land use type.
## Land use type is based off of the description provided for property classes.

## 1,818,155 PINs within municipalities. Excludes unincorporated PINs

## Excluding the Exemption variables except for abatements.
comm_ind_pins_ever <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql(
  "SELECT DISTINCT year, pin, class, tax_code_num, tax_bill_total, av_mailed, av_certified, av_board, av_clerk, exe_abate
   FROM pin
   WHERE pin IN ({distinct_pins$pin*})
  ",
  .con = ptaxsim_db_conn
  )) |>
  mutate_if(is.integer64, as.double ) %>%
  mutate(class = as.character(class)) |>
  left_join(cde, by = "class") |>
  left_join(ccao_loa, by = c("year", "class")) |>
  mutate(comparable_props = as.character(comparable_props),
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


## Add additional variables ---------------------------------------------

cross_county_lines <- c("030440000", "030585000", "030890000", "030320000", "031280000",
                        "030080000", "030560000", "031120000", "030280000", "030340000",
                        "030150000","030050000", "030180000","030500000", "031210000")


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


comm_ind_pins_ever <- comm_ind_pins_ever %>%
  rename(land_use = Alea_cat) |>
  arrange(pin) %>%
  left_join(tax_codes_muni, by = c("year", "tax_code_num")) %>%  # has muni clean_name  in it
  left_join(tif_distrib) %>%
  mutate(
    has_AB_exemp = as.character(ifelse(exe_abate > 0, 1, 0)),
    fmv = av_clerk / loa,
    fmv_NA_flag = ifelse(is.na(fmv),1,0),
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

#  Create 2006 to 2011 Timeseries ############################
timespan = 17

comm_ind_pins <- comm_ind_pins_ever  %>%
  group_by(pin) |>
  arrange(desc(year)) |>
  mutate(multi_muni = n_distinct(clean_name),
         multimuni_flag = ifelse(sum(multi_muni) > 1, 1, 0)) |>

  ## fill in muni names for the 700+ PINs that had multiple muni names or did not have a muni name for all years
  ## 471 were the Harvey/Markham Amazon PINs.
  mutate(clean_name = first(clean_name, na_rm = TRUE)) %>%

  mutate(
    tif_years = sum(in_tif==1),
    years_existed = n(),
    incentive_years = sum(incent_prop == "Incentive"),
    landuse_change =
      ifelse(
        sum(land_use == "Commercial") == timespan, "Always Commercial",
        ifelse(sum(land_use == "Industrial") == timespan, "Always Industrial",

               # some properties had an incentive class before 2011 and then were tax exempt. Dropped from panel.
               # created to remove PINs if they were tax exempt every year between 2011 and 2022.
               ifelse(sum(land_use == "Exempt") == timespan, "Drop Me",
                      ifelse(sum(land_use == "Exempt") > 0, "Exempt Sometime",
                             "Changes Land Use")))),
        incent_change = case_when(
          incentive_years == timespan ~ "Always Incentive",
          incentive_years == 0 ~ "Never Incentive",
          TRUE ~ "Changes Sometime"),
    tif_change = case_when(
      tif_years == timespan ~ "Always TIF",
      tif_years == 0 ~ "Never TIF",
      TRUE ~ "Changes")
  ) |>
  arrange(year) %>%
  mutate(incent = ifelse(class >= 600 & class <= 899, 1, 0)) %>%
  mutate(
    gain_incent = ifelse(incent == 1 & lag(incent) == 0, 1, 0),
    lose_incent = ifelse(incent == 0 & lag(incent) == 1, 1, 0),
    years_exempt = sum(ifelse(land_use == "Exempt", 1, 0))
  ) %>%
  mutate(incent_status =
           case_when(
             sum(gain_incent, na.rm=TRUE) > 0 & sum(lose_incent, na.rm=TRUE) > 0 ~ "Gains & Loses Incent",
             sum(gain_incent, na.rm=TRUE)  > 0 ~ "Gained Incentive",
             sum(lose_incent, na.rm=TRUE)  > 0 ~ "Had Incentive",
             sum(incent, na.rm=TRUE) == 0 ~ "Never had Incentive",
             sum(incent, na.rm=TRUE)== timespan  ~ "Always had Incentive"
           )) %>%
ungroup()

table(comm_ind_pins$years_exempt)
table(comm_ind_pins$landuse_change )

comm_ind_pins %>%
  filter(year == 2022) %>%
  reframe(n=n(), .by = landuse_change)


comm_ind_pins <- comm_ind_pins |>
  filter(
    !is.na(clean_name) &
      !agency_num %in% cross_county_lines &
      landuse_change != "Drop Me"
  )

comm_ind_pins <- comm_ind_pins |>
  mutate(exempt_flag = ifelse(land_use == "Exempt", 1, 0)) |>
  group_by(pin) |>
  mutate(
    base_year_fmv_2006 = ifelse( sum(exempt_flag) > 0, NA, fmv[year == 2006]),
    fmv_growth_2006 = (fmv/base_year_fmv_2006) - 1,
    base_year_fmv_2011 = ifelse( sum(exempt_flag) > 0, NA, fmv[year == 2011]),
    fmv_growth_2011 = (fmv/base_year_fmv_2011)-1) |>
  ungroup()



reassessment_years <- read_csv("./Necessary_Files/Triad_reassessment_years.csv")


reassessments_long <- reassessment_years %>%
  pivot_longer(cols = c(`2006`:`2022`), names_to = "year", values_to = "reassessed_year")




comm_ind_pins <- comm_ind_pins |>

  ## set variable types
  mutate(across(c(class, improvement_ind, has_AB_exemp, fmv_NA_flag, in_tif), as.character))

comm_ind_pins <- comm_ind_pins |>
  mutate(year = as.character(year)) |>
  left_join(reassessments_long, by = c("year", "Triad")) |>

  # Change to factors; set reference levels in next steps
  mutate(land_use = ifelse(!land_use %in% c("Commercial", "Industrial", "Land", "Exempt"), "Other Land Use", land_use),
         fmv_growth_2006 = round(fmv_growth_2006, digits = 4),
         fmv_growth_2011 = round(fmv_growth_2011, digits = 4) ) |>
  # percent change from previous years
  group_by(pin) |>
  arrange(year) |>
  mutate(fmv_pct_change = (fmv - lag(fmv))/ lag(fmv),
         av_clerk_pct_change = (av_clerk - lag(av_clerk)) / lag(av_clerk),
         av_mailed_pct_change = (av_mailed - lag(av_mailed)) / lag(av_mailed),

        ## lagged categorical variables #
         class_lag = lag(class),
         improvement_lag = lag(improvement_ind),
         reassess_lag = lag(reassessed_year),
         reassessed_taxyear = lead(reassessed_year),
         incent_change_year = ifelse(incent_prop != lag(incent_prop), year, NA),
         next_reassessment = ifelse(!is.na(incent_change_year) & reassessed_year == 1, "Same Year",
                                    ifelse(!is.na(incent_change_year) & reassessed_year == 0, "Next Assessment", NA))
  ) |>
  ungroup() |>
  group_by(year) |>

  # Winsorize FMV values observed. Remove extreme values from each year of observations
  mutate(
    base_year_fmv_2006_w =
      DescTools::Winsorize(base_year_fmv_2006,
                           quantile(base_year_fmv_2006,
                                    probs = c(0.01,0.99), na.rm=TRUE)),
    fmv_growth_2006_w =
      DescTools::Winsorize(fmv_growth_2006,
                           quantile(fmv_growth_2006, probs = c(0.01,0.99), na.rm=TRUE)),
    base_year_fmv_2011_w =
      DescTools::Winsorize(base_year_fmv_2011,
                           quantile(base_year_fmv_2011, probs = c(0.01,0.99), na.rm=TRUE)),
    fmv_growth_2011_w =
      DescTools::Winsorize(fmv_growth_2011,
                           quantile(fmv_growth_2011, probs = c(0.01,0.99), na.rm=TRUE))) |>
  ungroup() |>
  arrange(pin, year)

### How many properties LOSE their incentives? ####

# unique PINs that no longer had an incentive class within the data time frame
lose_incent_pins <- comm_ind_pins |>
  group_by(pin) |>
  filter(incent_prop == "Incentive" & lead(incent_prop) == "Non-Incentive") %>%
  select(pin, year) %>%
  mutate(type = "Loses Incentive")

# unique PINs that received an incentive class within the data time frame
gains_incent_pins <- comm_ind_pins |>
  group_by(pin) |>
  filter(incent_prop == "Incentive" & lag(incent_prop) == "Non-Incentive") %>%
  select(pin, year) %>%
  mutate(type = "Gains Incentive")

gain_lose_pins <- rbind(gains_incent_pins, lose_incent_pins) |>
  mutate(treatment_year = year) |>
  group_by(pin) |>
  summarize(treatment_year = first(treatment_year),
            type = first(type) )



comm_ind_pins <- comm_ind_pins %>%
  left_join(gain_lose_pins, by = "pin")

comm_ind_pins <- comm_ind_pins |>
  mutate(status = ifelse(treatment_year < year & incent_change == "Changes Sometime",
                         "Pre Treatment",
                         ifelse(treatment_year >= year & incent_change == "Changes Sometime",
                                "Post Treatment",
                                       "Control")))


write_csv(comm_ind_pins_ever, "./Output/comm_ind_PINs_2006to2022_timeseries.csv")


# write_csv(comm_ind_pins_ever, "./Output/comm_ind_inmunis_timeseries_2006to2022.csv")


# Create 2011-2022 timeseries --------------------------------------------

timespan = 12

comm_ind_pins <- comm_ind_pins_ever  %>%
  filter(year>=2011) |>
  group_by(pin) |>
  arrange(desc(year)) |>
  mutate(multi_muni = n_distinct(clean_name),
         multimuni_flag = ifelse(sum(multi_muni) > 1, 1, 0)) |>

  ## fill in muni names for the 700+ PINs that had multiple muni names or did not have a muni name for all years
  ## 471 were the Harvey/Markham Amazon PINs.
  mutate(clean_name = first(clean_name, na_rm = TRUE)) %>%

  mutate(
    tif_years = sum(in_tif==1),
    years_existed = n(),
    incentive_years = sum(incent_prop == "Incentive"),
    landuse_change =
      ifelse(
        sum(land_use == "Commercial") == timespan, "Always Commercial",
        ifelse(sum(land_use == "Industrial") == timespan, "Always Industrial",

               # some properties had an incentive class before 2011 and then were tax exempt. Dropped from panel.
               # created to remove PINs if they were tax exempt every year between 2011 and 2022.
               ifelse(sum(land_use == "Exempt") == timespan, "Drop Me",
                      ifelse(sum(land_use == "Exempt") > 0, "Exempt Sometime",
                             "Changes Land Use")))),

    # incent_change = original variable used before incent_status!
     incent_change = case_when(
      incentive_years == timespan ~ "Always Incentive",
      incentive_years == 0 ~ "Never Incentive",
      TRUE ~ "Changes Sometime"),
    tif_change = case_when(
      tif_years == timespan ~ "Always TIF",
      tif_years == 0 ~ "Never TIF",
      TRUE ~ "Changes")
  ) |>
  arrange(year) %>%
  mutate(incent = ifelse(class >= 600 & class <= 899, 1, 0),
         exempt = ifelse(class == 0, 1, 0)) %>%
  mutate(
    gain_incent = ifelse(incent == 1 & lag(incent) == 0, 1, 0),
    lose_incent = ifelse(incent == 0 & lag(incent) == 1, 1, 0),
    became_exempt = ifelse(exempt == 1 & lag(incent) == 1, 1, 0),
    became_taxed = ifelse(exempt == 0 & lag(incent) == 1, 1, 0),
    leave_tif = ifelse(in_tif == 0 & lag(in_tif) == 1, 1, 0),
    enter_tif = ifelse(in_tif == 1 & lag(in_tif) == 0, 1, 0),

    years_exempt = sum(ifelse(land_use == "Exempt", 1, 0))
  ) %>%
  mutate(incent_status =
           case_when(
             sum(gain_incent, na.rm=TRUE) > 0 & sum(lose_incent, na.rm=TRUE) > 0 ~ "Gains & Loses Incent",
             sum(gain_incent, na.rm=TRUE)  > 0 ~ "Gained Incentive",
             sum(lose_incent, na.rm=TRUE)  > 0 ~ "Lost Incentive",
             sum(incent, na.rm=TRUE) == 0 ~ "Never had Incentive",
             sum(incent, na.rm=TRUE)== timespan  ~ "Always had Incentive"
           )) %>%
  ungroup()

table(comm_ind_pins$years_exempt)
table(comm_ind_pins$landuse_change )

comm_ind_pins %>%
  filter(year == 2022) %>%
  reframe(n=n(), .by = landuse_change)


comm_ind_pins <- comm_ind_pins |>
  filter(
    !is.na(clean_name) &
      !agency_num %in% cross_county_lines &
      landuse_change != "Drop Me"
  )

comm_ind_pins <- comm_ind_pins |>
  mutate(exempt_flag = ifelse(land_use == "Exempt", 1, 0)) |>
  group_by(pin) |>
  mutate(
    base_year_fmv_2011 = ifelse( sum(exempt_flag) > 0, NA, fmv[year == 2011]),
    fmv_growth_2011 = (fmv/base_year_fmv_2011)-1) |>
  ungroup()



reassessment_years <- read_csv("./Necessary_Files/Triad_reassessment_years.csv") %>%
  select(-c(`2006`:`2010`))


reassessments_long <- reassessment_years %>%
  pivot_longer(cols = c(`2011`:`2022`), names_to = "year", values_to = "reassessed_year")




comm_ind_pins <- comm_ind_pins |>

  ## set variable types
  mutate(across(c(class, improvement_ind, has_AB_exemp, fmv_NA_flag, in_tif), as.character))

comm_ind_pins <- comm_ind_pins |>
  mutate(year = as.character(year)) |>
  left_join(reassessments_long, by = c("year", "Triad")) |>

  # Change to factors; set reference levels in next steps
  mutate(land_use = ifelse(!land_use %in% c("Commercial", "Industrial", "Land", "Exempt"), "Other Land Use", land_use),
         fmv_growth_2011 = round(fmv_growth_2011, digits = 4) ) |>
  # percent change from previous years
  group_by(pin) |>
  arrange(year) |>
  mutate(fmv_pct_change = (fmv - lag(fmv))/ lag(fmv),
         av_clerk_pct_change = (av_clerk - lag(av_clerk)) / lag(av_clerk),
         av_mailed_pct_change = (av_mailed - lag(av_mailed)) / lag(av_mailed),

         # lagged categorical variables
         class_lag = lag(class),
         improvement_lag = lag(improvement_ind),
         reassess_lag = lag(reassessed_year),
         reassessed_taxyear = lead(reassessed_year),

         # can be any incentive change, gain or lost, based on current coding
         incent_change_year = ifelse(incent_prop != lag(incent_prop), year, NA),
         next_reassessment = ifelse(!is.na(incent_change_year) & reassessed_year == 1, "Same Year",
                                    ifelse(!is.na(incent_change_year) & reassessed_year == 0, "Next Assessment", NA))
  ) |>
  ungroup() |>
  group_by(year) |>

  # Winsorize FMV values observed. Remove extreme values from each year of observations
  mutate(
    base_year_fmv_2011_w =
      DescTools::Winsorize(base_year_fmv_2011,
                           quantile(base_year_fmv_2011, probs = c(0.01,0.99), na.rm=TRUE)),
    fmv_growth_2011_w =
      DescTools::Winsorize(fmv_growth_2011,
                           quantile(fmv_growth_2011, probs = c(0.01,0.99), na.rm=TRUE))) |>
  ungroup() |>
  arrange(pin, year)

### How many properties LOSE their incentives? ####

# unique PINs that no longer had an incentive class within the data time frame
# 1024 lose based on 2022 data
lose_incent_pins <- comm_ind_pins |>
  group_by(pin) |>
  filter(incent_prop == "Incentive" & lead(incent_prop) == "Non-Incentive") %>%
  select(pin, year) %>%
  mutate(type = "Loses Incentive") |>
  mutate(treatment_year = year)


# unique PINs that received an incentive class within the data time frame
# 3138 gain based on 2022 data
gains_incent_pins <- comm_ind_pins |>
  group_by(pin) |>
  filter(incent_prop == "Incentive" & lag(incent_prop) == "Non-Incentive") %>%
  select(pin, year) %>%
  mutate(type = "Gains Incentive") |>
  mutate(treatment_year = year)

gain_lose_pins <- rbind(gains_incent_pins, lose_incent_pins) |>
  mutate(treatment_year = year) |>
  select(-year) |>
  group_by(pin) |>
  summarize(treatment_year = first(treatment_year),
            times_changed = n(),
            type = first(type) )


comm_ind_pins <- comm_ind_pins %>%
  left_join(gain_lose_pins, by = "pin")

comm_ind_pins <- comm_ind_pins |>
  mutate(status = ifelse(treatment_year < year & incent_status == "Gains Incent",
                         "Pre Treatment",
                         ifelse(treatment_year >= year & incent_change == "Gains Incent",
                                "Post Treatment",
                                       "Control"))         )


write_csv(comm_ind_pins, "./Output/comm_ind_PINs_2011to2022_timeseries.csv")


# Create 2006 PIN level Panel Data ----------------------------------------
#
# comm_ind_pins_2006 <- comm_ind_pins_ever |>
#   group_by(pin) |>
#   mutate(
#     tif_years = sum(in_tif==1),
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
## Examine dropped PINs from 2006 to 2022 panel data -----------------------
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
#### Drop PINs and export File -----------------------------------------------
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


## Unique PINs and their property classes over time ------------------------------------

pin_classes <- comm_ind_pins_ever %>%
  group_by(clean_name, pin, class) %>%
  summarize(count = n(),
            first_year = first(year),
            last_year = last(year)) %>%
  ungroup() %>%
  arrange(pin, first_year)


unique_ptax_w_class <- pin_classes %>% group_by(pin) %>%
  mutate(var2 = cumsum(row_number() == 1 | (class != dplyr::lag(class))))


unique_ptax_wide <- unique_ptax_w_class %>%
  pivot_wider(id_cols = c("pin", "clean_name"),
              names_from = var2,
              values_from = c(class, count, first_year, last_year))

## 119,993 Unique PINs and any class change they experienced:
write_csv(unique_ptax_wide, "./Output/pin_class_changes.csv")


# Create 2011-2022 PIN level Panel Data -----------------------------------

timespan = 12


comm_ind_2011to2022 <- comm_ind_pins_ever  %>%
  filter(year >= 2011 ) %>%
  group_by(pin) |>
  arrange(desc(year)) |>
  mutate(multi_muni = n_distinct(clean_name),
         multimuni_flag = ifelse(sum(multi_muni) > 1, 1, 0)) |>

  ## fill in muni names for the 700+ PINs that had multiple muni names or did not have a muni name for all years
  ## 471 were the Harvey/Markham Amazon PINs.
  mutate(clean_name = first(clean_name, na_rm = TRUE)) %>%

  mutate(
    tif_years = sum(in_tif==1),
    years_existed = n(),
    incentive_years = sum(incent_prop == "Incentive"),
    landuse_change =
      ifelse(
        sum(land_use == "Commercial") == timespan, "Always Commercial",
        ifelse(sum(land_use == "Industrial") == timespan, "Always Industrial",
               # some properties had an incentive class before 2011 and then were tax exempt. Dropped from panel.
               ifelse(sum(land_use == "Exempt") == timespan, "Drop Me",   # created to remove PINs if they were tax exempt every year between 2011 and 2022.
                      "Changes Land Use" ))),
    incent_change = case_when(
      incentive_years == timespan ~ "Always Incentive",
      incentive_years == 0 ~ "Never Incentive",
      TRUE ~ "Changes Sometime"),
    tif_change = case_when(
      tif_years == timespan ~ "Always TIF",
      tif_years == 0 ~ "Never TIF",
      TRUE ~ "Changes")
  ) |>
  ungroup()
# 1,284,971 obs before dropping PINs

## Examine PINs dropped from 2011-2022 panel data --------------------------


# only keep PINs that existed all years and if they are from a municipality
# assumes that clean_name and agency_number correctly merged to tax codes from municipalities and their agency number
# 97,521 PINs will be dropped from the 2011-2022 panel data if including years_existed in filter.

# drops 27,644 PINs when using clean_name, cross_county_lines, and landuse_change
dropped_pins2 <-  comm_ind_2011to2022 %>%
  filter(
      is.na(clean_name)  |       ## 4,370 PINs do not have municipality names
      agency_num %in% cross_county_lines |  ## 13,915 PINs located in Municipalities that have a majority of their EAV in other counties
      landuse_change == "Drop Me"     ## 12,048 PINs were tax exempt for all 12 years
  )
#  years_existed < 12  |      ##  69,491 PINs do not exist all 12 years


### Drop PINs and Export File -----------------------------------------------




weirdpins <- comm_ind_2011to2022 |>
  filter(
    years_existed == 12 &
      !is.na(clean_name) &
      !agency_num %in% cross_county_lines &
      landuse_change != "Drop Me"
  )  %>%
  group_by(pin) |>
  mutate(years_existed2 = n()) |>
  ungroup() |>
  filter(years_existed2 !=12)

# drop the 27,644 PINs from not having a muni name, being 50% in cook, and being taxable
comm_ind_2011to2022 <- comm_ind_2022to2022 |>
  filter(
 #   years_existed == 12 &
      !is.na(clean_name) &
      !agency_num %in% cross_county_lines &
      landuse_change != "Drop Me"
    )
## 1,257,327 remain - July 16 AWM
## 1,187,450 obs remain


# another 70,203 observations do not exist every year.
dropped_pins3 <- comm_ind_2011to2022 %>%
  group_by(pin) %>%
  mutate(years_existed2 = n()) %>%
  filter(years_existed2 < 12)


# Drop those PINs that didn't exist every year
comm_ind_2011to2022 <- comm_ind_2011to2022 %>%
  group_by(pin) %>%
  mutate(years_existed2 = n()) %>%
  filter(years_existed2 == 12)
# 1,187,124 obs remain

comm_ind_2011to2022 <- comm_ind_2011to2022 |>
  mutate(exempt_flag = ifelse(land_use == "Exempt", 1, 0)) |>
 group_by(pin) |>
  mutate(
    base_year_fmv_2011 = ifelse( sum(exempt_flag) > 0, NA, fmv[year == 2011]),
    fmv_growth_2011 = fmv/base_year_fmv_2011) |>
  ungroup()

## 100,900 PINs existed since 2011 (and did not become tax exempt)
## 102,717 PINs if not filtering for NA fmv growth
## 99,088 PINs as of July 11 - AWM (1.12 obs. - MVH)
## BUT some are exempt and will have 0 or errors for the fmv growth!!



comm_ind_2011to2022 %>%
 reframe(pincount = n(), .by = year)

comm_ind_2011to2022 %>%
  #select(year, land_use, incent_prop, fmv_growth_2011) %>%
  filter(year == 2022) %>% reframe(pincount = n(), .by = clean_name) %>% arrange()


## Write CSV to Output Folder
write_csv(comm_ind_2011to2022, "./Output/comm_ind_PINs_2011-2022_balanced.csv")


