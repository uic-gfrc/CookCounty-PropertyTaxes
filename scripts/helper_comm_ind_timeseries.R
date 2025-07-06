
options(scipen = 999) # scientific notation sucks

library(tidyverse)
library(glue)

## Import supporting files --------------------------------------

# Class dictionary

cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  mutate(class = as.character(class_code)) |>  # rename to match other data frames
  select(-c(loa_2022, Option2, class_desc, land, vacant_ind, last2dig,
            Res_nonRes, assessment_level, used_in2021, class_code)) |>
  mutate_at(.vars = c("improvement_ind", "incent_prop", "class_1dig", "major_class_code"), .funs = as.character
  )

# Levels of assessment by year (they change over time)

ccao_loa <- read_csv("./inputs/ccao_loa.csv") |>
  mutate(class = as.character(class_code)) |>
  filter(year > 2005) |>
  select(-class_code) |>
  mutate(loa = as.numeric(loa)) |>
  mutate(loa = ifelse(loa == 0, NA, loa) # avoid dividing by zero errors
  )

# "Clean" muni names

nicknames <- readxl::read_excel("./Necessary_Files/muni_shortnames.xlsx")  |>
  select(agency_number, clean_name, Triad, Township) |>
  mutate(agency_number = str_pad(agency_number, width = 9, side = "left", pad = "0")) |>
  mutate(agency_number = as.character(agency_number))

## Create class and border crosser variables. ---------------------

# The following munis cross county lines and are consistently dropped across the PTAX project

# "Frankfort", "Homer Glen",  "Oak Brook", "East Dundee", "University Park",
# "Bensenville", "Hinsdale", "Roselle", "Deer Park", "Deerfield"

cross_county_lines <- c("030440000", "030585000", "030890000", "030320000", "031280000",
                        "030080000", "030560000", "031120000", "030280000", "030340000",
                        "030150000","030050000", "030180000","030500000", "031210000")


# These had to be manually broken up because major classes 4, 5, and 8 include
# commercial and industrial properties

# Note: major class 400 is for nonprofits

commercial_classes <- c(401:435, 490, 491, 492, 496:499,
                        500:535, 590, 591, 592, 597:599,
                        700:799,
                        800:835, 891, 892, 897, 899
)

industrial_classes <- c(480:489, 493,
                        550:589, 593,
                        600:699,
                        850:890, 893
)

comm_ind_pins_ever <- read_csv("./Output/comm_ind_PINs_ever_2006to2023.csv")

#  Create 2006 to 2023 Timeseries ############################
timespan = 18

comm_ind_pins <- comm_ind_pins_ever  %>%
  # filter(year != 2023) |>
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
  mutate(incent = ifelse(class >= 600 & class <= 899, 1, 0)) %>%  # numeric version of incent_prop
  mutate(
    gain_incent = ifelse(incent == 1 & lag(incent) == 0, 1, 0),
    lose_incent = ifelse(incent == 0 & lag(incent) == 1, 1, 0),
    years_exempt = sum(land_use == "Exempt", na.rm=TRUE)
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
  filter(year == 2023) %>%
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
  pivot_longer(cols = c(`2006`:`2023`), names_to = "year", values_to = "reassessed_year")




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

# comm_ind_pins <- comm_ind_pins |>
#   mutate(status = ifelse(treatment_year < year & incent_change == "Changes Sometime",
#                          "Pre Treatment",
#                          ifelse(treatment_year >= year & incent_change == "Changes Sometime",
#                                 "Post Treatment",
#                                        "Control")))

comm_ind_pins <- comm_ind_pins %>%
  mutate(incent_change = ifelse(years_existed < timespan, "Excluded", incent_change),
         incent_status = ifelse(years_existed < timespan, "Excluded", incent_status),
         incent_status = ifelse(incent_status == "Gains & Loses Incent", "Excluded", incent_status),
         landuse_change = ifelse(years_existed < timespan, "Excluded", landuse_change),
         landuse_change = ifelse(landuse_change == "Exempt Sometime", "Excluded", landuse_change))

write_csv(comm_ind_pins, "./Output/comm_ind_PINs_2006to2023_timeseries.csv")




# Create 2011-2023 timeseries --------------------------------------------

timespan = 13

comm_ind_pins <- comm_ind_pins_ever  %>%
  filter(between(year, 2011, 2023)) |>
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
  filter(year == 2023) %>%
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
  pivot_longer(cols = c(`2011`:`2023`), names_to = "year", values_to = "reassessed_year")




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

# comm_ind_pins <- comm_ind_pins |>
#   mutate(status = ifelse(treatment_year < year & incent_status == "Gains Incent",
#                          "Pre Treatment",
#                          ifelse(treatment_year >= year & incent_change == "Gains Incent",
#                                 "Post Treatment",
#                                        "Control"))         )
comm_ind_pins <- comm_ind_pins %>%
  mutate(incent_change = ifelse(years_existed < timespan, "Excluded", incent_change),
         incent_status = ifelse(years_existed < timespan, "Excluded", incent_status),
         incent_status = ifelse(incent_status == "Gains & Loses Incent", "Excluded", incent_status),
         landuse_change = ifelse(years_existed < timespan, "Excluded", landuse_change),
         landuse_change = ifelse(landuse_change == "Exempt Sometime", "Excluded", landuse_change))


write_csv(comm_ind_pins, "./Output/comm_ind_PINs_2011to2023_timeseries.csv")







################################################################################
# Ended up not using this because it dropped observations that we cared about.

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
#
#
# # Create 2011-2022 PIN level Panel Data -----------------------------------
#
# timespan = 12
#
#
# comm_ind_2011to2022 <- comm_ind_pins_ever  %>%
#   filter(year >= 2011 ) %>%
#   group_by(pin) |>
#   arrange(desc(year)) |>
#   mutate(multi_muni = n_distinct(clean_name),
#          multimuni_flag = ifelse(sum(multi_muni) > 1, 1, 0)) |>
#
#   ## fill in muni names for the 700+ PINs that had multiple muni names or did not have a muni name for all years
#   ## 471 were the Harvey/Markham Amazon PINs.
#   mutate(clean_name = first(clean_name, na_rm = TRUE)) %>%
#
#   mutate(
#     tif_years = sum(in_tif==1),
#     years_existed = n(),
#     incentive_years = sum(incent_prop == "Incentive"),
#     landuse_change =
#       ifelse(
#         sum(land_use == "Commercial") == timespan, "Always Commercial",
#         ifelse(sum(land_use == "Industrial") == timespan, "Always Industrial",
#                # some properties had an incentive class before 2011 and then were tax exempt. Dropped from panel.
#                ifelse(sum(land_use == "Exempt") == timespan, "Drop Me",   # created to remove PINs if they were tax exempt every year between 2011 and 2022.
#                       "Changes Land Use" ))),
#     incent_change = case_when(
#       incentive_years == timespan ~ "Always Incentive",
#       incentive_years == 0 ~ "Never Incentive",
#       TRUE ~ "Changes Sometime"),
#     tif_change = case_when(
#       tif_years == timespan ~ "Always TIF",
#       tif_years == 0 ~ "Never TIF",
#       TRUE ~ "Changes")
#   ) |>
#   ungroup()
# # 1,284,971 obs before dropping PINs
#
# ## Examine PINs dropped from 2011-2022 panel data --------------------------
#
#
# # only keep PINs that existed all years and if they are from a municipality
# # assumes that clean_name and agency_number correctly merged to tax codes from municipalities and their agency number
# # 97,521 PINs will be dropped from the 2011-2022 panel data if including years_existed in filter.
#
# # drops 27,644 PINs when using clean_name, cross_county_lines, and landuse_change
# dropped_pins2 <-  comm_ind_2011to2022 %>%
#   filter(
#       is.na(clean_name)  |       ## 4,370 PINs do not have municipality names
#       agency_num %in% cross_county_lines |  ## 13,915 PINs located in Municipalities that have a majority of their EAV in other counties
#       landuse_change == "Drop Me"     ## 12,048 PINs were tax exempt for all 12 years
#   )
# #  years_existed < 12  |      ##  69,491 PINs do not exist all 12 years
#
#
# ### Drop PINs and Export File -----------------------------------------------
#
#
#
#
# weirdpins <- comm_ind_2011to2022 |>
#   filter(
#     years_existed == 12 &
#       !is.na(clean_name) &
#       !agency_num %in% cross_county_lines &
#       landuse_change != "Drop Me"
#   )  %>%
#   group_by(pin) |>
#   mutate(years_existed2 = n()) |>
#   ungroup() |>
#   filter(years_existed2 !=12)
#
# # drop the 27,644 PINs from not having a muni name, being 50% in cook, and being taxable
# comm_ind_2011to2022 <- comm_ind_2022to2022 |>
#   filter(
#  #   years_existed == 12 &
#       !is.na(clean_name) &
#       !agency_num %in% cross_county_lines &
#       landuse_change != "Drop Me"
#     )
# ## 1,257,327 remain - July 16 AWM
# ## 1,187,450 obs remain
#
#
# # another 70,203 observations do not exist every year.
# dropped_pins3 <- comm_ind_2011to2022 %>%
#   group_by(pin) %>%
#   mutate(years_existed2 = n()) %>%
#   filter(years_existed2 < 12)
#
#
# # Drop those PINs that didn't exist every year
# comm_ind_2011to2022 <- comm_ind_2011to2022 %>%
#   group_by(pin) %>%
#   mutate(years_existed2 = n()) %>%
#   filter(years_existed2 == 12)
# # 1,187,124 obs remain
#
# comm_ind_2011to2022 <- comm_ind_2011to2022 |>
#   mutate(exempt_flag = ifelse(land_use == "Exempt", 1, 0)) |>
#  group_by(pin) |>
#   mutate(
#     base_year_fmv_2011 = ifelse( sum(exempt_flag) > 0, NA, fmv[year == 2011]),
#     fmv_growth_2011 = fmv/base_year_fmv_2011) |>
#   ungroup()
#
# ## 100,900 PINs existed since 2011 (and did not become tax exempt)
# ## 102,717 PINs if not filtering for NA fmv growth
# ## 99,088 PINs as of July 11 - AWM (1.12 obs. - MVH)
# ## BUT some are exempt and will have 0 or errors for the fmv growth!!
#
#
#
# comm_ind_2011to2022 %>%
#  reframe(pincount = n(), .by = year)
#
# comm_ind_2011to2022 %>%
#   #select(year, land_use, incent_prop, fmv_growth_2011) %>%
#   filter(year == 2022) %>% reframe(pincount = n(), .by = clean_name) %>% arrange()
#
#
# ## Write CSV to Output Folder
#write_csv(comm_ind_2011to2022, "./Output/comm_ind_PINs_2011-2022_balanced.csv")
#