###MVH 7-15-23 Lagged Variables Learning###

options(scipen = 999)

##libraries

library(tidyverse)
library(data.table)
library(ggspatial)
library(gstat)
library(here)
library(sf)
library(stars)
library(glue)
library(lmtest)
library(huxtable)
library(jtools)
library(plm)
library(modelsummary)

##ptaxsim stuff

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db")

#base_url <- "https://datacatalog.cookcountyil.gov/resource/tx2p-k2g9.json"

##read in schools db

schools <- read.csv("schools_7-13.csv")

##Create lagged variables

schools %>%
  arrange(year) %>%
  group_by(agency_num) %>%
  mutate(eav_lag1 = lag(log_eav, n = 1, order_by = year, default = NA)) %>%
  ungroup()

schools <- schools %>%
  arrange(year) %>%
  group_by(agency_num) %>%
  mutate(eav_lag3 = lag(log_eav, n = 3, order_by = year, default = NA)) %>%
  ungroup()

##Check for missing values

schools %>%
  filter(year != 2015 | 2016) %>%
  summarize(n())

##Create total change in EAV

school_2006 <- schools %>%
  filter(year == 2006) %>%
  select(agency_num, eav2006 = log_eav) %>%
  arrange(agency_num)

school_2008 <- schools %>%
  filter(year == 2008) %>%
  select(agency_num, eav2008 = log_eav) %>%
  arrange(agency_num)

school_2021 <- schools %>%
  filter(year == 2021) %>%
  select(agency_num, eav2021 = log_eav) %>%
  arrange(agency_num)

diff_calc <- full_join(school_2006, school_2008, by = "agency_num")

diff_calc <- full_join(diff_calc, school_2021, by = "agency_num")

##Create variables by subtracting stuff.

diff_calc <- diff_calc %>%
  mutate(diff_2006 = eav2021 - eav2006) %>%
  mutate(diff_2008 = eav2021 - eav2008)


##Merge back into DF

schools <- left_join(schools, diff_calc, by = "agency_num")

schools <- schools %>%
  select(!c(eav2021, eav2008, eav2006))

schools %>%
  ggplot(aes()) +
  geom_density(aes(diff_2006), color = "green", bin = 2) +
  geom_density(aes(diff_2008), color = "purple", bin = 2)

##We should save this as a csv

write.csv(schools, "schools_7-15.csv")

schools <- read.csv("schools_7-15.csv")

schools <- schools %>%
  select(!c(diff_2006, diff_2008))

schools <- schools %>%
  select(!c(X.1, X, eav_lag3))

write.csv(schools, "schools_7-16.csv")

schools <- read.csv("schools_7-16.csv")

###MODEL TIME!!!###

##FEs, 2 year lag

school_all_2yr <- plm(log_levy ~ log_eav + eav_lag1 + eav_lag2, index = c("agency_name", "year"),
                        model = "within",  effect= "twoways", data = schools)

summary(school_all)

##FEs, 1 year lag

school_all_1yr <- plm(log_levy ~ log_eav + eav_lag1, index = c("agency_name", "year"),
                      model = "within",  effect= "twoways", data = schools)

summary(school_all_1yr)

##FEs, change against 2006

school_all_2008 <- plm(log_levy ~ log_eav + diff_2008, index = c("agency_name", "year"),
                      model = "within",  effect= "twoways", data = schools)

summary(school_all_2008)

cor.test(schools$log_eav, schools$diff_2008)

###JUST ELEMENTARY SCHOOLS###

# schools_ele <- schools %>%
#   filter(minor_type == )




##Figure out lagged variable.
##This dplyr command SHOULD work

schools <- schools %>%
  arrange(year) %>%
  group_by(agency_num) %>%
  mutate(eav_lag1 = lag(log_eav, n = 1, default = NA))

#problem: the "year" variable is not a date variable.

#as.Date(year) causes complete CHAOS

#Apparently we convert it to a date, then get rid of the extra info we have
#beyond year.

schools <- schools %>%
  mutate(year = as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"))

#Let's try making that lag variable again.

schools <- schools %>%
  arrange(year) %>%
  group_by(agency_num) %>%
  mutate(eav_lag1 = lag(log_eav, n = 1, default = NA))

#Still no dice.

schools <- read.csv("schools_7-13.csv")

schools <- schools %>%
  group_by(agency_num) %>%
  mutate(eav_lag1 = lag(log_eav, n = 1, default = NA)) %>%
  ungroup()

#giving chatgpt some of the data.

sample_data <- schools %>%
  select(year, agency_num, log_eav) %>%
  sample_n(50)

print(sample_data, n = 50)

schoolz <- schools %>%
  arrange(year) %>%
  group_by(agency_num) %>%
  mutate(eav_lag1 = lag(log_eav, n = 1, default = NA)) %>%
  ungroup()

#OMFG

schoolz <- schools %>%
  arrange(year) %>%
  group_by(agency_num) %>%
  mutate(eav_lag1 = lag(log_eav, n = 1, default = NA)) %>%
  ungroup() %>%
  slice(-1)

#NOOOOOOOO

schoolz <- schools %>%
  arrange(year) %>%
  group_by(agency_num) %>%
  mutate(eav_lag1 = lag(log_eav, n = 1),
         eav_lag1 = ifelse(row_number() == 1, NA, eav_lag1)) %>%
  ungroup()

schoolz %>%
  filter(agency_num == "40230000")

#I HATE THIS SO MUCH.

schoolz <- schools %>%
  arrange(year) %>%
  group_by(agency_num) %>%
  mutate(eav_lag1 = lag(log_eav, n = 1, order_by = year, default = NA)) %>%
  ungroup()

#TRY AGAIN

schoolz <- schools %>%
  arrange(year) %>%
  group_by(agency_num) %>%
  mutate(eav_lag1 = lag(log_eav, n = 1, default = NA)) %>%
  ungroup()
