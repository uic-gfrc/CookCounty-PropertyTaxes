### Zero Bill Analysis ###
## MVH 11/24 ##

library(DBI)
library(RSQLite)
library(tidyverse)
library(ptaxsim)

options(scipen = 999)


## Things MVH wants for memo:
# Total zero bills & distribution of TEAV
# top middle bottom 5 munis by #
# top middle bottom 5 munis by EAV
# back on rolls for > $150 EAV by < $150  EAV
# back on rolls for > $150
# for when EAV <= TEAV where is that  change EAV come from (e.g. disabled vets?)



# Load random files

eq_factor <- read_csv("./Necessary_Files/eq_factor.csv") |>
  select(year, eq_factor_final)

cde <- read_csv("./Necessary_Files/class_dict_expanded.csv") |>
  mutate(class = as.character(class_code)) |>  # rename to match other data frames
  select(-c(loa_2022, Option2, class_desc, land, vacant_ind, last2dig,
            Res_nonRes, assessment_level, used_in2021, class_code)) %>%
  mutate_at(.vars = c("improvement_ind", "incent_prop", "class_1dig", "major_class_code"), .funs = as.character
  )

# Connect to the database
con <- dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")
ptaxsim_db_conn <- dbConnect(RSQLite::SQLite(), "./ptaxsim.db/ptaxsim-2023.0.0.db")


# Pull residential PINs between 2010 and 2023 with bills of 0 or eq_av less than 150
# For eq factor, used the upper bound of 3.3 to capture all the low-eav bills.
pin_data <- dbGetQuery(con, "
SELECT
  *
FROM pin
WHERE
  year BETWEEN 2010 AND 2023
  AND substr(CAST(class as TEXT), 1, 1) = '2'
  AND (
    (av_clerk * 3.3) < 150
    OR tax_bill_total = 0
  )")


pin_data2 <- dbGetQuery(con, "
SELECT
  *
FROM pin
WHERE
  year BETWEEN 2010 AND 2023
  AND substr(CAST(class as TEXT), 1, 1) = '2'
  AND (
    (av_clerk * 3.3) < 150
    OR tax_bill_total = 0
  )")

joined_pins <- read_csv("./output/Dont_Upload/0_joined_PIN_data_2023.csv") %>% mutate(tax_code_num = as.character(tax_code_num))

joined_pins <- joined_pins |> 
  filter(class > 199 & class < 300) |> 
  mutate(zero_bill = ifelse(pin %in% pin_data$pin[pin_data$year == 2023][pin_data$tax_bill_total == 0], 1, 0)) |>  # if pin is recorded as no bill in the pin table
  left_join(eq_factor, by = "year") |>
  mutate(eq_av = av*eq_factor_final) |>
  mutate(exe_total = rowSums(across(starts_with("exe_"))),
         exe_total = ifelse(exe_total > eq_av, eq_av, exe_total)) |>
  mutate(taxable_eav = eq_av - exe_total) |> # amount of eav that can be taxed after exemptions
  
  mutate(flag_missingdata = ifelse(zero_bill ==1 & taxable_eav > 1000, 1, 0)) |>
  mutate(exe_missing_disvet = ifelse(taxable_eav > 1000 & zero_bill ==1, taxable_eav, 0)) |>
  mutate(taxable_eav_adj = ifelse(zero_bill == 1 & flag_missingdata == 1, 0, taxable_eav)) |> # should be less than or equal to the taxable EAV calculated before
  mutate(total_billed_adj = taxable_eav_adj * tax_code_rate/100 ) |>
  
  mutate(proposal_eav = eq_av * 0.1) |> 
  mutate(proposal_taxbill = proposal_eav * tax_code_rate/100) |>
  mutate(exe_total_adj = rowSums(across(starts_with("exe_")))-exe_total)

joined_pins |> 
  summarize( n = n(), 
             taxbill = sum(total_billed, na.rm = T),
             taxbill_adj = sum( (eq_av - exe_total_adj)*tax_code_rate/100, na.rm=T),
             n_disvet = sum(exe_missing_disvet > 0),
             n_ghe = sum(exe_homeowner > 0),
             n_senior = sum(exe_senior > 0),
             n_freeze = sum(exe_freeze > 0),
             n_zerobill = sum(zero_bill == 1),
             
             exe_missing_disvet = sum(exe_missing_disvet),
             exe_vet_dis = sum(exe_vet_dis_lt50+exe_vet_dis_50_69+exe_vet_dis_ge70),
             exe_homeowner = sum(exe_homeowner),
             exe_senior = sum(exe_senior),
             exe_freeze = sum(exe_freeze),
             
             taxable_eav = sum(taxable_eav), 
             taxable_eav_adj = sum(taxable_eav_adj),
             
             exe_total = sum(exe_total),
             exe_total_adj = sum(exe_total_adj)
  )  |>
  View()

joined_pins |> 
  mutate(taxed_eav = eav-all_exemptions,
         bill_change = proposal_eav * tax_code_rate/100) |>
  filter(proposal_eav > taxed_eav | zero_bill == 1) |>
  summarize(n= n(),
    n_zerobills = sum(zero_bill==1),
    amount_paid = sum(final_tax_to_dist),
    shifted_rev = sum(0.1*eq_av * tax_code_rate/100),
    shifted_rev_fromdisvets = sum(.1*(exe_missing_disvet+exe_vet_dis_ge70)*tax_code_rate/100),
    avg_billchange = mean(bill_change),
    median_billchange = median(bill_change)
  ) 

# 37,801 PINs are paying less than 10% of their EAV (or less than ~1% of their market value for assessment purposes)
# 14.9 million in revenue as upper bound

## Municipalities with most zero dollar bills and the share of levy shifted to other tax payers
joined_pins |>  # already only class 2 pins 
  mutate(taxed_eav = eav-all_exemptions,
         bill_change = proposal_eav * tax_code_rate/100,
         final_tax_to_dist = ifelse(total_billed_adj == 0, 0, final_tax_to_dist)) |>
  group_by(clean_name) |>
  mutate(muni_levy = sum(final_tax_to_dist, na.rm=T)) |>
  
  filter(proposal_eav > taxed_eav | zero_bill == 1) |>

  summarize(n= n(),
            muni_levy = max(muni_levy),
            levy_paid_bygroup = sum(final_tax_to_dist, na.rm=T),
            n_zerobills = sum(zero_bill==1),
            n_disvet_zeros = sum(exe_missing_disvet + exe_vet_dis_lt50
                                 + exe_vet_dis_50_69 + exe_vet_dis_ge70 > 0),
            
            shifted_rev = sum(0.1*eq_av * tax_code_rate/100, na.rm=T),
            shifted_rev_fromdisvets = sum(.1*(exe_missing_disvet + exe_vet_dis_ge70)*tax_code_rate/100),
            exe_homeowner = sum(exe_homeowner),
            exe_senior = sum(exe_senior),
            exe_freeze = sum(exe_freeze),
            exe_vet_dis_total = sum(exe_missing_disvet+exe_vet_dis_ge70),
            exe_total_adj = sum(exe_total_adj),
            avg_billchange = mean(bill_change),
            median_billchange = median(bill_change),
            max_billchange = max(bill_change),
            min_billchange = min(bill_change)

            ) |>  # if all properties with $0 taxbills had 10% of their equalized AV taxed at current tax rate

  mutate(rev_share = shifted_rev/muni_levy,
         revenue_increase = shifted_rev-levy_paid_bygroup, .before = n) |>
  arrange(desc(rev_share)) |> View()


## Cook county sums
joined_pins |>  # already only class 2 pins 
  mutate(taxed_eav = eav-all_exemptions,
         bill_change = proposal_eav * tax_code_rate/100,
         
         final_tax_to_dist = ifelse(total_billed_adj == 0, 0, final_tax_to_dist)) |>
  mutate(muni_levy = sum(final_tax_to_dist, na.rm=T)) |>
  
  filter(proposal_eav > taxed_eav | zero_bill == 1) |>
  
  summarize(n= n(),
            muni_levy = max(muni_levy),
            levy_paid_bygroup = sum(final_tax_to_dist, na.rm=T),
            n_zerobills = sum(zero_bill==1),
            n_disvet_zeros = sum(exe_missing_disvet+exe_vet_dis_lt50+exe_vet_dis_50_69+exe_vet_dis_ge70>0),
            
            shifted_rev = sum(0.1*eq_av * tax_code_rate/100, na.rm=T),
            shifted_rev_fromdisvets = sum(.1*(exe_missing_disvet+exe_vet_dis_ge70)*tax_code_rate/100),
            exe_homeowner = sum(exe_homeowner),
            exe_senior = sum(exe_senior),
            exe_freeze = sum(exe_freeze),
            exe_vet_dis_total = sum(exe_missing_disvet+exe_vet_dis_ge70),
            exe_total_adj = sum(exe_total_adj),
            avg_billchange = mean(bill_change),
            median_billchange = median(bill_change),
            max_billchange = max(bill_change),
            min_billchange = min(bill_change)
            
            
  ) |>  # if all properties with $0 taxbills had 10% of their equalized AV taxed at current tax rate
  
  mutate(rev_share = shifted_rev/muni_levy,
  ) |>
  arrange(desc(rev_share)) |> View()

## Histogram of bill change -----------------
joined_pins |>  # already only class 2 pins 
  mutate(taxed_eav = eav-all_exemptions,
         bill_change = proposal_eav * tax_code_rate/100,
         final_tax_to_dist = ifelse(total_billed_adj == 0, 0, final_tax_to_dist)) |>
  mutate(muni_levy = sum(final_tax_to_dist, na.rm=T)) |>
  filter(proposal_eav > taxed_eav | zero_bill == 1) |>
  ggplot()+
  geom_histogram(aes(x=bill_change), binwidth = 50) +
  theme_classic() +
  scale_x_continuous(n.breaks = 10)+
  labs(title = "Distribution of Tax Bill Change", 
       x = "Tax Bill Change ($)",
       y = "# of Bills")

taxcode_rates  <- dbGetQuery(con, "
SELECT DISTINCT year, tax_code_num, tax_code_rate
FROM tax_code
                             ")


taxyear = 2023
taxyear = as.data.frame(taxyear)
source("./scripts/helper_tc_muninames.R") 

tc_muninames <- tc_muninames |> select(tax_code_num, tax_code_rate, clean_name, Triad, Township)


pin_data <- pin_data |>
  left_join(tc_muninames, by = c("tax_code_num")) |>
  left_join(eq_factor, by = "year") |>
 # left_join(taxcode_rates, by = c("year", "tax_code_num")) |>
  mutate(zero_bill = ifelse(tax_bill_total == 0, 1, 0)) |>
  mutate(eq_av = av_clerk*eq_factor_final) |>
  mutate(exe_total = rowSums(across(starts_with("exe_"))),
         exe_total = ifelse(exe_total > eq_av, eq_av, exe_total)) |>
  mutate(taxable_eav = eq_av - exe_total) |>
  mutate(flag_missingdata = ifelse(taxable_eav > 1000, 1, 0)) |>
  mutate(exe_missing_disvet = ifelse(taxable_eav > 1000, taxable_eav, 0)) |>
  mutate(taxable_eav_adj = ifelse(taxable_eav > 1000 & flag_missingdata == 1, 0 , taxable_eav)) |>
  mutate(proposal_eav = eq_av * 0.1) |> 
  mutate(proposal_taxbill = proposal_eav * tax_code_rate/100) |>
  #mutate(no_eav = ifelse(taxable_eav <= 0, 1, 0)) |>
  #mutate(exemps_no_eav = ifelse(eq_av < exe_total, 1, 0)) |>
  select(-av_mailed, -av_certified, -av_board) |>
  mutate(exe_total_adj = rowSums(across(starts_with("exe_")))-exe_total) # don't count the total value when summing the values 

# Over 27,000 residential PINs did not have to pay a tax bill in 2023.
pin_data |> 
  # filter(taxable_eav_adj > 0 ) |> 
  group_by(year) |> 
  summarize( n = n(), 
             n_disvet = sum(exe_missing_disvet > 0),
             n_ghe = sum(exe_homeowner > 0),
             n_senior = sum(exe_senior > 0),
             n_freeze = sum(exe_freeze > 0),
             
             exe_missing_disvet = sum(exe_missing_disvet),
             exe_vet_dis = sum(exe_vet_dis_lt50+exe_vet_dis_50_69+exe_vet_dis_ge70),
             exe_homeowner = sum(exe_homeowner),
             exe_senior = sum(exe_senior),
             exe_freeze = sum(exe_freeze),
             
             taxable_eav = sum(taxable_eav), 
             taxable_eav_adj = sum(taxable_eav_adj),
           
             exe_total = sum(exe_total),
             exe_total_adj = sum(exe_total_adj),
             
             shifted_rev = sum(0.1*eq_av * tax_code_rate/100),  # if all properties with $0 taxbills had 10% of their equalized AV taxed at current tax rate
             #  shifted_rev_adj = sum(taxable_eav_adj * tax_code_rate/100)
             
            ) |> mutate(
              avg_taxable_eav= taxable_eav_adj/n) |> View()

hist(pin_data$eq_av, breaks = 100, xlim = c(0,300000))

 # ALL zero-dollar PINs in 2023 -------------------------------

## Total: 26,979 <<<- this value

# NOTE: 17,000+ taxbills are zero dollar bills when using the tax_bill() command.
# tax_bill() sums the exemptions for each pin and then calculates the bill 
# independently from the bill amount in the pin table

pin_data |>
  filter(year == 2023 & av_clerk > 0) |>
  filter(tax_bill_total == 0) |>
  summarize(n = n()) # 27053, 26,979 with  an av > 0

## AV of 0: 74 

pin_data |>
  filter(year == 2023) |>
  filter(av_clerk == 0) |>
  summarize(n = n())


## AV > 0, Taxable EAV <= 0: 9622 

pin_data |>
  filter(year == 2023) |>
  filter(av_clerk > 0 & taxable_eav <= 0) |>
  summarize(n = n())

## AV > 0 & taxable EAV = 0: 16,843      <-- this value after adjusting likely missing exemptions
pin_data |>
  filter(year == 2023) |>
  filter(av_clerk > 0 & taxable_eav_adj <= 0) |>
  summarize(n = n())

## Taxable EAV > 0 and Zero-Bill: 17357  <<-- this value based on ptaxsim pin table

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav > 0 & tax_bill_total == 0) |>
  summarize(n = n())

## Taxable EAV > 0 and Zero-Bill: 10,136 <<-- this value after adjusting missing exemptions
pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav_adj > 0 & tax_bill_total <= 0) |>
  summarize(n = n())



## Taxable EAV between 0 and 150: 10,003

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav > 0 & taxable_eav < 150 & tax_bill_total <= 0) |>
  summarize(n = n())

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav_adj > 0 & taxable_eav_adj < 150 & tax_bill_total <= 0) |>
  summarize(n = n())



## Taxable EAV > $150: 7354
## Adjusted Taxable EAV > $150: 133 residential PINs in 2023

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav > 150 & tax_bill_total <= 0) |>
  summarize(n = n())

pin_data |>
  filter(year == 2023) |>
  filter(taxable_eav_adj > 150 & tax_bill_total <= 0) |>
  summarize(n = n())



# Section 18-40 Zero Dollar Bills ----------------------------------

# These bills should have an EAV < $150 and > 0 after accounting for exemptions
# but they tend to have EAVs much greater than 150, even after exemptions are subtracted
# potentially due to missing exemption values (such as the disabled veteran exemption values noticed by the RAs)



## All with Taxable EAV between 0 and 150 --------------------

# $5,000 could be collected from harassing 10,000 residents for their tiny amount of taxbills that are current waived 

# Using maximum calculations
pin_data |>
  filter(taxable_eav > 0 & taxable_eav <= 150 & tax_bill_total == 0) |>
  group_by(year) |>
  summarize(n = n(),
            eq_av = sum(av_clerk*eq_factor_final, na.rm=TRUE),
            total_exempt = sum(exe_total), 
            taxed_eav = sum(av_clerk*eq_factor_final - exe_total, na.rm=T),
            billed = sum(tax_bill_total),
            max_av = max((av_clerk*eq_factor_final- exe_total)/eq_factor_final),
            max_eav = max(av_clerk*eq_factor_final),
            max_taxable_eav = max(av_clerk*eq_factor_final- exe_total))


# Raw Values
pin_data |>
  filter(taxable_eav > 0 & taxable_eav <= 150 & tax_bill_total == 0) |>
  group_by(year) |>
  summarize(n = n(),
            sum_av = sum(av_clerk, na.rm = T),
            sum_eq_av = sum(eq_av, na.rm=TRUE),
            sum_eav_exempt = sum(exe_total, na.rm = T),
            sum_adj_eav_exempt = sum(exe_total_adj),
            sum_taxable_eav = sum(taxable_eav, na.rm=T),
            sum_adj_taxable_eav = sum(taxable_eav_adj, na.rm=T),
            
            sum_billed = sum(tax_bill_total),
            sum_foregone = sum(taxable_eav*tax_code_rate/100, na.rm = T))
# $5000 from properties with taxable eav between 0 and 150.
# 10,000 from properties with taxable eav > 0

# double checking
pin_data |> filter(exe_total < av_clerk*eq_factor_final ) %>%
  reframe(n=n(),
          sum(eq_av), sum(exe_total), sum(taxable_eav), billed = sum(tax_bill_total),
          .by = year)

## Zero Bill with EAV > $150 --------------------------------
# using values from ptaxsim pin table, which might be missing exemption
pin_data |>
  filter(taxable_eav > 150 & tax_bill_total == 0) |>
  group_by(year) |>
  summarize(n = n(),
            sum_av = sum(av_clerk, na.rm = T),
            sum_eq_av = sum(eq_av, na.rm = T),
            sum_exe_total = sum(exe_total, na.rm = T),
            sum_taxable_eav = sum(taxable_eav, na.rm = T),
            sum_foregone = sum(.1*taxable_eav*tax_code_rate/100, na.rm = T)) |>
  View()

pin_data |>
  filter(taxable_eav_adj > 0) |>
  group_by(year) |>
  summarize(n = n(),
            sum_av = sum(av_clerk, na.rm = T),
            sum_eq_av = sum(eq_av, na.rm = T),
            sum_exe_total = sum(exe_total, na.rm = T),
            sum_exe_total_adj = sum(exe_total_adj, na.rm = T),
            
            sum_taxable_eav = sum(taxable_eav, na.rm = T),
            sum_taxable_eav_adj = sum(taxable_eav_adj, na.rm = T),
            proposal_rev = sum(.1*eq_av*tax_code_rate/100, na.rm = T),
            sum_1840_foregone = sum(taxable_eav*tax_code_rate/100, na.rm = T)
            ) |>
  View()


pin_data |>
  filter(tax_bill_total == 0 & av_clerk > 0) |>
  group_by(year) |>
  summarize(n = n(),
            n_disvet = sum(exe_missing_disvet > 0),
            n_disvet_total = sum(exe_vet_dis_lt50+exe_vet_dis_ge70),
            n_ghe = sum(exe_homeowner > 0),
            n_senior = sum(exe_senior > 0),
            n_freeze = sum(exe_freeze > 0),
            exe_vet_dis_total = sum(exe_missing_disvet + exe_vet_dis_ge70),
            
            exe_missing_disvet = sum(exe_missing_disvet),
            exe_other_vet_dis = sum(exe_vet_dis_lt50+exe_vet_dis_50_69),
            exe_homeowner = sum(exe_homeowner),
            exe_senior = sum(exe_senior),
            exe_freeze = sum(exe_freeze),
            sum_av = sum(av_clerk, na.rm = T),
            sum_eq_av = sum(eq_av, na.rm = T),
            sum_exe_total = sum(exe_total, na.rm = T),
            sum_exe_total_adj = sum(exe_total_adj, na.rm = T),
            
            sum_taxable_eav = sum(taxable_eav, na.rm = T),
            sum_taxable_eav_adj = sum(taxable_eav_adj, na.rm = T),
            proposal_rev = sum(.1*eq_av*tax_code_rate/100, na.rm = T),
            sum_1840_foregone = sum(taxable_eav*tax_code_rate/100, na.rm = T)
  ) |>
  View()

# More Exemptions than EAV -----------------------------------------

# These properties do not have tax bills due to having exemptions that remove all taxable value.
# Over 9600 PINs had enough in exemptions to cover all of their EAV

pin_data |> filter(exe_total >= av_clerk*eq_factor_final ) %>%
  mutate(skin_in_game = 0.1*av_clerk*eq_factor_final*tax_code_rate) |>
  reframe(n=n(),
          rev_change = sum(skin_in_game),
          .by = year)



pin_data |> filter(av_clerk == 0 ) %>%
  reframe(n=n(), .by = year)

pin_data |> filter(av_clerk  < 150 ) %>%
  reframe(n=n(), .by = year)

## All Exempt EAV > eq_av Zero-Bills: 9696

pin_data |>
  filter(year == 2023) |>
  filter(exe_total >= eq_av) |>
  summarize(n = n())
