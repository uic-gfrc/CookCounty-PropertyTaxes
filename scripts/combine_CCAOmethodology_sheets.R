# Required libraries
library(tidyverse)
library(readxl)


# Helper function to read sheets
read_sheets <- function(file_path) {
  # Get the list of sheets, excluding summary sheets
  sheets <- excel_sheets(file_path)
  sheets <- sheets[!sheets %in% "Summary"]
  
  # Create a dataframe by combining each sheet's content
  dfs <- map_dfr(sheets, function(sheet) {
    read_excel(file_path, sheet = sheet) %>%
      mutate(sheet_name = sheet, file_name = basename(file_path)) %>%
     mutate(across(everything(), as.character))
  })
  
  return(dfs)
}


# Main script
dir_path <- "inputs/methodologyreports/"   # Path to the directory containing the files

# Get the list of files
file_paths <- list.files(dir_path, pattern = "*.xlsx", full.names = TRUE)

# Combine data from all files
final_df <- map_dfr(file_paths, read_sheets)

# Output or further processing...
# For example, save to a CSV file
#write_csv(final_df, "combined_data.csv")

library(naniar)
library(janitor)
# Rename variables using rename() function


## 14,575 obs after importing South Triad excel files. 


final_df <- final_df %>%
  replace_with_na_all(~.x == "NA" )  %>% 
  clean_names()

final_df %>% write_csv("Output/CCAOmethodologyreports_temp.csv")

data_renamed <- final_df %>%
  mutate(
    
    adj_rent_sf = ifelse(is.na(adj_rent_sf), adj_rent_sf_2, adj_rent_sf),
    bldg_sqft = ifelse(is.na(bldg_sqft), bldg_sq_ft, bldg_sqft ),
    bldg_sqft = ifelse(is.na(bldg_sqft), bldg_sf, bldg_sqft ),
    excess_land_area = ifelse(is.na(excess_land_area), excess_land_area_2, excess_land_area),

    excess_land_value = ifelse(is.na(excess_land_value), excess_land_value_2, excess_land_value),
    excess_land_value = ifelse(is.na(excess_land_value), excess_land_value_3, excess_land_value),
   # final_market_value = ifelse(is.na(final_market_value), final_market_value_2, final_market_value),
    final_mv_sf = ifelse(is.na(final_mv_sf), final_mv_sf_2, final_mv_sf),
    mobile_home_pads = ifelse(is.na(mobile_home_pads), mobile_home_pads_2, mobile_home_pads),
    pct_owner_interest = ifelse(is.na(pct_owner_interest), pct_owner_interest_2, pct_owner_interest),
    percent_exp = ifelse(is.na(percent_exp), percent_exp_2, percent_exp),
    property_description = ifelse(is.na(property_description), property_description_2, property_description),
    total_land_sf = ifelse(is.na(total_land_sf), total_land_sf_2, total_land_sf),
   total_exp = ifelse(is.na(total_exp), total_exp_2, total_exp),
   
   market_value = ifelse(is.na(market_value), market_value_2, market_value),
   x1br_units = ifelse(is.na(x1br_units), x1br_units_2, x1br_units),
   x2br_units = ifelse(is.na(x2br_units), x2br_units_2, x2br_units),
   x3br_units = ifelse(is.na(x3br_units), x3br_units_2, x3br_units),
   x2023_permit_partial_demo_value = ifelse(is.na(x2023_permit_partial_demo_value), x2023_permit_partial_demo_value_2, x2023_permit_partial_demo_value),
   vacancy_percent = ifelse(is.na(vacancy_percent), percent_vac, vacancy_percent),
   year_built = ifelse(is.na(year_built), year_built_2, year_built),
   year_built = ifelse(is.na(year_built), year_built_3, year_built),
   final_mv_sf = ifelse(is.na(final_mv_sf), final_mv_sf_2, final_mv_sf) 
   )     %>% 
  select(-c(adj_rent_sf_2, final_mv_sf_2, year_built_2, year_built_3, total_exp_2, percent_exp_2, excess_land_value_2, excess_land_value_3, bldg_sq_ft, bldg_sf, excess_land_value_2, excess_land_value_3, market_value_2,
            property_description_2, pct_owner_interest_2, mobile_home_pads_2, x2023_permit_partial_demo_value_2, x1br_units_2, x2br_units_2, x3br_units_2 )) %>%
  select(pin = ias_world_pi_ns, key_pin, classes, market_value, everything()) %>%
mutate(across(.cols = c(adj_rent_sf:percent_exp, total_land_sf:year_built), .fns = as.numeric)) 





# mutate(
# 
#     year_built = ifelse(is.na(`Year Built`), `YearBuilt`, `Year Built`),
#     land_sqft = ifelse(is.na(`Land Sqft`), `Land SF`, `Land Sqft`),
#     building_sqft = ifelse(is.na(BldgSqft), `Bldg SF`, `BldgSqft`),
#  #   num_beds = ifelse(is.na(`# of beds`), Num_beds, `# of beds`), 
#     vacancy_percent = ifelse(is.na(`Vacancy %`), `% Vac.`, `Vacancy %`),
#     expense_percent = ifelse(is.na(`Exp %`), `% Exp.`, `Exp %`),
#    # market_value_per_key = ifelse(is.na(`MV $ / Key`),  `MV_Per_Key`, `MV $ / Key`),
#   ) %>% 
#   select(-c(`Year Built`, `YearBuilt`, 
#             `Land Sqft`, `Land SF`, 
#             `BldgSqft`, `Bldg SF`, 
#            # `# of beds`, 
#             #`Num_beds`, 
#             `Vacancy %`, `% Vac.`, `Exp %`, `% Exp`,
#            #`MV $ / Key`, 
#             #`MV_Per_Key` 
#             )) %>%
#   rename(
#     keypin = KeyPIN,
#     pin = `iasWorld PINs`,
#     classes = Classes,
#     address = Address,
#     tax_dist = `Tax Dist`,
#     property_use = `Property Use`,
#     idph_license = `IDPH License #`,
#     noi = NOI,
#     cap_rate = `Cap Rate`,
#     market_value_per_bed = `Market Value $ / Bed`,
#     market_value = `Market Value`,
#     permit_partial_demo_value_2023 = `2023 Permit / Partial / Demo Value`,
#     sheet_name = sheet_name,
#     file_name = file_name,
#     property_description = `Property Description`,
#     revenue_perbedperday = `Revenue Bed/Day`,
#     pgi = PGI,
#     hotel_class = `Hotel Class`,
#     num_rooms = `# Of Rooms`,
#     num_beds = `# of beds`, 
#     market_value_per_key =`MV $ / Key`,
#     
#     category = Category,
#     avg_daily_rate = `Avg Daily Rate`,
#     occupancy_percent = `Occ. %`,
#   # revpar = `Rev`
#     total_revenue = `Total Rev`,
#     ebitda_noi = `EBITDA / NOI`,
#     adj_sale_per_sf = `Adj. Sale $/SF`,
#     total_land_sf = `Total Land SF`,
#     net_rentable_sf = `Net Rentable SF`,
#     investment_rating = `Investment Rating`,
#     adj_rent_per_sf = `Adj Rent $/SF`,
#     vc = `V/C`,
#     egi = EGI,
#     percent_owner_interest = `Pct Owner Interest`,
#     #expense_percent = `% Exp`,
#     total_expense = `Total Exp`,
#     final_mv_per_sf = `Final MV / SF`,
#     excess_land_area = `Excess Land Area`,
#     excess_land_value = `Excess Land Value`,
#     studio_units = `Studio Units`,
#     br_unit2_1 = `1BR Units`,
#     br_units_2 = `2BR Units`,
#     br_units_3 = `3BR Units`,
#     apt = Apt,
#     total_units = `Total Units`,
#     comm_sf = `Comm SF`,
#     oil_tank_value = `Oil Tank Value`
#   )
# 
# 
# 
# # Rename variables using rename() function
# data_renamed <- data %>%
#   rename(
#     keypin = key_pin,
#     year_built = ifelse("year_built" %in% names(data), year_built, `year_built_2`),
#     property_description = ifelse("property_description" %in% names(data), property_description, `property_description_2`),
#     land_sqft = ifelse("land_sqft" %in% names(data), land_sqft, `land_sf`),
#     bldg_sqft = ifelse("bldg_sqft" %in% names(data), bldg_sqft, `bldg_sq_ft`),
#     number_of_beds = ifelse("number_of_beds" %in% names(data), number_of_beds, `studio_units`),
#     avg_daily_rate = avg_daily_rate,
#     occ_percent = ifelse("occ_percent" %in% names(data), occ_percent, `vacancy_percent`),
#     total_exp = ifelse("total_exp" %in% names(data), total_exp, `total_exp_2`),
#     mv_key = mv_key,
#     adj_sale_sf = ifelse("adj_sale_sf" %in% names(data), adj_sale_sf, `adj_rent_sf`),
#     final_mv_sf = ifelse("final_mv_sf" %in% names(data), final_mv_sf, `final_mv_sf_2`),
#     oil_tank_value = oil_tank_value,
#     percent_owner_interest = ifelse("percent_owner_interest" %in% names(data), percent_owner_interest, `pct_owner_interest`),
#     percent_exp = ifelse("percent_exp" %in% names(data), percent_exp, `percent_exp_2`),
#     final_market_value = final_market_value
#   ) %>%
#   # Reorder variables
#   select(keypin, pin, year_built, sheet_name, file_name, address, adj_sale_sf, avg_daily_rate, bldg_sqft, land_sqft, mv_key, number_of_beds, occ_percent, oil_tank_value, percent_exp, percent_owner_interest, property_description, total_exp, final_market_value)



write_csv(data_renamed, "Output/combined_methodologyworksheets.csv")

# PINs and keyPINs-------------------------------------------------------------

pins_pivot <- data_renamed %>% 
  
  mutate(keypin_concat = as.character(key_pin),
         keypin_concat = str_remove_all(keypin_concat, "-"),
         keypin_concat = str_pad(keypin_concat, 14, "left", pad = "0"),
         pin = as.character(pin),
         
         pin_concat = str_remove_all(pin, "-"),
         pin_concat = str_pad(pin_concat, 14, "left", pad = "0"),
         major_class = str_sub(classes, 1, 1))  %>%
  select(keypin_concat, pin_concat, pin, major_class)  %>%
  mutate(pins2 = str_split(pin_concat, pattern = " ")) %>%
  unnest(pins2) %>%
  mutate( pins2 = trimws(pins2) ) %>%
#  filter(!is.na(pins2) | pins2 == " ") %>%
 #  mutate(pins3 = str_split(pins2, pattern = " ")) %>%
 # unnest(pins3) %>%
  mutate(check_me = ifelse(str_length(pins2)<14, 1, 0))



keypins <- unique(comval$keypin_concat)


sum(as.numeric(data_renamed$final_market_value), na.rm = TRUE)
sum(as.numeric(data_renamed$market_value), na.rm = TRUE)

