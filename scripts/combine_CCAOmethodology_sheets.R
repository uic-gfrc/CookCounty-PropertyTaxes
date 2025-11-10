# Required libraries
library(tidyverse)
library(readxl)
library(janitor)
library(naniar)

# Helper function to read sheets
read_sheets <- function(file_path) {
  # Get the list of sheets, excluding summary sheets
  sheets <- excel_sheets(file_path)
  sheets <- sheets[!sheets %in% "Summary"]
  
  # Create a dataframe by combining each sheet's content
  dfs <- map_dfr(sheets, function(sheet) {
    read_excel(file_path, sheet = sheet) %>%
      mutate(sheet_name = sheet, file_name = basename(file_path)) %>%
      
    clean_names() %>%  # Clean variable names using janitor::clean_names()
    #  map(~ .[, order(names(.))])  %>%   # Rearrange columns alphabetically
     mutate(across(everything(), as.character))
  })
  
  return(dfs)
}

# Deleted a summary table for affordable housing that was throwing off the  column names. 
# deleted from Hanover

# delete sheets 4 from Hanover


# Main script
dir_path_north <- "inputs/methodologyreports/north_2022"   # Path to the directory containing the files

# Get the list of files
file_paths <- list.files(dir_path_north, pattern = "*.xlsx", full.names = TRUE)
# Combine data from all files
final_df_north <- map_dfr(file_paths, read_sheets) 

final_df_north <- final_df_north %>% select(-c(x18:x29, x30:x32)) |>
  select(key_pin:file_name)

final_df_north <- final_df_north %>%
  mutate(pi_ns = ifelse(is.na(pi_ns), key_pin, pi_ns)) |>
  filter(!is.na(pi_ns))

write.csv(final_df_north, "Output/Combined Methodology Worksheets/combined_methodologyworksheets_NORTH2022.csv")


# South Triad Townships 2023 ----------------------------------------------


dir_path_south <- "inputs/methodologyreports/south_2023"   # Path to the directory containing the files
file_paths <- list.files(dir_path_south, pattern = "*.xlsx", full.names = TRUE
                         )

# Old way of combining files:
# works, but doesn't clean variables before combining 
 final_df_south <- map_dfr(file_paths, read_sheets)
#final_df_south

 final_df_south <- final_df_south |> 
   mutate(ias_world_pi_ns = ifelse(is.na(ias_world_pi_ns), key_pin, ias_world_pi_ns)) |>
   filter(!is.na(ias_world_pi_ns))
 
 final_df_south <- final_df_south %>%
   replace_with_na_all(~.x == "NA" )
 
 
 
 
write.csv(final_df_south, "Output/Combined Methodology Worksheets/combined_methodologyworksheets_SOUTH.csv")

 
 
# Chicago Sheets -------------------------------------------------------------

# Main script
dir_path_chicago <- "inputs/methodologyreports/chicago_2021"   # Path to the directory containing the files

# Get the list of files
file_paths <- list.files(dir_path_chicago, pattern = "*.xlsx", full.names = TRUE)
# Combine data from all files
final_df_chi <- map_dfr(file_paths, read_sheets) 

final_df_chi <- final_df_chi |>
  mutate(pi_ns = ifelse(is.na(pi_ns), key_pin, pi_ns)) |>
  filter(!is.na(pi_ns))

# final_df_chi <- final_df_chi %>%
#   mutate(across(replace_with_na,  "NA" ) )


write.csv(final_df_chi, "Output/Combined Methodology Worksheets/combined_methodologyworksheets_CHICAGO.csv")



# Chicago 2024 Townships --------------------------------------------------


# Main script
dir_path_chicago <- "inputs/methodologyreports/chicago_2024"   # Path to the directory containing the files

# Get the list of files
file_paths <- list.files(dir_path_chicago, pattern = "*.xlsx", full.names = TRUE)
# Combine data from all files
final_df_chi <- map_dfr(file_paths, read_sheets) 

final_df_chi <- final_df_chi |>
  mutate(ias_world_pi_ns = ifelse(is.na(ias_world_pi_ns), key_pin, ias_world_pi_ns)) |>
  filter(!is.na(ias_world_pi_ns))
# 
# final_df_chi <- final_df_chi %>%
#   replace_with_na_all(~.x == "NA" )

write.csv(final_df_chi, "Output/combined_methodologyworksheets_chicago2024.csv")
 

# North 2025 Townships ----------------------------------------------------

# Main script
dir_path_chicago <- "inputs/methodologyreports/north_2025"   # Path to the directory containing the files

# Get the list of files
file_paths <- list.files(dir_path_chicago, pattern = "*.xlsx", full.names = TRUE)
# Combine data from all files
final_df_chi <- map_dfr(file_paths, read_sheets) 


final_df_chi <- final_df_chi |>
  mutate(pi_ns = ifelse(is.na(pi_ns), key_pin, pi_ns)) |>
  filter(!is.na(pi_ns))
# 
# final_df_chi <- final_df_chi %>%
#   replace_with_na_all(~.x == "NA" )

write.csv(final_df_chi, "Output/Combined Methodology Worksheets/combined_methodologyworksheets_north2025.csv")  




# # Load all Excel files into a list of data frames
# load_clean_rearrange <- function(file) {
#   excel_sheets(file) %>%
#     set_names() %>%
#     map(read_excel, path = file) %>%
#     map(clean_names) %>%  # Clean variable names using janitor::clean_names()
#     map(~ .[, order(names(.))])  %>%   # Rearrange columns alphabetically
#     map(~ mutate_all(., as.character))  # Convert all variables to character type
# }
# 
# data_list <- lapply(file_paths, load_clean_rearrange)
#  
# all_column_names <- unique(unlist(unlist(lapply(data_list, names))))
# 
# 
# # Function to ensure all data frames have the same set of columns
# standardize_columns <- function(df) {
#   missing_columns <- setdiff(all_column_names, names(df))
#   df[, missing_columns] <- NA
#   df <- df[, all_column_names]
#   return(df)
# }
# 

# combined_data <- data_list %>%
#   bind_rows(.id = "source_file")
# # Output or further processing...
# # For example, save to a CSV file
# #write_csv(final_df, "combined_data.csv")



## 14,575 obs after importing South Triad excel files. 



#final_df %>% write_csv("Output/CCAOmethodologyreports_temp.csv")
# 
# data_renamed <- final_df %>%
#   mutate(
#     
#     adj_rent_sf = ifelse(is.na(adj_rent_sf), adj_rent_sf_2, adj_rent_sf),
#     bldg_sqft = ifelse(is.na(bldg_sqft), bldg_sq_ft, bldg_sqft ),
#     bldg_sqft = ifelse(is.na(bldg_sqft), bldg_sf, bldg_sqft ),
#     excess_land_area = ifelse(is.na(excess_land_area), excess_land_area_2, excess_land_area),
# 
#     excess_land_value = ifelse(is.na(excess_land_value), excess_land_value_2, excess_land_value),
#     excess_land_value = ifelse(is.na(excess_land_value), excess_land_value_3, excess_land_value),
#    # final_market_value = ifelse(is.na(final_market_value), final_market_value_2, final_market_value),
#     final_mv_sf = ifelse(is.na(final_mv_sf), final_mv_sf_2, final_mv_sf),
#     mobile_home_pads = ifelse(is.na(mobile_home_pads), mobile_home_pads_2, mobile_home_pads),
#     pct_owner_interest = ifelse(is.na(pct_owner_interest), pct_owner_interest_2, pct_owner_interest),
#     percent_exp = ifelse(is.na(percent_exp), percent_exp_2, percent_exp),
#     property_description = ifelse(is.na(property_description), property_description_2, property_description),
#     total_land_sf = ifelse(is.na(total_land_sf), total_land_sf_2, total_land_sf),
#    total_exp = ifelse(is.na(total_exp), total_exp_2, total_exp),
#    
#    market_value = ifelse(is.na(market_value), market_value_2, market_value),
#    x1br_units = ifelse(is.na(x1br_units), x1br_units_2, x1br_units),
#    x2br_units = ifelse(is.na(x2br_units), x2br_units_2, x2br_units),
#    x3br_units = ifelse(is.na(x3br_units), x3br_units_2, x3br_units),
#    x2023_permit_partial_demo_value = ifelse(is.na(x2023_permit_partial_demo_value), x2023_permit_partial_demo_value_2, x2023_permit_partial_demo_value),
#    vacancy_percent = ifelse(is.na(vacancy_percent), percent_vac, vacancy_percent),
#    year_built = ifelse(is.na(year_built), year_built_2, year_built),
#    year_built = ifelse(is.na(year_built), year_built_3, year_built),
#    final_mv_sf = ifelse(is.na(final_mv_sf), final_mv_sf_2, final_mv_sf) 
#    )     %>% 
#   select(-c(adj_rent_sf_2, final_mv_sf_2, year_built_2, year_built_3, total_exp_2, percent_exp_2, excess_land_value_2, excess_land_value_3, bldg_sq_ft, bldg_sf, excess_land_value_2, excess_land_value_3, market_value_2,
#             property_description_2, pct_owner_interest_2, mobile_home_pads_2, x2023_permit_partial_demo_value_2, x1br_units_2, x2br_units_2, x3br_units_2 )) %>%
#   select(pin = ias_world_pi_ns, key_pin, classes, sheet_name, file_name, market_value ,everything()) %>%
#   mutate(across(.cols = c(adj_rent_sf:percent_exp, total_land_sf:year_built), .fns = as.numeric)) 
# 
# 
# write.csv(data_renamed, "Output/combined_methodologyworksheets.csv")



# Unnesting -------------------------------------------------------
data_renamed_unnest <- data_renamed %>% 
  
  mutate(keypin_concat = as.character(key_pin),
         keypin_concat = str_remove_all(keypin_concat, "-"),
         keypin_concat = str_pad(keypin_concat, 14, "left", pad = "0"),
         pin = as.character(pin),
         
         pin_concat = str_remove_all(pin, "-"),
         pin_concat = str_pad(pin_concat, 14, "left", pad = "0"),
         major_class = str_sub(classes, 1, 1))  %>%
  mutate(pins2 = str_split(pin_concat, pattern = " ")) %>%
  unnest(pins2) %>%
  mutate(pins2 = trimws(pins2) ) %>%
  #  filter(!is.na(pins2) | pins2 == " ") %>%
  #  mutate(pins3 = str_split(pins2, pattern = " ")) %>%
  # unnest(pins3) %>%
  mutate(check_me = ifelse(str_length(pins2)<14, 1, 0))

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
  mutate(pins2 = trimws(pins2) ) %>%
#  filter(!is.na(pins2) | pins2 == " ") %>%
 #  mutate(pins3 = str_split(pins2, pattern = " ")) %>%
 # unnest(pins3) %>%
  mutate(check_me = ifelse(str_length(pins2)<14, 1, 0))



keypins <- unique(comval$keypin_concat)


sum(as.numeric(data_renamed$final_market_value), na.rm = TRUE)
sum(as.numeric(data_renamed$market_value), na.rm = TRUE)

table(pins_pivot$major_class)


