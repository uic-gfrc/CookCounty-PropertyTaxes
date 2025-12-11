# must open file in PINs to Projects subfolder for file paths to work
# if you struggle to get the filepaths to work, close R, and open the file from 
# your folder and it will probably not open a project in R. That will use the 
# local directory that the file is in.

library(quarto)
library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)

# Read keypins to get township list
keypins <- read_xlsx("../Output/projects_checked_MAINFILE.xlsx") |>
  mutate(keypin = main_keypin)

# Change `Township` here if your township variable is named differently
townships <- keypins$Township |>
  unique() |>
  setdiff("Chicago") |>
  sort()

# townships <- keypins |>
#   distinct(Township) |>
#   filter(!Township =="Chicago") |>
#   select(Township)



for (place in townships){
  quarto_render(
    input         = "mapping_CI_projects_cleaned2.qmd",
    output_file   = paste0(place, "projects", sep = "-"),
    execute_params = list(township = place)
  )
}


# walk(
#   reports,
#   quarto_render,
#   .progress = TRUE
# )
# 
# quarto::quarot_render(
#   input         = "mapping_CI_projects_cleaned2.qmd",
#   execute_params = townships
# )
