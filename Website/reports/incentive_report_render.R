library(quarto)
library(tidyverse)

years <- c(2021, 2022, 2023) |> as.character()

reports <-
  tibble(
    input = "incentive-report-template.qmd",
    output_file = str_glue("incentive-report-{years}.html"),
    execute_params = map(years, ~ list(year = .))   # create a list where the dot represents each row of years in the vector of years
  )

pwalk(reports, quarto_render)
