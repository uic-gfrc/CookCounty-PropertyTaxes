### Materials for Rachel Weber's 4/19/24 Slide Deck ###
### MVH 4-12-24 ###

# Load packages and data

options(digits = 2, scipen = FALSE)

library(tidyverse)

#Trends in Incentivized FMV as a percent of the base over time

# Linegraph

muni_MC <- read_csv("./Output/ptaxsim_muni_class_summaries_2006-2022.csv") %>%
  select(year, class, av)

class_dict <- read_csv("./Necessary_Files/class_dict_expanded.csv") %>%
  select(class = class_code, class_1dig, assess_ratio, incent_prop, Alea_cat)

plot_df <- left_join(muni_MC, class_dict, by = "class")

line_df <- plot_df %>%
  filter(Alea_cat %in% c("Commercial", "Industrial")) %>%
  mutate(FMV = av*assess_ratio) %>%
  group_by(year)

# Scatterplot
## Goal: scatterplot where change in tax rate from exemptions on x-axis and change from incentives on y-axis
## Dot size represents total tax rate change
## Another color dot will represent municipalities

## Generate data

rates <- read_csv("alt_rates.csv")

delta_rates <- rates %>%
  select(clean_name, delta_exe = change_noExe, delta_inc = change_noInc, delta_both = change_neither) %>%
  distinct() %>%
  arrange(desc(delta_both)) %>%
  mutate(top_both = ifelse(row_number() %in% 1:20, 1, 0)
  ) %>%
  arrange(desc(delta_exe)) %>%
  mutate(top_exe = ifelse(row_number() %in% 1:20, 1, 0)) %>%
  arrange(desc(delta_inc)) %>%
  mutate(top_inc = ifelse(row_number() %in% 1:20, 1, 0
                          )) %>%
  mutate(key_obs = ifelse(top_exe == 1 & top_inc == 1, 1, 0)) %>%
  mutate(key_obs = as.factor(key_obs))

## Create plot

delta_rates %>%
  ggplot(aes(delta_exe, delta_inc)) +
  geom_point(aes(size = delta_both, color = key_obs), alpha = .5) +
  scale_size_continuous(guide = "none") +
  scale_color_manual(guide = "none", values = c("green", "orange")) +
  labs(
    x = "Exemptions",
    y = "Incentives",
    caption = "Observation size represents total change in tax rate from eliminating exemptions and incentives. Observations in orange represent municipalities with the top 20 rate changes from both exemptions and incentives."
  ) +
  theme_classic()


