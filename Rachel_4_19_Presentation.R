### Materials for Rachel Weber's 4/19/24 Slide Deck ###
### MVH 4-12-24 ###

# Load packages and data

library(tidyverse)

options(scipen = 999)

#Trends in Incentivized FMV as a percent of the base over time

# Linegraph

muni_MC <- read_csv("./Output/ptaxsim_muni_class_summaries_2006-2022.csv") %>%
  select(year, class, av)

class_dict <- read_csv("./Necessary_Files/class_dict_expanded.csv") %>%
  select(class = class_code, class_1dig, assess_ratio, incent_prop, Alea_cat)

plot_df <- left_join(muni_MC, class_dict, by = "class") %>%
  mutate(FMV = av/assess_ratio) %>%
  select(year, class_1dig, incent_prop, Alea_cat, FMV) %>%
  filter(Alea_cat %in% c("Commercial", "Industrial")) %>%
  reframe(FMV = sum(FMV), .by = c("year", "class_1dig", "Alea_cat", "incent_prop")) %>%
  group_by(year) %>%
  mutate(year_sum_FMV = sum(FMV)) %>%
  ungroup() %>%
  group_by(year, Alea_cat) %>%
  mutate(cat_year_FMV = sum(FMV)) %>%
  ungroup() %>%
  mutate(class_8 = ifelse(class_1dig == 8, 1, 0)) %>%
  group_by(year, class_8) %>%
  mutate(year_class_8_FMV = sum(FMV)) %>%
  ungroup() %>%
  arrange(desc(year))

plot_df_FMV <- left_join(muni_MC, class_dict, by = "class") %>%
  mutate(FMV = av/assess_ratio) %>%
  select(year, class_1dig, incent_prop, Alea_cat, FMV) %>%
  filter(Alea_cat %in% c("Commercial", "Industrial")) %>%
  group_by(year) %>%
  mutate(year_max_tb = sum(FMV)) %>%
  select(year, year_max_tb) %>%
  distinct() %>%
  arrange(year)

plot_df_8 <- left_join(muni_MC, class_dict, by = "class") %>%
  mutate(FMV = av/assess_ratio) %>%
  select(year, class_1dig, incent_prop, Alea_cat, FMV) %>%
  filter(Alea_cat %in% c("Commercial", "Industrial")) %>%
  group_by(year) %>%
  mutate(class_8 = ifelse(class_1dig == 8, 1, 0)) %>%
  filter(class_8 == 1) %>%
  group_by(year) %>%
  mutate(year_class_8_FMV = sum(FMV)) %>%
  arrange(year) %>%
  select(year_class_8_FMV) %>%
  distinct()

plot_df_comm <- left_join(muni_MC, class_dict, by = "class") %>%
  mutate(FMV = av/assess_ratio) %>%
  select(year, class_1dig, incent_prop, Alea_cat, FMV) %>%
  filter(Alea_cat %in% c("Commercial")) %>%
  group_by(year) %>%
  mutate(comm_tb = sum(FMV)) %>%
  filter(class_1dig %in% c(7, 8)) %>%
  mutate(comm_inc_FMV = sum(FMV)) %>%
  arrange(year) %>%
  select(comm_inc_FMV, comm_tb) %>%
  distinct()

plot_df_ind <- left_join(muni_MC, class_dict, by = "class") %>%
  mutate(FMV = av/assess_ratio) %>%
  select(year, class_1dig, incent_prop, Alea_cat, FMV) %>%
  filter(Alea_cat %in% c("Industrial")) %>%
  group_by(year) %>%
  mutate(ind_tb = sum(FMV)) %>%
  filter(class_1dig %in% c(6, 8)) %>%
  mutate(ind_inc_FMV = sum(FMV)) %>%
  arrange(year) %>%
  select(ind_inc_FMV, ind_tb) %>%
  distinct()


# Rename year columns in plot_df_comm, plot_df_ind, and plot_df_8

plot_df_comm <- rename(plot_df_comm, year_comm = year)
plot_df_ind <- rename(plot_df_ind, year_ind = year)
plot_df_8 <- rename(plot_df_8, year_8 = year)

# Combine data frames and remove redundant year columns
plot_final <- bind_cols(plot_df_FMV, plot_df_comm, plot_df_ind, plot_df_8) %>%
  select(-starts_with("year_"))



# plot_df <- plot_df %>%
#   filter(Alea_cat %in% c("Commercial", "Industrial")) #%>%
#   group_by(year) %>%
#   reframe(FMV, dig1FMV = sum(FMV), .by = c("year", "class_1dig")) %>%
#   reframe(alea_cat_FMV = sum(FMV), .by = c("year", "Alea_cat"))

#total_comm_ind_base = sum(all industrial & commercial FMV)
#total_ind_base = sum(all ind FMV)
#total_com_base = sum(all comm FMV)
#total_ind_incent = sum(AV for class 6)
##total_com_incent = sum(AV for class 7)
##total_8_incent = sum(AV for class 8)
#
#ind_ratio = total_ind_incent/total_ind_base
#com_ratio = total_com_incent/total_comm_base
#8_ratio = total_8_incent/total_comm_ind_base


# line_df <- plot_df %>%
#   filter(Alea_cat %in% c("Commercial", "Industrial")) %>%
#   mutate(FMV = av * assess_ratio) %>%
#   group_by(year) %>%
#   mutate(year = as.character(year)) %>%
#   rowwise() %>%
#   summarise(FMV = sum(FMV)) %>%
#   mutate(!!paste0("FMV_tot_", year) := FMV) %>%
#   select(-FMV)


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
    #caption = "Observation size represents total change in tax rate from eliminating exemptions and incentives. Observations in orange represent municipalities with the top 20 rate changes from both exemptions and incentives."
  ) +
  theme_classic()


