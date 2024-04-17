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

plot_df_final <- plot_df_FMV %>%
  left_join(plot_df_8, by = "year") %>%
  left_join(plot_df_comm) %>%
  left_join(plot_df_ind) %>%
#mutate ratio variables
  mutate(class_8_ratio = year_class_8_FMV/year_max_tb,
         inc_ind_ratio = ind_inc_FMV/ind_tb,
         inc_com_ratio = comm_inc_FMV/comm_tb,
         total_incent_ratio =
           (year_class_8_FMV + ind_inc_FMV + comm_inc_FMV)/year_max_tb)

#make line graph

plot_df_final %>%
  ggplot() +
  geom_line(aes(x = year, y = total_incent_ratio, color = "Ind. & Comm. FMV"), linewidth = 1.5) +
  geom_line(aes(x = year, y = inc_ind_ratio, color = "Industrial %"), linewidth = 1.5) +
  geom_line(aes(x = year, y = inc_com_ratio, color = "Commercial %"), linewidth = 1.5) +
  geom_line(aes(x = year, y = class_8_ratio, color = "Class-8 %"), linewidth = 1.5) +
  labs(y = "percent") +
  scale_color_manual(values = c("#b2182b", "#fd8d3c", "#878787", "#000000")) +
  theme_classic() +
  scale_x_continuous(breaks = seq(2006, 2022, by = 3)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.45), breaks = seq(0, 0.5, by = 0.05)) +
  guides(color = guide_legend(title = NULL))


#make bar chart (NOT A HISTOGRAM)

plot_df_final %>%
  ggplot() +
  geom_col(aes(x = year, y = total_incent_ratio), color = "#fecc5c") +
  geom_col(aes(x = year, y = inc_ind_ratio), color = "#fd8d3c") +
  geom_col(aes(x = year, y = inc_com_ratio), color = "#bd0026") +
  theme_classic()

plot_df_final %>%
  ggplot() +
  geom_col(aes(x = year - 0.1, y = total_incent_ratio, fill = "Total"), width = .1, position = position_dodge(width = 0.2)) +
  geom_col(aes(x = year, y = inc_ind_ratio, fill = "Industrial"), position = position_dodge(width = 0.2), width = .1) +
  geom_col(aes(x = year + 0.1, y = inc_com_ratio, fill = "Commercial"), position = position_dodge(width = 0.2), width = .1) +
  scale_fill_manual(values = c("Total" = "#fecc5c", "Industrial" = "#fd8d3c", "Commercial" = "#bd0026")) +
  theme_classic() +
  labs(x = "Year", y = "Ratio")

plot_df_final %>%
  ggplot() +
  geom_col(aes(x = year - 0.15, y = total_incent_ratio, fill = "Total"), width = .1, position = position_dodge(width = 0.2)) +
  geom_col(aes(x = year - 0.05, y = inc_ind_ratio, fill = "Industrial"), position = position_dodge(width = 0.2), width = .1) +
  geom_col(aes(x = year + 0.05, y = inc_com_ratio, fill = "Commercial"), position = position_dodge(width = 0.2), width = .1) +
  geom_col(aes(x = year + 0.15, y = class_8_ratio, fill = "Class-8"), position = position_dodge(width = 0.2), width = .1) +
  scale_fill_manual(values = c("Total" = "#fecc5c", "Industrial" = "#fd8d3c", "Commercial" = "#bd0026", "Class-8" = "#000000")) +
  theme_classic() +
  labs(x = "Year", y = "Percent") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.45), breaks = seq(0, 0.45, by = 0.05)) +
  scale_x_continuous(breaks = seq(2006, 2022, by = 3)) +
  guides(fill = guide_legend(title = NULL))


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
    caption = our_caption
  ) +
  theme_classic()


