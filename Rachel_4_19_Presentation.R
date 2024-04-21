### Materials for Rachel Weber's 4/19/24 Slide Deck ###
### MVH 4-12-24 ###

# Load packages and data

library(tidyverse)

options(scipen = 999)

# Trends in Incentivized FMV as a percent of the base over time

## Data prep

muni_MC <- read_csv("./Output/ptaxsim_muni_class_summaries_2006-2022.csv") %>%
  select(year, clean_name, class, av)

class_dict <- read_csv("./Necessary_Files/class_dict_expanded.csv") %>%
  select(class = class_code, class_1dig, assess_ratio, incent_prop, Alea_cat, major_class_code)

muni_MC <- muni_MC %>%
  left_join(class_dict, by = c("class")) %>%
  filter(class !=0) # drop exempt property types with 0 taxable value

class_8_munis <- read_csv("./Necessary_Files/datarequests_Class8Munis.csv")
#class_8_munis <- read_csv("./Output/datarequests_Class8Munis.csv")

# changed from as.list to as.character
class_8_munis <- as.character(class_8_munis$clean_name)



# class 8 munis - at the year-class level
class_8_df <- # left_join(muni_MC, class_dict, by = "class") %>%
  muni_MC %>%
  filter(clean_name %in% class_8_munis) %>%
  filter(av != 0) %>%
  mutate(FMV = av/assess_ratio) %>%
  group_by(year) %>%
  mutate(year_tb_tot = sum(FMV)) %>%           # tax base for all class 8 munis together, per year
  ungroup() %>%
  filter(Alea_cat %in% c("Industrial", "Commercial")) %>%      ## drops all non-industrial and non-commercial classes to calculate the rest of the totals
  group_by(year) %>%
  mutate(year_ind_comm_FMV = sum(FMV)) %>%    #  total commercial and industrial FMV for all class 8 munis together, per year
  ungroup() %>%
  group_by(year, clean_name) %>%
  mutate(muni_year_ind_comm_FMV = sum(FMV)) %>%   # calculates total FMV in each munis  each year
  ungroup() %>%
  group_by(year, Alea_cat) %>%
  mutate(cat_year_FMV = sum(FMV)) %>%    # calculates FMV within each commercial vs industrial category for each year
  ungroup() %>%

  ## Do you want class_1dig or major_class_code? Does it matter?
  ## probably not if I already had joined in the commecial and industrial codes to the class level data...
  group_by(year, clean_name, class_1dig) %>%   # total fmv in class 5, 6, 7, and 8 per muni
  mutate(year_muni_class_FMV = sum(FMV))

## Added this for cook level totals:
class_8_df_outofCook <-
  muni_MC %>%
 # filter(clean_name %in% class_8_munis) %>% ## keep all munis, use for cook county totals.
  filter(av != 0) %>%
  mutate(FMV = av/assess_ratio) %>%
  group_by(year) %>%
  mutate(year_tb_tot = sum(FMV)) %>%           # tax base for all class 8 munis together, per year
  ungroup() %>%
  filter(Alea_cat %in% c("Industrial", "Commercial")) %>%      ## drops all non-industrial and non-commercial classes to calculate the rest of the totals
  group_by(year) %>%
  mutate(year_ind_comm_FMV = sum(FMV)) %>%    #  total commercial and industrial FMV for all class 8 munis together, per year
  ungroup() %>%
  group_by(year, clean_name) %>%
  mutate(muni_year_ind_comm_FMV = sum(FMV)) %>%   # calculates total FMV in each munis  each year
  ungroup() %>%
  group_by(year, Alea_cat) %>%
  mutate(cat_year_FMV = sum(FMV)) %>%    # calculates FMV within each commercial vs industrial category for each year
  ungroup() %>%

  ## Do you want class_1dig or major_class_code? Does it matter?
  ## probably not if I already had joined in the commecial and industrial codes to the class level data...
  group_by(year, clean_name, class_1dig) %>%   # total fmv in class 5, 6, 7, and 8 per muni
  mutate(year_muni_class_FMV = sum(FMV))

 class(class_8_df$class_1dig)   ## moved this lower because it didn't exist yet when you ran it

# Class 8 Munis

## Incentive types over time (line graph and line-area graph)

# ggplot() +
#   geom_line(data = class_8_df %>%
#              summarize(ind_comm_perc = year_ind_comm_FMV/year_tb_tot), aes(x = year, y = ind_comm_perc)) +#, color = "Ind. & Comm. FMV") +
#   geom_line(data = class_8_df %>%
#               filter(incent_prop == "Incentive") %>%
#               group_by(year) %>%
#               summarise(incent_perc = sum(FMV)/year_tb_tot),
#             aes(x = year, y = incent_perc)#,
#             #color = "Incent. Class FMV"
#             ) +
#   geom_line(data = class_8_df %>%
#               filter(class_1dig == 8)) +        # was missing a + sign here
#   theme_classic() +
#   scale_x_continuous(breaks = seq(2006, 2022, by = 3)) +
#   scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.45), breaks = seq(0, 0.5, by = 0.05))

 ## Alea Version:
ggplot() +
  geom_line(data = class_8_df %>%
              group_by(year) %>%
              summarize(ind_comm_perc = mean(year_ind_comm_FMV/year_tb_tot)),
            aes(x = year, y = ind_comm_perc, color =  "Commercial+Industrial"), lwd = 1) +

  # industrial fmv
  geom_line(data = class_8_df_outofCook %>%
              filter(Alea_cat == "Industrial") %>%
              group_by(year) %>%
              # needed na.rm=TRUE, otherwise it didn't work. perc_industrial was not being calculated without it.
              summarize(perc_industrial =  sum(FMV/year_tb_tot, na.rm=TRUE)),   ## added this part
            aes(x = year, y = perc_industrial, color = "Industrial"), lwd = 1) +

  # commercial fmv
  geom_line(data = class_8_df_outofCook %>%
              filter(Alea_cat == "Commercial") %>%
              group_by(year) %>%
              summarize(perc_commercial =  sum(FMV/year_tb_tot, na.rm=TRUE)),   ## added this part
            aes(x = year, y = perc_commercial, color = "Commercial"), lwd = 1) +
  geom_line(data = class_8_df %>%
              filter(incent_prop == "Incentive") %>%
              group_by(year) %>%
              summarise(incent_perc = sum(FMV)/year_tb_tot),
            aes(x = year, y = incent_perc, color = "Incentive Classes"), lwd = 1 ) +
  geom_line(data = class_8_df %>%               # threw error here, missing x and y in aes()
              filter(class_1dig == 8) %>%
              group_by(year) %>%
              summarize(perc_8 =  sum(FMV/year_tb_tot)),   ## added this part
            aes(x = year, y = perc_8, color = "Class 8"), lwd = 1) +        # was missing a + sign here
  theme_classic() +
  scale_x_continuous(name = "", breaks = seq(2006, 2022, by = 3), limits = c(2006, 2022), expand = c(0,0)) +
  scale_y_continuous(name = "Percent of FMV", labels = scales::percent_format(), limits = c(0, 0.20),
                     breaks = seq(0, 0.5, by = 0.05), expand = c(0,0))  +
 scale_color_manual(name = "", values = c("Commercial+Industrial" = "black", "Industrial" = "gray70", "Commercial" = "gray50",  "Incentive Classes" = "orange",  "Class 8" = "red" )) +
  theme(legend.position = "bottom") +
  labs(title = "Property in the Class 8 Townships") +
  guides(color = guide_legend(nrow=2, byrow = TRUE))



## Alea Version for Cook Level:
ggplot() +
  # Commercial + Industrial FMV in cook
  geom_line(data = class_8_df_outofCook %>%
              group_by(year) %>%  #didn't group by year before?
              summarize(ind_comm_perc = mean(year_ind_comm_FMV/year_tb_tot)),
            aes(x = year, y = ind_comm_perc, color =  "Commercial+Industrial"), lwd = 1) +

  # incentive class properties in cook
  geom_line(data = class_8_df_outofCook %>%
              filter(incent_prop == "Incentive") %>%
              group_by(year) %>%
              summarise(incent_perc = sum(FMV/year_tb_tot)),
            aes(x = year, y = incent_perc, color = "Incentive Classes"), lwd = 1 ) +

  # FMV with class 8 property class in cook county
   geom_line(data = class_8_df_outofCook %>%               # threw error here, missing x and y in aes()
              filter(class_1dig == 8) %>%
              group_by(year) %>%
              summarize(perc_8 =  sum(FMV/year_tb_tot)),   ## added this part
            aes(x = year, y = perc_8, color = "Class 8"), lwd = 1) +        # was missing a + sign here

  # industrial fmv in cook county
  geom_line(data = class_8_df_outofCook %>%               # threw error here, missing x and y in aes()
              filter(Alea_cat == "Industrial") %>%
              group_by(year) %>%
              # needed na.rm=TRUE, otherwise it didn't work. perc_industrial was not being calculated without it.
              summarize(perc_industrial =  sum(FMV/year_tb_tot, na.rm=TRUE)),   ## added this part
            aes(x = year, y = perc_industrial, color = "Industrial"), lwd = 1) +

  # commercial fmv in cook county
geom_line(data = class_8_df_outofCook %>%               # threw error here, missing x and y in aes()
            filter(Alea_cat == "Commercial") %>%
            group_by(year) %>%
            summarize(perc_commercial =  sum(FMV/year_tb_tot, na.rm=TRUE)),   ## added this part
          aes(x = year, y = perc_commercial, color = "Commercial"), lwd = 1) +

  # make it pretty:
   theme_classic() +
  scale_x_continuous(name = "", breaks = seq(2006, 2022, by = 3), limits = c(2006, 2022), expand = c(0,0)) +
  scale_y_continuous(name = "Percent of County FMV", labels = scales::percent_format(),  limits = c(0, 0.20),
                     breaks = seq(0, 0.5, by = 0.05), expand = c(0,0))  +
  scale_color_manual(name = "", values = c("Commercial+Industrial" = "black", "Industrial" = "gray80", "Commercial" = "gray40", "Incentive Classes" = "orange", "Class 8" =  "red")) +
  theme(legend.position = "bottom") +
  labs(title= "Cook County Commercial & Industrial FMV") + guides(color = guide_legend(nrow=2, byrow = TRUE))



###### I got the two graphs above to work and stopped here ########

#############################

## this one doesn't work because you never pass a data frame to it..

# Linegraph
# ggplot() +
#   geom_line(aes(x = year, y = total_incent_ratio, color = "Ind. & Comm. FMV"), linewidth = 1.5) +
#   geom_line(aes(x = year, y = inc_ind_ratio, color = "Industrial %"), linewidth = 1.5) +
#   geom_line(aes(x = year, y = inc_com_ratio, color = "Commercial %"), linewidth = 1.5) +
#   geom_line(aes(x = year, y = class_8_ratio, color = "Class-8 %"), linewidth = 1.5) +
#   labs(y = "percent") +
#   scale_color_manual(values = c("#b2182b", "#fd8d3c", "#878787", "#000000")) +
#   theme_classic() +
#   scale_x_continuous(breaks = seq(2006, 2022, by = 3)) +
#   scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.45), breaks = seq(0, 0.5, by = 0.05)) +
#   guides(color = guide_legend(title = NULL))


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

plot_df_FMV_cook <- left_join(muni_MC, class_dict, by = "class") %>%
  mutate(FMV = av/assess_ratio) %>%
  select(year, class_1dig, incent_prop, Alea_cat, FMV) %>%
  filter(Alea_cat %in% c("Commercial", "Industrial")) %>%
  group_by(year) %>%
  summarize(year_max_tb = sum(FMV)) %>%
 # select(year, year_max_tb) %>%
 # distinct() %>%
  arrange(year)


# is this for commercial FMV totals for county or class 8 munis?

plot_df_comm <-  muni_MC %>%
  left_join(muni_MC, class_dict, by = "class") %>%
  filter(Alea_cat == "Commercial") %>%
  mutate(FMV = av/assess_ratio) %>%
  #select(year, class_1dig, incent_prop, Alea_cat, FMV) %>%
#  group_by(year, class_1dig, incent_prop, Alea_cat, FMV) %>%
  reframe(year, class_1dig, incent_prop, Alea_cat, FMV,
          comm_tb = sum(FMV), .by = year) %>%
#  mutate(comm_tb = sum(FMV)) %>%    # total commercial FMV each year
 filter(class_1dig %in% c(7, 8)) %>%
  reframe(year, class_1dig, incent_prop, Alea_cat, comm_tb,
          comm_inc_FMV = sum(FMV), .by=year) %>%
 # mutate(comm_inc_FMV = sum(FMV)) %>% # amount FMV incentivized each year
 # ungroup() %>%
  arrange(year) #%>%
#  select(year, comm_inc_FMV, comm_tb) %>%   ### needed to select year here!!
 # distinct()

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


# Class 8 Muni Trends over Time

class8_df <- read_csv("./Output/class_8_ind_comm_FMV.csv")

class8_df_sums <- class8_df %>%
  group_by(year, incentive, type) %>%
  summarize(year, incentive, type, sum(FMV))


#make bar chart (NOT A HISTOGRAM)

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
  geom_col(aes(x = year - 0.15, y = total_incent_ratio, fill = "Total"), width = .1,
           position = position_dodge(width = 0.2)) +
  geom_col(aes(x = year - 0.05, y = inc_ind_ratio, fill = "Industrial"),
           position = position_dodge(width = 0.2), width = .1) +
  geom_col(aes(x = year + 0.05, y = inc_com_ratio, fill = "Commercial"),
           position = position_dodge(width = 0.2), width = .1) +
  geom_col(aes(x = year + 0.15, y = class_8_ratio, fill = "Class-8"),
           position = position_dodge(width = 0.2), width = .1) +
  scale_fill_manual(values = c("Total" = "#fecc5c", "Industrial" = "#fd8d3c",
                               "Commercial" = "#bd0026", "Class-8" = "#000000")) +
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


