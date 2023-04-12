
# Sammanslagen ranking
setwd("---")

library(readr)
library(dplyr)
library(Matrix)
library(tidyr)
library(tidyverse)
library(brms)
library(brmsmargins)
library(xtable)
library(ggplot2)
library(ggthemes)
library(TTR)



# Hämta marginal effect som är beräknat till 0.3823431

marginal_effect <- read_rds("fit/ave_marginal_effect.rds")
ave_mar_effect  <- marginal_effect$M 

##### Inladdning av data från modeller och simuleringar -----
# Hämtar de skattade koefficienterna för kval och race
koef_driver_race       <- read_rds("fit/summary_koef_driver_race.rds")
koef_driver_kval       <- read_rds("fit/summary_koef_driver_kval.rds")
koef_team_race         <- read_rds("fit/summary_koef_team_race.rds")
koef_team_kval         <- read_rds("fit/summary_koef_team_kval.rds")

# Hömtar ut simuleringarna från kval och race
samples_koef_driver_race       <- read_rds("fit/samples_koef_driver_race.rds")
samples_koef_driver_kval       <- read_rds("fit/samples_koef_driver_kval.rds")
samples_koef_team_race         <- read_rds("fit/samples_koef_team_race.rds")
samples_koef_team_kval         <- read_rds("fit/samples_koef_team_kval.rds")



unika_forare_race <- unique(samples_koef_driver_race$Driver)
unika_forare_kval <- unique(samples_koef_driver_kval$Driver)
unika_forare_race <- as.character(unika_forare_race)
unika_forare_kval <- as.character(unika_forare_kval)



simuleringar_driver_kval <- samples_koef_driver_kval %>%
  filter(Driver %in% unika_forare_race) %>%
  mutate(Driver = as.factor(Driver), Year = as.integer(Year))

simuleringar_driver_race <- samples_koef_driver_race %>%
  filter(Driver %in% unika_forare_race) %>%
  mutate(Driver = as.factor(Driver), Year = as.integer(Year),
         skill_yr_race = skill_yr)

simuleringar_comb <- left_join(simuleringar_driver_race, simuleringar_driver_kval[, c(1,2,3,6)], by = c("Driver", ".draw", "Year"))
sim_comb_na <- simuleringar_comb %>% drop_na(skill_yr.x, skill_yr_race, skill_yr.y, Skill) 


#Skill_yr.x = Skill driver för race
#Skill_yr.y = Skill driver för kval


##### Plotning av data enbart för race -----
race_rank_peak <- 
  simuleringar_driver_race %>%
  group_by(Driver, Year) %>%
  summarise(
    est = mean(skill_yr_race),
    lower = quantile(skill_yr_race, 0.025),
    upper = quantile(skill_yr_race, 0.975)
  )
race_rank_peak_topp_15 <- race_rank_peak %>% group_by(Driver) %>% slice_max(n = 1, est) %>% arrange(desc(est))
race_rank_peak_topp_15 <- race_rank_peak_topp_15[1:15, ]

driver_skill_race_peak_plot <- race_rank_peak_topp_15 %>%
  ungroup %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(title = "F1-förarskicklighet peak Race",
       subtitle = "Med hänsyn till konstruktörskapacitet, topp 15",
       x = "Skill (log odds ratio)",
       y = "Driver")
driver_skill_race_peak_plot
ggsave("img/driver_skill_race_peak_topp15.png", plot = driver_skill_race_peak_plot, width = 12, height = 6, bg = "white")


##### Plotning av data enbart för kval -----
kval_rank_peak <- 
  simuleringar_driver_kval %>%
  group_by(Driver, Year) %>%
  summarise(
    est = mean(skill_yr),
    lower = quantile(skill_yr, 0.025),
    upper = quantile(skill_yr, 0.975)
  )
kval_rank_peak_topp_15 <- kval_rank_peak %>% group_by(Driver) %>% slice_max(n = 1, est) %>% arrange(desc(est))
kval_rank_peak_topp_15 <- kval_rank_peak_topp_15[1:15, ]

driver_skill_kval_peak_plot <- kval_rank_peak_topp_15 %>%
  ungroup %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(title = "F1-förarskicklighet peak Kval",
       subtitle = "Med hänsyn till konstruktörskapacitet, topp 15",
       x = "Skill (log odds ratio)",
       y = "Driver")
driver_skill_kval_peak_plot
ggsave("img/driver_skill_kval_peak_topp15.png", plot = driver_skill_kval_peak_plot, width = 12, height = 6, bg = "white")




##### Plottning av data sammansatt ranking -----

sim_comb_summary <- sim_comb_na %>%
  group_by(Driver, Year) %>%
  mutate(ave_marginal_effect = ave_mar_effect,
         race_skill = skill_yr.x * (1-ave_marginal_effect),
         quali_skill = skill_yr.y * ave_marginal_effect,
         comb_skill = race_skill + quali_skill
         )

comb_rank_summary <- 
  sim_comb_summary %>%
  group_by(Driver, Year) %>%
  summarise(
    est = mean(comb_skill),
    lower = quantile(comb_skill, 0.025),
    upper = quantile(comb_skill, 0.975)
  )



comb_rank <- comb_rank_summary %>% group_by(Driver) %>% slice_max(n = 1, est) %>% arrange(desc(est))
driver_skill_combined_peak_plot <- comb_rank[1:15, ] %>%
  ungroup %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(title = "F1-förarskicklighet peak Kombinerad",
       subtitle = "Med hänsyn till konstruktörskapacitet, topp 15",
       x = "Skill (log odds ratio)",
       y = "Driver")
driver_skill_combined_peak_plot
ggsave("img/driver_skill_combined_peak.png", plot = driver_skill_combined_peak_plot, width = 12, height = 6, bg = "white")





###### Moving Average ------
driver_peak_intervall <- data.frame(comb_rank[, 1:2])

#### Race
MA_peak_race      <- race_rank_peak %>% group_by(Driver) %>% filter(n() >= 3) %>% mutate(MA = runMean(est, 3)) %>% slice_max(n = 1, est) %>% arrange(desc(est))
MA_peak_race      <- data_frame(MA_peak_race[, 1:2], Startar = MA_peak_race$Year-2)
sim_race_MA_sort  <- left_join(simuleringar_driver_race, MA_peak_race[, c(1, 3)], by = c("Driver"))
sim_race_MA_sort  <- drop_na(sim_race_MA_sort,Startar)
sim_race_MA_sort  <- sim_race_MA_sort %>%
  group_by(Driver) %>%
  filter(Year >= Startar, Year <= Startar+2)

sim_race_summary_MA <- sim_race_MA_sort %>%
  group_by(Driver) %>%
  summarise( est = mean(skill_yr),
             lower = quantile(skill_yr, 0.025),
             upper = quantile(skill_yr, 0.975))
sim_race_summary_MA_ordered <- sim_race_summary_MA %>%  slice_max(n = 15, est)

driver_skill_race_MA_plot <- sim_race_summary_MA_ordered %>%
  ungroup %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") + 
  theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(title = "F1-förarskicklighet  3 år MA, enbart Race",
       subtitle = "Med hänsyn till konstruktörskapacitet, topp 15",
       x = "Skill (log odds ratio)",
       y = "Driver")
ggsave("img/driver_skill_race_peak_MA.png", plot = driver_skill_race_MA_plot, width = 12, height = 6, bg = "white")

#### Kval
MA_peak_kval      <- kval_rank_peak %>% group_by(Driver) %>% filter(n() >= 3) %>% mutate(MA = runMean(est, 3)) %>% slice_max(n = 1, est) %>% arrange(desc(est))
MA_peak_kval      <- data_frame(MA_peak_kval[, 1:2], Startar = MA_peak_kval$Year-2)
sim_kval_MA_sort  <- left_join(simuleringar_driver_kval, MA_peak_kval[, c(1, 3)], by = c("Driver"))
sim_kval_MA_sort  <- drop_na(sim_kval_MA_sort,Startar)
sim_kval_MA_sort  <- sim_kval_MA_sort %>%
  group_by(Driver) %>%
  filter(Year >= Startar, Year <= Startar+2)

sim_kval_summary_MA <- sim_kval_MA_sort %>%
  group_by(Driver) %>%
  summarise( est = mean(skill_yr),
             lower = quantile(skill_yr, 0.025),
             upper = quantile(skill_yr, 0.975))
sim_kval_summary_MA_ordered <- sim_kval_summary_MA %>%  slice_max(n = 15, est)

driver_skill_kval_MA_plot <- sim_kval_summary_MA_ordered %>%
  ungroup %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") +
  theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(title = "F1-förarskicklighet  3 år MA, enbart Kval",
       subtitle = "Med hänsyn till konstruktörskapacitet, topp 15",
       x = "Skill (log odds ratio)",
       y = "Driver")
ggsave("img/driver_skill_kval_peak_MA.png", plot = driver_skill_kval_MA_plot, width = 12, height = 6, bg = "white")






#### Kombinerad
MA_peak <- comb_rank_summary %>% group_by(Driver) %>% filter(n() >= 3) %>% mutate(MA = runMean(est, 3)) %>% slice_max(n = 1, est) %>% arrange(desc(est))
MA_peak <- data_frame(MA_peak[, 1:2], Startar = MA_peak$Year-2)  
sim_comb_MA_sort <- left_join(sim_comb_summary, MA_peak[, c(1, 3)], by = c("Driver"))
sim_comb_MA_sort <- drop_na(sim_comb_MA_sort,Startar) # Här droppar vi de förare som ej varit med i tre år eller fler
sim_comb_MA_sort <- sim_comb_MA_sort %>%
  group_by(Driver) %>%
  filter(Year >= Startar, Year <= Startar+2)

sim_comb_summary_MA <- sim_comb_MA_sort %>%
  group_by(Driver) %>%
  summarise( est = mean(comb_skill),
             lower = quantile(comb_skill, 0.025),
             upper = quantile(comb_skill, 0.975)
  )


sim_comb_summary_MA_ordered <- sim_comb_summary_MA %>%  slice_max(n = 15, est)

driver_skill_MA_plot <- sim_comb_summary_MA_ordered %>%
  ungroup %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") + 
  theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(title = "F1-förarskicklighet  3 år MA, kombinerad",
       subtitle = "Med hänsyn till konstruktörskapacitet, topp 15",
       x = "Skill (log odds ratio)",
       y = "Driver")
ggsave("img/driver_skill_peak_MA.png", plot = driver_skill_MA_plot, width = 12, height = 6, bg = "white")


