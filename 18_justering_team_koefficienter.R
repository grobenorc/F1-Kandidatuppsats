
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Forts?ttningskurs/STAH11 Kandidatuppsats/Test_egen_kval_och_grid")
## Justering för teamkoefficienter

#Tidsserie av Estimeringar (driver & constructor intercept)
library(tidyr)
library(Matrix)
library(tidyverse)
library(brms)
library(cmdstanr)
library(readr)
library(sjPlot)
library(ggplot2)
library(ggthemes)

### Data ###
koef_team_race    <- read_rds("fit/simulated_koef_team_race.rds")
koef_team_kval    <- read_rds("fit/simulated_koef_team_kval.rds")
koef_driver_race  <- read_rds("fit/simulated_koef_race.rds")
koef_driver_kval  <- read_rds("fit/simulated_koef_kval.rds")

constructors <- read_csv("dat/f1db_csv/constructors.csv")


##### LOESS Regression av teamkoefficienter
loess50               <- loess(est ~ Year, data = koef_team_race, span = .5)
koef_team_race        <- drop_na(koef_team_race) # 1 datapunkt i Koef_team_race saknas, detta är information om vilket stall.
koef_team_race_loess  <- data_frame(Year = koef_team_race$Year, LOESS = predict(loess50)) 
koef_team_race_loess  <- unique(koef_team_race_loess)
win.graph()
plot(koef_team_race_loess$Year, koef_team_race_loess$LOESS, ylim = c(-1.5, 1.5))
abline(h = mean(koef_team_race_loess$LOESS))


koef_team_race_loess_test <- koef_team_race_loess 
koef_team_race_loess_test$avst <- mean(koef_team_race_loess_test$LOESS)
koef_team_race_loess_test$absavst <- koef_team_race_loess_test$LOESS - koef_team_race_loess_test$avst 
koef_team_race_loess_test$absavst_quot <-  koef_team_race_loess_test$absavst / mean(koef_team_race_loess_test$LOESS)



constructor_advantage_sort_justering <- 

constructor_advantage_summary_justerad <-
  left_join(constructor_advantage_summary, constructor_advantage_sort, by = c("Year")) %>%
  mutate(Justering = LOESS / mean(LOESS), 
         Justerad_est = est * Justering,
         absolut = abs(est),
         Justering_abs = est + est * (1-Justering))

win.graph()
constructor_koefficienter_justerad_plot <- constructor_advantage_summary_justerad %>%
  group_by(Year) %>%
  ggplot(aes(x = factor(Year), y = Justering_abs, color = Justering_abs)) +
  geom_boxplot(alpha = .25) + theme_fivethirtyeight() +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(1950,2021,10)) + 
  labs(title = "Konstruktörsfördel 1950-2021",
       subtitle = "Grupperad per år, justerad") +
  guides(color = FALSE)
constructor_koefficienter_justerad_plot










##### Driver rating justering för 'inflation' av teamet justering per givet år #####

# Inhämta koefficienter
driver_mean <- as_draws_df(fit, variable = "r_driver\\[.+Intercept]", regex = TRUE) %>% select(-.chain, -.iteration)
driver_form <- as_draws_df(fit, variable = "r_driver:year\\[.+Intercept]", regex = TRUE) %>% select(-.chain,-.iteration)

driver_mean_long <-
  driver_mean  %>%
  pivot_longer(-.draw, names_to = "Driver", values_to = "Skill",
               names_pattern = "\\[(\\w+),") %>%
  mutate(Driver = as.factor(Driver))

driver_form_long <-
  driver_form %>%
  pivot_longer(-.draw, names_to = c("Driver", "Year"), values_to = "Form",
               names_pattern = "\\[(\\w+)_([0-9]{4}),") %>%
  mutate(Driver = as.factor(Driver), Year = as.integer(Year))

driver_samples <-
  left_join(driver_form_long, driver_mean_long, by = c("Driver", ".draw")) %>%
  mutate(skill_yr = Form + Skill)

driver_skill_summary <- #Här har vi alltså 95% konfidensintervall
  driver_samples %>%
  group_by(Driver, Year) %>%
  summarise(
    est = mean(skill_yr),
    lower = quantile(skill_yr, 0.025),
    upper = quantile(skill_yr, 0.975),
  ) 


#H?r b?rjar justeringen
driver_skill_inflation <- 
  left_join(driver_skill_summary, constructor_advantage_sort, by = c("Year"))
driver_skill_inflation <- drop_na(driver_skill_inflation)

driver_skill_inflation <- 
  driver_skill_inflation %>%
  mutate(Justering = LOESS / mean(LOESS), 
         Justerad_est = est * Justering)






