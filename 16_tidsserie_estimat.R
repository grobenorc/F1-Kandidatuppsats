
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Forts?ttningskurs/STAH11 Kandidatuppsats/Test_egen_kval_och_grid")
# Tidsserieanalys av Estimeringar (driver & constructor intercept)
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
drivers <- read_csv("dat/f1db_csv/drivers.csv")


##### Teamundersökning -----

# Plot av team koefficienter Race
win.graph()
par(mfcol=c(2, 1))

constructor_koefficienter_plot_race <- koef_team_race %>%
  group_by(Year) %>%
  ggplot(aes(x = factor(Year), y = est, color = est)) +
  geom_boxplot(alpha = .25) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(1950,2021,10)) + 
  labs(title = "Konstruktörsfördel Race 1950-2021",
       subtitle = "Grupperad per år",
       y = "Estimat",
       x = "Årtal") +
  guides(color = FALSE)
constructor_koefficienter_plot_race
ggsave("img/tidsserie_konstruktorsfordel_race.png", plot = constructor_koefficienter_plot_race, width = 12, height = 9, bg = "white")

constructor_koefficienter_plot_kval <- koef_team_kval %>%
  group_by(Year) %>%
  ggplot(aes(x = factor(Year), y = est, color = est)) +
  geom_boxplot(alpha = .25) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(1950,2021,10)) + 
  labs(title = "Konstruktörsfördel Kval 1950-2021",
       subtitle = "Grupperad per år",
       y = "Estimat",
       x = "Årtal") +
  guides(color = FALSE)
constructor_koefficienter_plot_kval
ggsave("img/tidsserie_konstruktorsfordel_kval.png", plot = constructor_koefficienter_plot_kval, width = 12, height = 9, bg = "white")



# Plot av varians per år konstruktörsfördel
varians_constructor_skill_plot_race <- koef_team_race %>%
  group_by(Year) %>%
  summarize(var_est = var(est, na.rm=TRUE)) %>%
  ggplot(aes(x = factor(Year), y = var_est, color = var_est)) +
  geom_line(alpha = .25) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks = seq(1950,2021,10)) + 
  labs(title = "Varians av konstruktörsfördel Race 1950-2021",
       subtitle = "Grupperad per år",
       y = "Estimat",
       x = "Årtal") +
  guides(color = FALSE)
varians_constructor_skill_plot_race
ggsave("img/tidsserie_varians_konstruktorsfordel_race.png", plot = varians_constructor_skill_plot_race, width = 12, height = 9, bg = "white")


# Plot av varians per år konstruktörsfördel
varians_constructor_skill_plot_kval <- koef_team_kval %>%
  group_by(Year) %>%
  summarize(var_est = var(est, na.rm=TRUE)) %>%
  ggplot(aes(x = factor(Year), y = var_est, color = var_est)) +
  geom_line(alpha = .25) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks = seq(1950,2021,10)) + 
  labs(title = "Varians av konstruktörsfördel Kval 1950-2021",
       subtitle = "Grupperad per år",
       y = "Estimat",
       x = "Årtal") +
  guides(color = FALSE)
varians_constructor_skill_plot_kval
ggsave("img/tidsserie_varians_konstruktorsfordel_kval.png", plot = varians_constructor_skill_plot_kval, width = 12, height = 9, bg = "white")



##### Förarundersökning -----

# Plot av förarkoefficienter
driver_koefficienter_plot_race <- koef_driver_race %>%
  group_by(Year) %>%
  ggplot(aes(x = factor(Year), y = est, color = est)) +
  geom_boxplot(alpha = .25) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(1950,2021,10)) + 
  labs(title = "Förarskicklighet Race 1950-2021",
       subtitle = "Grupperad per år",
       y = "Estimat",
       x = "Årtal") +
  guides(color = FALSE)
driver_koefficienter_plot_race
ggsave("img/tidsserie_forarskicklighet_race.png", plot = driver_koefficienter_plot_race, width = 12, height = 9, bg = "white")


driver_koefficienter_plot_kval <- koef_driver_kval %>%
  group_by(Year) %>%
  ggplot(aes(x = factor(Year), y = est, color = est)) +
  geom_boxplot(alpha = .25) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(1950,2021,10)) + 
  labs(title = "Förarskicklighet Kval 1950-2021",
       subtitle = "Grupperad per år",
       y = "Estimat",
       x = "Årtal") +
  guides(color = FALSE)
driver_koefficienter_plot_kval
ggsave("img/tidsserie_forarskicklighet_kval.png", plot = driver_koefficienter_plot_kval, width = 12, height = 9, bg = "white")






# Plot av varians per ?r f?rarskicklighet
varians_driver_skill_plot_race <- koef_driver_race %>%
  group_by(Year) %>%
  summarize(var_est = var(est, na.rm=TRUE)) %>%
  ggplot(aes(x = factor(Year), y = var_est, color = var_est)) +
  geom_line(alpha = .25) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks = seq(1950,2021,10)) + 
  labs(title = "Varians av förarskicklighet Race 1950 - 2021",
       subtitle = "Grupperad per år",
       y = "Estimat",
       x = "Årtal") +
  guides(color = FALSE)
varians_driver_skill_plot_race
ggsave("img/tidsserie_varians_forarskicklighet_race.png", plot = varians_driver_skill_plot_race, width = 12, height = 9, bg = "white")


varians_driver_skill_plot_kval <- koef_driver_kval %>%
  group_by(Year) %>%
  summarize(var_est = var(est, na.rm=TRUE)) %>%
  ggplot(aes(x = factor(Year), y = var_est, color = var_est)) +
  geom_line(alpha = .25) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks = seq(1950,2021,10)) + 
  labs(title = "Varians av förarskicklighet Kval 1950 - 2021",
       subtitle = "Grupperad per år",
       y = "Estimat",
       x = "Årtal") +
  guides(color = FALSE)
varians_driver_skill_plot_kval
ggsave("img/tidsserie_varians_forarskicklighet_kval.png", plot = varians_driver_skill_plot_kval, width = 12, height = 9, bg = "white")


