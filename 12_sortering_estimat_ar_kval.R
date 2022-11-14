
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Test_egen_kval_och_grid")
# Den b?sta under tv? ?rs period

library(tidyverse)
library(brms)
library(patchwork)
library(glue)
library(TTR)


vilken_modell <- read_rds("fit/loo_results_kval.rds")
vilken_modell
fit <- read_rds("fit/fit_circuit_kval.rds") 


#####------ Kval 
koef_driver_kval            <- read_rds("fit/summary_koef_driver_kval.rds") #Hämta koefficienterna för kval
koef_team_kval              <- read_rds("fit/summary_koef_team_kval.rds")
unika_forare_kval           <- as.character(unique(koef_driver_kval$Driver))
koef_driver_kval$Driver     <- as.character(koef_driver_kval$Driver)
koef_driver_kval_sort       <- koef_driver_kval[koef_driver_kval$Driver %in% unika_forare_kval, ]
koef_driver_kval_sort$Driver<- as.factor(koef_driver_kval_sort$Driver)



## Sortering av data f?r plot
driver_skill_summary_sort <- arrange(koef_driver_kval_sort, Driver, Year)
driver_skill_summary_sort <- driver_skill_summary_sort %>%
  group_by(Driver) %>%
  filter(n() > 2) # Best?m filterv?rde f?r hur m?nga s?songer en m?ste ha k?rt f?r att f? ett v?rde 


# Plot av peak skicklighet
driver_skill_peak <- driver_skill_summary_sort %>% group_by(Driver) %>% slice_max(n = 1, est) %>% arrange(desc(est))
driver_skill_peak_kval <- driver_skill_peak[1:15, ] %>%
  ungroup %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") + 
  labs(title = "F1-förarskicklighet  peak",
       subtitle = "Med hänsyn till konstruktörskapacitet",
       x = "Skill (log odds ratio)",
       y = "Driver")
ggsave("img/driver_skill_peak_kval.png", plot = driver_skill_peak_kval, width = 9, height = 9, bg = "white")
plot(driver_skill_peak_kval)
xtable::xtable(driver_skill_peak_kval[1:15, ])



# Moving average av tre år
driver_skill_MA <- driver_skill_summary_sort %>%
  group_by(Driver) %>%
  mutate(MA = runMean(est, 2)) # Best?m filterv?rde f?r hur m?nga s?songer en m?ste ha k?rt f?r att f? ett v?rde 

driver_skill_max <- driver_skill_MA %>% group_by(Driver) %>% slice_max(n = 1, MA)
driver_skill_max$Year_min <- driver_skill_max$Year - 2

driver_skill_max_intervall <- driver_skill_max[, c(1, 7, 6, 2)]
driver_skill_max_MA_intervall <- left_join(driver_skill_summary_sort, driver_skill_max_intervall, by="Driver")
driver_skill_max_MA_intervall

df_driver_skill_max_MA_intervall <- driver_skill_max_MA_intervall %>%
  group_by(Driver) %>%
  filter(as.numeric(Year.x) >= as.numeric(Year_min) & as.numeric(Year.x) <= as.numeric(Year.y)) %>% arrange(desc(MA))
df_driver_skill_max_MA_intervall

df_driver_skill_max_MA_intervall <- df_driver_skill_max_MA_intervall[1:45, ] 
length(unique(df_driver_skill_max_MA_intervall$Driver)) #Skall vara 15 då vi tittar på topp 15 förare

plot_driver_ma <- df_driver_skill_max_MA_intervall %>%
  group_by(Driver) %>% 
  ggplot(aes(Year.x, est, ymin = lower, ymax = upper, color=Driver)) +
  geom_line() +
  geom_pointrange(colour = 2) + 
  xlim(1950, 2020)
plot(plot_driver_ma)

plot_driver_ma <- df_driver_skill_max_MA_intervall %>%
  group_by(Driver) %>% 
  ggplot(aes(Year.x, est, ymin = lower, ymax = upper, color=Driver)) +
  geom_ribbon(aes(fill = Driver), alpha = .2) +
  geom_line(aes(colour = Driver)) +
  geom_point(aes(colour = Driver))+ 
  xlim(1950, 2000) +
  labs(x = "Årtal", y = "Advantage (log odds ratio)  ", title = "Förarskicklighet topp 15 MA 3 år",
       subtitle = "Beräknat för Konstruktörsfördel")
plot(plot_driver_ma)
ggsave("img/driver_ma_skill_kval.png", plot = plot_driver_ma, width = 12, height = 9, bg = "white")



# Nedan är driver sqrt ( driver skill ) för utseendet
plot_driver_ma_sqrt <- df_driver_skill_max_MA_intervall %>%
  group_by(Driver) %>% 
  ggplot(aes(Year.x, sqrt(est), ymin = sqrt(lower), ymax = sqrt(upper), color=Driver)) +
  geom_ribbon(aes(fill = Driver), alpha = .2) +
  geom_line(aes(colour = Driver)) +
  geom_point(aes(colour = Driver))+ 
  xlim(1950, 2000) +
  labs(x = "Årtal", y = "Advantage sqrt( (log odds ratio) ) ", title = "Förarskicklighet topp 15 MA 3 år",
     subtitle = "Beräknat för Konstruktörsfördel")
plot(plot_driver_ma)
ggsave("img/driver_ma_skill_sqrt_kval.png", plot = plot_driver_ma_sqrt, width = 12, height = 9, bg = "white")


plot_driver_ma_enskild <- df_driver_skill_max_MA_intervall %>%
  ggplot(aes(x = Year.x, y = est, ymin = lower, ymax = upper)) +
  geom_ribbon(aes(fill = Driver), alpha = .2) +
  geom_line(aes(colour = Driver)) +
  geom_point(aes(colour = Driver)) +
  facet_wrap(~Driver) +
  labs(x = "Årtal", y = "Advantage (log odds ratio)", title = "Förarskicklighet topp 15 MA 3 år",
       subtitle = "Beräknat för Konstruktörsfördel")
plot(plot_driver_ma_enskild)

