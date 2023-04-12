setwd("---")
library(tidyverse)
library(TTR)



##### Konstruktörer ----
summary_team <-     read_rds("fit/summary_koef_team_race.rds")
summary_team_MA_intervall <-  summary_team %>% group_by(Constructor) %>% 
  filter(n()> 2) %>% 
  mutate(MA = runMean(est, 2)) %>% 
  slice_max(, order_by = MA, n =1) %>%
  mutate(Year_min = Year - 2,
         Year_max = Year) %>%
  select(Constructor, Year_min, Year_max)
head(summary_team_MA)




peak_plot_team_race <- function() {
  summary_team <- read_rds("fit/summary_koef_team_race.rds")
  plotdata <- summary_team %>% group_by(Constructor) %>% 
    slice_max( , order_by = est, n = 1) %>% 
    arrange(desc(est)) %>% 
    ungroup() %>% 
    slice(1:15) %>%
    mutate(Constructor = fct_reorder(Constructor, est)) %>%
    ggplot(aes(y = Constructor, x = est, xmin = lower, xmax = upper)) +
    geom_text(aes(label=Year), hjust=0.5, vjust=-1, size=3) +
    geom_pointrange(colour = "red") +
    labs(title = "F1-konstruktörsfördel Peak",
         subtitle = "Bästa respektive år per konstruktör \n 95% konfidens",
         x = "Konstruktörsfördel (log odds ratio)",
         y = "Konstruktör") +
    theme_classic() + geom_vline(xintercept = c(0.5, 1, 1.5), lty = 2, colour = "grey")
  ggsave("img/constructor_advantage_peak.png", plot = plotdata, width = 9, height = 9, bg = "white")
}



MA_peak_plot_team_race <- function() {
  summary_team <- read_rds("fit/summary_koef_team_race.rds")
  samples_team <- read_rds("fit/samples_koef_team_race.rds")
  #Hittar intervallet för de bästa åren för respektive konstruktör
  summary_team_MA_intervall <-  summary_team %>% group_by(Constructor) %>% 
    filter(n()> 2) %>% 
    mutate(MA = runMean(est, 3)) %>% 
    slice_max(, order_by = MA, n =1) %>%
    mutate(Year_min = Year - 2,
           Year_max = Year) %>%
    select(Constructor, Year_min, Year_max)
  #Filtrear ut simuleringarna för de bästa åren
  samples_MA <- left_join(samples_team, summary_team_MA_intervall, by = "Constructor") %>% 
    group_by(Constructor) %>% 
    filter(Year >= Year_min & Year <= Year_max)
  # Summerar simuleringarna över de tre åren
  samp <- samples_MA %>%
  group_by(Constructor) %>%
    summarise(
      est = mean(Form),
      lower = quantile(Form, 0.025),
      upper = quantile(Form, 0.975),
      Year_min = min(Year),
      Year_max = max(Year)) %>%
    mutate(Intervall = Year_max - Year_min) %>%
  filter(Intervall == 2)
  # Plottning 
  plottning <- samp %>% arrange(desc(est)) %>% ungroup() %>% slice(1:15) %>%
    mutate(Constructor = fct_reorder(Constructor, est)) %>%
    ggplot(aes(y = Constructor, x = est, xmin = lower, xmax = upper)) +
    geom_text(aes(label=paste(Year_min, " - ", Year_max), hjust=0.5, vjust=-1), size=3, show.legend = FALSE) +
    geom_pointrange(colour = "red") + theme_classic() + theme(axis.title = element_text()) +
    labs(title = "F1-konstruktörsfördel  3 år MA",
         subtitle = "Bästa 3-års perioden per respektive konstruktör",
         x = "Konstruktörsfördel (log odds ratio)",
         y = "Konstruktör") +
    geom_vline(xintercept = c(-0.5, 0, 0.5, 1, 1.5), lty = 2, colour = "grey")
  ggsave("img/constructor_advantage_MA.png", plot = plottning, width = 6, height = 8, bg = "white")
}
 

MA_plot_team_race <- function(){
  summary_team <- read_rds("fit/summary_koef_team_race.rds")
  #Hittar intervallet för de bästa åren för respektive förare
  summary_team_MA_intervall <-  summary_team %>% group_by(Constructor) %>% 
    filter(n()> 2) %>% 
    mutate(MA = runMean(est, 3)) %>% 
    slice_max(, order_by = MA, n =1) %>%
    mutate(Year_min = Year - 2,
           Year_max = Year) %>%
    select(Constructor, Year_min, Year_max)
  #Plottning
  summary_MA <- left_join(summary_team, summary_team_MA_intervall, by = "Constructor") %>% 
    group_by(Constructor) %>% 
    filter(Year >= Year_min & Year <= Year_max) %>%
    mutate(medel = mean(est)) %>% arrange(desc(medel)) %>% ungroup() %>% slice(1:45) %>% mutate(sek = rep(1:3, times = 15)) %>%
    ggplot(aes(sek, est, ymin = lower, ymax = upper, color=Constructor)) +
    geom_ribbon(aes(fill = Constructor), alpha = .2) +
    geom_line(aes(colour = Constructor)) +
    geom_point(aes(colour = Constructor)) +
    facet_wrap(~Constructor) +
    labs(x = "År", y = "Advantage (log odds ratio)", title = "Konstruktörsfördel topp 15 MA 3 år") + theme_classic() + geom_hline(yintercept = c(0, 0.5, 1, 1.5, 2), lty = 2, colour="grey") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.85)) + 
    scale_x_discrete(limits=1:3)
  ggsave("img/MA_intervall_team.png", plot = summary_MA, width = 9, height = 9)
}


##### Förare ----

peak_plot_driver_race <- function() {
  summary_driver <- read_rds("fit/summary_koef_driver_race.rds")
  plotdata <<- summary_driver %>% group_by(Driver) %>% 
    slice_max( , order_by = est, n = 1) %>% 
    arrange(desc(est)) %>% 
    ungroup() %>% 
    slice(1:15) %>%
    mutate(Driver = fct_reorder(Driver, est)) %>%
    ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
    geom_text(aes(label=Year), hjust=0.5, vjust=-1, size=3) +
    geom_pointrange(colour = "red") +
    labs(title = "F1-förarskicklighet Peak",
         subtitle = "Bästa respektive år per konstruktör \n 95% konfidens",
         x = "Konstruktörsfördel (log odds ratio)",
         y = "Konstruktör") +
    theme_classic() + geom_vline(xintercept = c(0.5, 1, 1.5, 2), lty = 2, colour = "grey")
  ggsave("img/driver_skill_peak.png", plot = plotdata, width = 9, height = 9, bg = "white")
}


MA_peak_plot_driver_race <- function() {
  summary_driver <- read_rds("fit/summary_koef_driver_race.rds")
  samples_driver <- read_rds("fit/samples_koef_driver_race.rds")
  #Hittar intervallet för de bästa åren för respektive förare
  summary_driver_MA_intervall <-  summary_driver %>% group_by(Driver) %>% 
    filter(n()> 2) %>% 
    mutate(MA = runMean(est, 3)) %>% 
    slice_max(, order_by = MA, n =1) %>%
    mutate(Year_min = Year - 2,
           Year_max = Year) %>%
    select(Driver, Year_min, Year_max)
  #Filtrear ut simuleringarna för de bästa åren
  samples_MA <<- left_join(samples_driver, summary_driver_MA_intervall, by = "Driver") %>% 
    group_by(Driver) %>% 
    filter(Year >= Year_min & Year <= Year_max)
  # Summerar simuleringarna över de tre åren
  samp <- samples_MA %>%
    group_by(Driver) %>%
    summarise(
      est = mean(skill_yr),
      lower = quantile(skill_yr, 0.025),
      upper = quantile(skill_yr, 0.975),
      Year_min = min(Year),
      Year_max = max(Year)) %>%
    mutate(Intervall = Year_max - Year_min) %>%
    filter(Intervall == 2)
  # Plottning 
  plottning <- samp %>% arrange(desc(est)) %>% ungroup() %>% slice(1:15) %>%
    mutate(Driver = fct_reorder(Driver, est)) %>%
    ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
    geom_text(aes(label=paste(Year_min, " - ", Year_max), hjust=0.5, vjust=-1), size=3, show.legend = FALSE) +
    geom_pointrange(colour = "red") + theme_classic() + theme(axis.title = element_text()) +
    labs(title = "F1-förarskicklighet  3 år MA",
         subtitle = "Bästa 3-års perioden per respektive förare",
         x = "Förarskicklighet (log odds ratio)",
         y = "Förare") +
    geom_vline(xintercept = c(0.5, 1, 1.5, 2), lty = 2, colour = "grey")
  ggsave("img/driver_skill_MA.png", plot = plottning, width = 6, height = 8, bg = "white")
  datan <<- plottning$data
}

datan$År <- paste(datan$Year_min, "-", datan$Year_max)
stargazer(datan[,c(1,2,3,4,8)], summary=FALSE)



MA_peak_plot_driver_race <- function() {
  summary_driver <- read_rds("fit/summary_koef_driver_race.rds")
  #Hittar intervallet för de bästa åren för respektive förare
  summary_driver_MA_intervall <-  summary_driver %>% group_by(Driver) %>% 
    filter(n()> 2) %>% 
    mutate(MA = runMean(est, 3)) %>% 
    slice_max(, order_by = MA, n =1) %>%
    mutate(Year_min = Year - 2,
           Year_max = Year) %>%
    select(Driver, Year_min, Year_max)
  #Filtrear ut simuleringarna för de bästa åren
  samples_MA <<- left_join(samples_driver, summary_driver_MA_intervall, by = "Driver") %>% 
    group_by(Driver) %>% 
    filter(Year >= Year_min & Year <= Year_max)
  # Summerar simuleringarna över de tre åren
  samp <- samples_MA %>%
    group_by(Driver) %>%
    summarise(
      est = mean(skill_yr),
      lower = quantile(skill_yr, 0.025),
      upper = quantile(skill_yr, 0.975),
      Year_min = min(Year),
      Year_max = max(Year)) %>%
    mutate(Intervall = Year_max - Year_min) %>%
    filter(Intervall == 2)
  # Plottning 
  plottning <- samp %>% arrange(desc(est)) %>% ungroup() %>% slice(1:15) %>%
    mutate(Driver = fct_reorder(Driver, est)) %>%
    ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
    geom_text(aes(label=paste(Year_min, " - ", Year_max), hjust=0.5, vjust=-1), size=3, show.legend = FALSE) +
    geom_pointrange(colour = "red") + theme_classic() + theme(axis.title = element_text()) +
    labs(title = "F1-förarskicklighet  3 år MA",
         subtitle = "Bästa 3-års perioden per respektive förare",
         x = "Förarskicklighet (log odds ratio)",
         y = "Förarskicklighet") +
    geom_vline(xintercept = c(0.5, 1, 1.5, 2), lty = 2, colour = "grey")
  ggsave("img/driver_skill_MA.png", plot = plottning, width = 6, height = 9, bg = "white")
  datan <<- plottning$data
}




MA_plot_driver_race <- function(){
summary_driver <- read_rds("fit/summary_koef_driver_race.rds")
#Hittar intervallet för de bästa åren för respektive förare
summary_driver_MA_intervall <-  summary_driver %>% group_by(Driver) %>% 
  filter(n()> 2) %>% 
  mutate(MA = runMean(est, 3)) %>% 
  slice_max(, order_by = MA, n =1) %>%
  mutate(Year_min = Year - 2,
         Year_max = Year) %>%
  select(Driver, Year_min, Year_max)
#Plottning
summary_MA <- left_join(summary_driver, summary_driver_MA_intervall, by = "Driver") %>% 
  group_by(Driver) %>% 
  filter(Year >= Year_min & Year <= Year_max) %>%
  mutate(medel = mean(est)) %>% arrange(desc(medel)) %>% ungroup() %>% slice(1:45) %>% mutate(sek = rep(1:3, times = 15)) %>%
  ggplot(aes(sek, est, ymin = lower, ymax = upper, color=Driver)) +
  geom_ribbon(aes(fill = Driver), alpha = .2) +
  geom_line(aes(colour = Driver)) +
  geom_point(aes(colour = Driver)) +
  facet_wrap(~Driver) +
  labs(x = "År", y = "Skills (log odds ratio)", title = "Förarskicklighet topp 15 MA 3 år",
       subtitle = "Beräknat för Konstruktörsfördel") + theme_classic() + geom_hline(yintercept = c(0.5, 1, 1.5, 2), lty = 2, colour="grey") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.85)) + 
  scale_x_discrete(limits=1:3)
  ggsave("img/MA_intervall_forare.png", plot = summary_MA, width = 9, height = 9)
}



library(xtable)
xtable(datan[,c(1,2,8,3,4)])




kvantil_funktionen_exempel <- function(){
  samples <- read_rds("fit/samples_koef_driver_race.rds")
  target <- c("fisichella", "alonso", "yamamoto")
  samples2 <- samples %>% 
    filter(Driver %in% target & Year == 2007) %>% 
    select(Driver, skill_yr)
  
  p <- ggplot(samples2, aes(x = skill_yr, y = Driver, fill = factor(stat(quantile)))) +
    stat_density_ridges(
      geom = "density_ridges_gradient",
      calc_ecdf = TRUE,
      quantiles = c(0.025, 0.975),
      alpha = .1) +
    scale_fill_manual(
      name = "Sannolikhet", values = c("Red", "Green", "Red"),
      labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
    theme_classic() + labs(title="Simuleringar av förarkoefficienter",
                           x ="Förarkoefficienter", y = "Förare")
  p
  ggsave("img/exempel_kvantilfunktion_simuleringar.png",p, width = 6, height = 7.5)
  }

