
setwd("---")

library(tidyverse)
library(brms)
library(patchwork)
library(glue)

drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

driver_samples      <- read_rds("fit/samples_koef_driver_race.rds")
driver_summary      <- read_rds("fit/summary_koef_driver_race.rds")
constrcutor_samples <- read_rds("fit/samples_koef_team_race.rds")
constructor_summary <- read_rds("fit/summary_koef_team_race.rds")


# Alla år, Posterior samples
posterior_driver   <- driver_samples
posterior_driver   <- drop_na(posterior_driver)
mc_areas_driver    <- mcmc_areas(posterior_driver,
                                 pars = c("Form", "Skill", "skill_yr"),
                                 prob = 0.95) + ggtitle("Posterior fördelningar Driver",
                                                        "med median och 95% intervall")


posterior_team   <- constructor_samples
posterior_team   <- drop_na(posterior_team)
mc_areas_driver    <- mcmc_areas(posterior_team,
                                 pars = c("Form", "Skill", "skill_yr"),
                                 prob = 0.95) + ggtitle("Posterior fördelningar Team",
                                                        "med median och 95% intervall")






# Plottning av 
plt_skill_trajectory <-
  driver_skill_summary %>%
  ungroup() %>%
  filter(Driver %in% drivers_focus) %>%
  mutate(Driver = fct_reorder(Driver, -est)) %>%
  ggplot(aes(x = Year, y = est, ymin = lower, ymax = upper)) +
  geom_ribbon(aes(fill = Driver), alpha = .2) +
  geom_line(aes(colour = Driver)) +
  geom_point(aes(colour = Driver)) +
  facet_wrap(~Driver) +
  labs(x = "Season", y = "Skill (log odds ratio)", title = "F1 driver skill trajectories",
       subtitle = "Hybrid-era (2014-2021) driver skill,\naccounting for yearly constructor advantage.")


ggsave("img/plt_skill_trajectories.png", plot = plt_skill_trajectory, width = 12, height = 9, bg = "white")

plt_driver_skill_2021 <-
  driver_skill_summary %>%
  ungroup() %>%
  filter(Year == 2021) %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") +
  labs(title = "2021 F1 driver skill",
       subtitle = "Accounting for yearly constructor advantage.",
       x = "Skill (log odds ratio)",
       y = "Driver")

ggsave("img/plt_skill_2021.png", plot = plt_driver_skill_2021, width = 9, height = 9, bg = "white")

plt_driver_skill_2000 <-
  driver_skill_summary %>%
  ungroup() %>%
  filter(Year == 2000) %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") +
  labs(title = "2000 F1 driver skill",
       subtitle = "Accounting for yearly constructor advantage.",
       x = "Skill (log odds ratio)",
       y = "Driver")

ggsave("img/plt_skill_2000.png", plot = plt_driver_skill_2000, width = 9, height = 9, bg = "white")

plt_driver_skill_1990 <-
  driver_skill_summary %>%
  ungroup() %>%
  filter(Year == 1990) %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") +
  labs(title = "1990 F1 driver skill",
       subtitle = "Accounting for yearly constructor advantage.",
       x = "Skill (log odds ratio)",
       y = "Driver")

ggsave("img/plt_skill_1990.png", plot = plt_driver_skill_1990, width = 9, height = 9, bg = "white")


plt_driver_skill_1956 <-
  driver_skill_summary %>%
  ungroup() %>%
  filter(Year == 1956) %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") +
  labs(title = "1956 F1 driver skill",
       subtitle = "Accounting for yearly constructor advantage.",
       x = "Skill (log odds ratio)",
       y = "Driver")

ggsave("img/plt_skill_1956.png", plot = plt_driver_skill_1956, width = 9, height = 9, bg = "white")


# Inference about constructor advantage ----
constructors_focus <- c("ferrari", "benetton", "mclaren", "mercedes", "red_bull", "williams")

constructor_mean <- as_draws_df(fit, variable = "r_constructor\\[.+Intercept]", regex = TRUE) %>% select(-.chain, -.iteration)
constructor_form <- as_draws_df(fit, variable = "r_constructor:year\\[.+Intercept]", regex = TRUE) %>% select(-.chain,-.iteration)


constructor_mean_long <-
  constructor_mean  %>%
  pivot_longer(-.draw, names_to = "Constructor", values_to = "Advantage",
               names_pattern = "\\[(\\w+),") %>%
  mutate(Constructor = as_factor(Constructor))

constructor_form_long <-
  constructor_form %>%
  pivot_longer(-.draw, names_to = c("Constructor", "Year"), values_to = "Form",
               names_pattern = "\\[(\\w+)_([0-9]{4}),") %>%
  mutate(Constructor = as_factor(Constructor), Year = as.integer(Year))

constructor_samples <-
  left_join(constructor_form_long, constructor_mean_long, by = c("Constructor", ".draw")) %>%
  mutate(advantage_yr = Form + Advantage)

constructor_advantage_summary <-
  constructor_samples %>%
  group_by(Constructor, Year) %>%
  summarise(
    est = mean(advantage_yr),
    lower = quantile(advantage_yr, 0.055),
    upper = quantile(advantage_yr, 0.945),
  )
write_rds(constructor_advantage_summary, "fit/simulated_koef_team_race.rds")





plt_advantage_trajectory <-
  constructor_advantage_summary %>%
  ungroup() %>%
  filter(Constructor %in% constructors_focus) %>%
  mutate(Constructor = fct_relevel(Constructor, "ferrari", "mercedes", "red_bull", "mclaren", "benetton", "williams")) %>%
  ggplot(aes(x = Year, y = est, ymin = lower, ymax = upper)) +
  geom_ribbon(aes(fill = Constructor), alpha = .2) +
  geom_line(aes(colour = Constructor)) +
  geom_point(aes(colour = Constructor)) +
  facet_wrap(~Constructor) +
  labs(x = "Season", y = "Advantage (log odds ratio)", title = "F1 constructor advantage trajectories",
       subtitle = "Constructor advantage,\naccounting for yearly driver skill.")

ggsave("img/plt_advantage_trajectory.png", plot = plt_advantage_trajectory, width = 12, height = 9, bg = "white")


constructors_2021 <- c("alfa", "alphatauri", "alpine", "ferrari", "haas", "mclaren",
                       "mercedes", "aston_martin", "red_bull", "williams")

constructor_mean_summary <-
  constructor_mean_long %>%
  group_by(Constructor) %>%
  summarise(
    est = mean(Advantage),
    lower = quantile(Advantage, 0.055),
    upper = quantile(Advantage, 0.945),
  )


plt_advantage_avg <-
  constructor_mean_summary %>%
  ungroup() %>%
  filter(Constructor %in% constructors_2021) %>%
  mutate(Constructor = fct_reorder(Constructor, est)) %>%
  ggplot(aes(y = Constructor, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = 2) +
   labs(title = "Average F1 constructor advantage",
       subtitle = "Accounting for yearly driver skill and constructor form.",
       x = "Advantage (log odds ratio)",
       y = "Constructor")

ggsave("img/plt_advantage_avg.png", plot = plt_advantage_avg, width = 9, height = 6, bg = "white")

constructor_mean_summary_top_10 <- 
  constructor_mean_summary %>% 
  slice_max(est, n = 10) %>%
  mutate(Constructor = fct_reorder(Constructor, est)) %>%
  ggplot(aes(y = Constructor, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = 2) +
  labs(title = "Genomsnittlig F1 constructor advantage",
       subtitle = "Accounting for yearly driver skill and constructor form.",
       x = "Advantage (log odds ratio)",
       y = "Constructor")
ggsave("img/plt_advantage_avg_10.png", plot = plt_advantage_avg, width = 9, height = 6, bg = "white")


# Driver versus constructor contributions ----
# random effects standard deviation summary
sfit <- summary(fit, prob = 0.89)
ranef_summary <- rbind(
  "constructor" = sfit$random$constructor,
  "constructor form" = sfit$random$`constructor:year`,
  "driver" = sfit$random$driver,
  "driver form" = sfit$random$`driver:year`
)[1:6, 1:4]
xtable::xtable(ranef_summary)

# how much of variance is due to car?
colSums(ranef_summary[1:2,]^2)/colSums(ranef_summary^2)

# and how much due to the driver?
colSums(ranef_summary[3:4,]^2)/colSums(ranef_summary^2)



# Overall performance in 2021 ----
grid_2021 <-
  f1_dat %>%
  filter(year == 2021, driver != "kubica") %>% # kubica only did one race
  select(driver, constructor, year) %>%
  distinct() %>%
  arrange(constructor)

pp_2021 <- posterior_predict(fit, grid_2021)
pp_2021_summary <-
  pp_2021 %>%
  as_tibble(.name_repair = "minimal") %>%
  set_names(grid_2021$driver) %>%
  pivot_longer(everything(), names_to = "driver") %>%
  group_by(driver) %>%
  summarise(est = mean(value), lower = quantile(value, 0.045), upper = quantile(value, 0.955)) %>%
  left_join(grid_2021) %>%
  select(driver, constructor, performance = est, lower, upper) %>%
  arrange(-performance)

xtable::xtable(pp_2021_summary, digits = 3)

ggsave("img/plt_performance_2021.png", width = 6, height = 9, bg = "white")
