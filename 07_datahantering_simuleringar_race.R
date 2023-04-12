setwd("---")
# Datasortering för inferens av race

vilken_modell <- read_rds("fit/loo_results_gridprop.rds")
vilken_modell
fit <- read_rds("fit/fit_circuit.rds") # Vi går vidare med den bästa modellen


# Driver -----
driver_mean <- as_draws_df(fit, variable = "r_driver\\[.+Intercept]", regex = TRUE) %>% select(-.chain, -.iteration)
driver_form <- as_draws_df(fit, variable = "r_driver:year\\[.+Intercept]", regex = TRUE) %>% select(-.chain,-.iteration)

driver_mean_long <-
  driver_mean  %>%
  pivot_longer(-.draw, names_to = "Driver", values_to = "Skill",
               names_pattern = "\\[(\\w+),") %>%
  mutate(Driver = as_factor(Driver))

driver_form_long <-
  driver_form %>%
  pivot_longer(-.draw, names_to = c("Driver", "Year"), values_to = "Form",
               names_pattern = "\\[(\\w+)_([0-9]{4}),") %>%
  mutate(Driver = as_factor(Driver), Year = as.integer(Year))

driver_samples <-
  left_join(driver_form_long, driver_mean_long, by = c("Driver", ".draw")) %>%
  mutate(skill_yr = Form + Skill)

driver_skill_summary <-
  driver_samples %>%
  group_by(Driver, Year) %>%
  summarise(
    est = mean(skill_yr),
    lower = quantile(skill_yr, 0.055),
    upper = quantile(skill_yr, 0.945),
    var = var(skill_yr),
    n = n()
  )
write_rds(driver_skill_summary, "fit/summary_koef_driver_race.rds")
write_rds(driver_samples, "fit/samples_koef_driver_race.rds")



# Constructor -----
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
    var = var(advantage_yr),
    n = n()
  )
write_rds(constructor_advantage_summary, "fit/summary_koef_team_race.rds")
write_rds(constructor_samples, "fit/samples_koef_team_race.rds")
