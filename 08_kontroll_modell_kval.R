
setwd("---")
library(tidyverse)
library(brms)
library(tidyverse)
library(brms)
library(ggthemes)


f1_dat <- read_rds("dat/f1_dat_kval_bearbetad.rds")
fit <- read_rds("fit/fit_circuit_kval.rds") # Här tar vi enbart den bästa modellen
driver_samples      <- read_rds("fit/samples_koef_driver_kval.rds")
constrcutor_samples <- read_rds("fit/samples_koef_team_kval.rds")


# Alla år, Posterior samples
posterior_driver   <- driver_samples
posterior_driver   <- drop_na(posterior_driver)
mc_areas_driver    <- mcmc_areas(posterior_driver,
                                 pars = c("Form", "Skill", "skill_yr"),
                                 prob = 0.95) + ggtitle("Posterior fördelningar Driver",
                                                        "med median och 95% intervall") + 
                                                theme_fivethirtyeight() + theme(axis.title = element_text())
ggsave("img/posterior_samples_driver_kval.png", width = 7, height = 10, bg = "white")


posterior_team          <- constructor_samples
posterior_team          <- drop_na(posterior_team)
mc_areas_constrcutor    <- mcmc_areas(posterior_team,
                                 pars = c("Form", "Skill", "skill_yr"),
                                 prob = 0.95) + ggtitle("Posterior fördelningar Team",
                                                        "med median och 95% intervall") + 
                                                theme_fivethirtyeight() + theme(axis.title = element_text())
ggsave("img/posterior_samples_constructor_kval.png", width = 7, height = 10, bg = "white")


# MCMC mixing ----
mcmc_plot(fit, type = "trace") +
  facet_wrap(~parameter, nrow = 6, scales = "free") + ggtitle("Chains Kval") + theme_fivethirtyeight() + theme(axis.title = element_text())
ggsave("img/chains_kval.png", width = 7, height = 10, bg = "white")

rhats <- rhat(fit)
any(rhats[!is.nan(rhats)] > 1.01)


# 1955 posterior predictive check ----
# create drivers & constructors in 1955
pred_tab_1955 <-
  f1_dat %>%
  filter(year == 1955) %>%
  select(driver, constructor, year) %>%
  distinct() %>%
  mutate(weather_type = "dry", circuit_type = "permanent")

# predict proportion of outperformed drivers
pp_tab_1955 <- posterior_predict(fit, pred_tab_1955)



## Proportion plot ----
# yrep
pred_tab_long_1955 <-
  pred_tab_1955 %>%
  bind_cols(t(pp_tab_1955) %>% as_tibble(.name_repair = "minimal") %>% set_names(1:7000)) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "prop_trans"
  ) %>%
  mutate(origin = "simulated")

# y
true_tab_long_1955 <-
  f1_dat %>%
  filter(year == 1955) %>%
  select(driver, constructor, year, prop_trans) %>%
  mutate(origin = "observed")

ordered_levels_1955 <-
  true_tab_long_1955 %>%
  group_by(driver) %>%
  summarise(prop = mean(prop_trans)) %>%
  arrange(-prop) %>%
  pull(driver) %>%
  as.character()



bind_rows(pred_tab_long_1955, true_tab_long_1955) %>%
  ggplot(aes(x = prop_trans, fill = origin)) +
  geom_density(alpha = 0.8, bw = .07) +
  facet_wrap(~factor(driver, levels = ordered_levels_1955), scales = "free") +
  xlim(0, 1) +
  theme(legend.position = "top") + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(
    title = "1955 säsong posterior prediktionskontroll",
    x = "Andel slagna förare",
    y = "",
    fill = ""
  )
ggsave("img/pp_check_prop_1955_kval.png", width = 15, height = 12, bg = "white")



# 1986 posterior predictive check ----
# create drivers & constructors in 1986
pred_tab_1986 <-
  f1_dat %>%
  filter(year == 1986) %>%
  select(driver, constructor, year) %>%
  distinct() %>%
  mutate(weather_type = "dry", circuit_type = "permanent")

# predict proportion of outperformed drivers
pp_tab_1986 <- posterior_predict(fit, pred_tab_1986)



## Proportion plot ----
# yrep
pred_tab_long_1986 <-
  pred_tab_1986 %>%
  bind_cols(t(pp_tab_1986) %>% as_tibble(.name_repair = "minimal") %>% set_names(1:7000)) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "prop_trans"
  ) %>%
  mutate(origin = "simulated")

# y
true_tab_long_1986 <-
  f1_dat %>%
  filter(year == 1986) %>%
  select(driver, constructor, year, prop_trans) %>%
  mutate(origin = "observed")

ordered_levels_1986 <-
  true_tab_long_1986 %>%
  group_by(driver) %>%
  summarise(prop = mean(prop_trans)) %>%
  arrange(-prop) %>%
  pull(driver) %>%
  as.character()



bind_rows(pred_tab_long_1986, true_tab_long_1986) %>%
  ggplot(aes(x = prop_trans, fill = origin)) +
  geom_density(alpha = 0.8, bw = .07) +
  facet_wrap(~factor(driver, levels = ordered_levels_1986), scales = "free") +
  xlim(0, 1) +
  theme(legend.position = "top") + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(
    title = "1986 säsong posterior prediktionskontroll",
    x = "Andel slagna förare",
    y = "",
    fill = ""
  )
ggsave("img/pp_check_prop_1986_kval.png", width = 15, height = 12, bg = "white")



# 2007 posterior predictive check ----
# create drivers & constructors in 2007
pred_tab_2007 <-
  f1_dat %>%
  filter(year == 2007) %>%
  select(driver, constructor, year) %>%
  distinct() %>%
  mutate(weather_type = "dry", circuit_type = "permanent")

# predict proportion of outperformed drivers
pp_tab_2007 <- posterior_predict(fit, pred_tab_2007)



## Proportion plot ----
# yrep
pred_tab_long_2007 <-
  pred_tab_2007 %>%
  bind_cols(t(pp_tab_2007) %>% as_tibble(.name_repair = "minimal") %>% set_names(1:7000)) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "prop_trans"
  ) %>%
  mutate(origin = "simulated")

# y
true_tab_long_2007 <-
  f1_dat %>%
  filter(year == 2007) %>%
  select(driver, constructor, year, prop_trans) %>%
  mutate(origin = "observed")

ordered_levels_2007 <-
  true_tab_long_2007 %>%
  group_by(driver) %>%
  summarise(prop = mean(prop_trans)) %>%
  arrange(-prop) %>%
  pull(driver) %>%
  as.character()



bind_rows(pred_tab_long_2007, true_tab_long_2007) %>%
  ggplot(aes(x = prop_trans, fill = origin)) +
  geom_density(alpha = 0.8, bw = .07) +
  facet_wrap(~factor(driver, levels = ordered_levels_2007), scales = "free") +
  xlim(0, 1) +
  theme(legend.position = "top") + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(
    title = "2007 säsong posterior prediktionskontroll",
    x = "Andel slagna förare",
    y = "",
    fill = ""
  )
ggsave("img/pp_check_prop_2007_kval.png", width = 15, height = 12, bg = "white")


# 2007 posterior predictive check ----
# create drivers & constructors in 2007
pred_tab_2007 <-
  f1_dat %>%
  filter(year == 2007) %>%
  select(driver, constructor, year) %>%
  distinct() %>%
  mutate(weather_type = "dry", circuit_type = "permanent")

# predict proportion of outperformed drivers
pp_tab_2007 <- posterior_predict(fit, pred_tab_2007)



## Proportion plot ----
# yrep
pred_tab_long_2021 <-
  pred_tab_2021 %>%
  bind_cols(t(pp_tab_2021) %>% as_tibble(.name_repair = "minimal") %>% set_names(1:7000)) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "prop_trans"
  ) %>%
  mutate(origin = "simulated")

# y
true_tab_long_2021 <-
  f1_dat %>%
  filter(year == 2021) %>%
  select(driver, constructor, year, prop_trans) %>%
  mutate(origin = "observed")

ordered_levels_2021 <-
  true_tab_long_2021 %>%
  group_by(driver) %>%
  summarise(prop = mean(prop_trans)) %>%
  arrange(-prop) %>%
  pull(driver) %>%
  as.character()



bind_rows(pred_tab_long_2021, true_tab_long_2021) %>%
  ggplot(aes(x = prop_trans, fill = origin)) +
  geom_density(alpha = 0.8, bw = .07) +
  facet_wrap(~factor(driver, levels = ordered_levels_2021), scales = "free") +
  xlim(0, 1) +
  theme(legend.position = "top") + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(
    title = "2021 säsong posterior prediktionskontroll",
    x = "Andel slagna förare",
    y = "",
    fill = ""
  )
ggsave("img/pp_check_prop_2021_kval.png", width = 15, height = 12, bg = "white")









## PPC on rank scale ----

# predict proportion of outperformed drivers
# finish position distribution to weigh observations by
# 2021
n_races <- length(unique(paste0(f1_dat$year, "_", f1_dat$round)))
position_table <- table(f1_dat$position) / n_races
reweigh <- function(rank_sample) {
  # function to resample the ranks based on their value
  sample(rank_sample, prob = position_table[rank_sample], replace = TRUE)
}

pp_ranks_2021 <-
  apply(pp_tab_2021, 1, function(x) rank(-x)) %>%
  apply(1, reweigh) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(across(.fns = as.integer)) %>%
  set_names(1:7000)



# yrep
pred_rank_long_2021 <-
  pred_tab_2021 %>%
  bind_cols(pp_ranks_2021) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "position"
  ) %>%
  mutate(origin = "simulated")

# y
true_rank_long_2021 <-
  f1_dat %>%
  filter(year == 2021) %>%
  select(driver, constructor, year, position) %>%
  mutate(origin = "observed")

driver_length_2021 <- length(unique(true_rank_long_2021$driver))

bind_rows(pred_rank_long_2021, true_rank_long_2021) %>%
  filter(is.na(sample) | sample %in% sample(7000, driver_length_2021)) %>%
  ggplot(aes(x = factor(position), fill = origin)) +
  geom_bar(position = position_dodge(preserve = "single")) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  facet_wrap(~factor(driver, levels = ordered_levels_2021)) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(1,driver_length_2021,5)) + 
  theme(legend.position = "top") +
  labs(
    title = "2021 säsong posterior prediktionskontroll",
    x = "Position",
    y = "Antal",
    fill = ""
  )
ggsave("img/pp_check_rank_2021_kval.png", width = 15, height = 12, bg = "white")




# finish position distribution to weigh observations by
# 2007
n_races <- length(unique(paste0(f1_dat$year, "_", f1_dat$round)))
position_table <- table(f1_dat$position) / n_races
reweigh <- function(rank_sample) {
  # function to resample the ranks based on their value
  sample(rank_sample, prob = position_table[rank_sample], replace = TRUE)
}

pp_ranks_2007 <-
  apply(pp_tab_2007, 1, function(x) rank(-x)) %>%
  apply(1, reweigh) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(across(.fns = as.integer)) %>%
  set_names(1:7000)



# yrep
pred_rank_long_2007 <-
  pred_tab_2007 %>%
  bind_cols(pp_ranks_2007) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "position"
  ) %>%
  mutate(origin = "simulated")

# y
true_rank_long_2007 <-
  f1_dat %>%
  filter(year == 2007) %>%
  select(driver, constructor, year, position) %>%
  mutate(origin = "observed")

driver_length_2007 <- length(unique(true_rank_long_2007$driver))

bind_rows(pred_rank_long_2007, true_rank_long_2007) %>%
  filter(is.na(sample) | sample %in% sample(7000, driver_length_2007)) %>%
  ggplot(aes(x = factor(position), fill = origin)) +
  geom_bar(position = position_dodge(preserve = "single")) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  facet_wrap(~factor(driver, levels = ordered_levels_2007)) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(1,driver_length_2007,5)) + 
  theme(legend.position = "top") +
  labs(
    title = "2007 säsong posterior prediktionskontroll",
    x = "Position",
    y = "Antal",
    fill = ""
  )
ggsave("img/pp_check_rank_2007_kval.png", width = 15, height = 12, bg = "white")



# finish position distribution to weigh observations by
# 1986
n_races <- length(unique(paste0(f1_dat$year, "_", f1_dat$round)))
position_table <- table(f1_dat$position) / n_races
reweigh <- function(rank_sample) {
  # function to resample the ranks based on their value
  sample(rank_sample, prob = position_table[rank_sample], replace = TRUE)
}

pp_ranks_1986 <-
  apply(pp_tab_1986, 1, function(x) rank(-x)) %>%
  apply(1, reweigh) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(across(.fns = as.integer)) %>%
  set_names(1:7000)



# yrep
pred_rank_long_1986 <-
  pred_tab_1986 %>%
  bind_cols(pp_ranks_1986) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "position"
  ) %>%
  mutate(origin = "simulated")

# y
true_rank_long_1986 <-
  f1_dat %>%
  filter(year == 1986) %>%
  select(driver, constructor, year, position) %>%
  mutate(origin = "observed")

driver_length_1986 <- length(unique(true_rank_long_1986$driver))

bind_rows(pred_rank_long_1986, true_rank_long_1986) %>%
  filter(is.na(sample) | sample %in% sample(7000, driver_length_1986)) %>%
  ggplot(aes(x = factor(position), fill = origin)) +
  geom_bar(position = position_dodge(preserve = "single")) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  facet_wrap(~factor(driver, levels = ordered_levels_1986)) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(1,driver_length_1986,5)) + 
  theme(legend.position = "top") +
  labs(
    title = "1986 säsong posterior prediktionskontroll",
    x = "Position",
    y = "Antal",
    fill = ""
  )
ggsave("img/pp_check_rank_1986_kval.png", width = 15, height = 12, bg = "white")




