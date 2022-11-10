setwd("~/Documents/F1_sim")

library(tidyverse)
library(ggplot2)

f1_dat <- read_rds("dat/f1_dat.rds")
f1_dat_kval <- read_rds("dat/f1_dat_kval.rds")

# Data processering ----

### Data processering pa RACE ###
# Konvertera till faktorer
f1_dat <- f1_dat %>% mutate(
  status       = as_factor(status),
  constructor  = as_factor(constructor),
  driver       = as_factor(driver),
  weather_type = as_factor(weather_type),
  circuit_type = as_factor(circuit_type)
)

# Exkludera icke-klassificerade
compute_classified <- function(status) {
  out <- rep("not classified", length(status))
  # anyone above the last person still running (finished or +n laps is classified)
  last_classified <- max(which(status == "Finished" | str_starts(status, "\\+")))
  out[1:last_classified] <- "classified"
  out
}

f1_dat_finished <-
  f1_dat %>%
  group_by(year, round) %>%
  mutate(classified = compute_classified(status)) %>%
  filter(classified == "classified") %>%
  mutate(
    position_prop = (n() - position) / (n() - 1),         # how many classified drivers did you beat?
    prop_trans = (position_prop * (n() - 1) + 0.5) / n(), # https://stats.stackexchange.com/a/134297/116878
    grid_prop = (n() - rank(grid)) / (n()-1),
    gridprop_trans = (grid_prop * (n() - 1)+0.5) / n(),
    netto.grid = rank(grid),
    milliseconds2 = ifelse( (max(laps)-laps) > 0, max(milliseconds, na.rm = TRUE) + (max(milliseconds, na.rm = TRUE)/max(laps) * ( (max(laps, na.rm = TRUE)-laps)-1) ), milliseconds),
    milli_intervall =  max(milliseconds2, na.rm = TRUE) - (milliseconds2),         # +1 justering för att inte dela med noll
    prop_milli = milli_intervall / (max(milli_intervall, na.rm = TRUE)),
    propmilli_trans = (prop_milli * (n() - 1) + 0.5) / n()
  ) %>%
  ungroup() %>%
  select(-classified)

f1_dat_finished <- 
  f1_dat_finished %>% drop_na(prop_trans)
which(is.na(f1_dat_finished$prop_trans))

write_rds(f1_dat_finished, "dat/f1_dat_finished.rds")


### Data Processering p? KVAL ###
f1_dat_kval <- f1_dat_kval %>% mutate(
  constructor  = as_factor(constructor),
  driver       = as_factor(driver),
  circuit_type = as_factor(circuit_type)
)

f1_dat_kval_proc <-
  f1_dat_kval %>%
  group_by(year, round) %>%
  mutate(
    kvalposition_prop = (n() - position) / (n() - 1),         # how many classified drivers did you beat?
    prop_trans = (kvalposition_prop * (n() - 1) + 0.5) / n() # https://stats.stackexchange.com/a/134297/116878
  ) %>%
  ungroup()
f1_dat_kval_proc <- f1_dat_kval_proc[f1_dat_kval_proc$kvalposition_prop >= 0, ]
which(is.na(f1_dat_kval_proc$prop_trans))
write_rds(f1_dat_kval_proc, "dat/f1_dat_kval_bearbetad.rds")





# Some EDA ----
# finish position ,    (!!!!!!!) Notera att det finns bortfall i data, speciellt av vinnare
ggplot(f1_dat_finished, aes(x = factor(position))) +
  geom_bar() +
  labs(
    title = "F?rdelning av slutpositioner",
    x = "Slutposition",
    y = "Antal"
  )
ggsave("img/fordelning_slutposition.png", width = 9, height = 6)

# finish position
ggplot(f1_dat_kval[f1_dat_kval$year > 1982, ], aes(x = factor(position))) +
  geom_bar() +
  labs(
    title = "F?rdelning av kvalposition",
    x = "Kvalposition",
    y = "Antal"
  )
ggsave("img/fordelning_kvalposition.png", width = 9, height = 6)


### basic plot 
# (2019-2021)
f1_dat_finished[f1_dat_finished$year > 2018, ] %>%
  filter(driver %in% c("hamilton", "raikkonen", "giovinazzi")) %>%
  ggplot(aes(x = factor(position), fill = driver)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  labs(
    x = "Slutposition",
    y = "Antal",
    title = "Olika f?rarers slutposition (2018-2021)",
    subtitle = "Givet att f?rare inte brutit",
    fill = ""
  ) +
  theme(legend.position = "top") +
  facet_wrap(~year)
ggsave("img/slutposition_2018_2021.png", width = 12, height = 9)

# (1998-2001)
f1_dat_finished[f1_dat_finished$year > 1997 & f1_dat_finished$year < 2001, ] %>%
  filter(driver %in% c("michael_schumacher", "hakkinen", "barrichello")) %>%
  ggplot(aes(x = factor(position), fill = driver)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  labs(
    x = "Slutposition",
    y = "Antal",
    title = "Olika f?rarers slutposition (1998-2001)",
    subtitle = "Givet att f?rare inte brutit",
    fill = ""
  ) +
  theme(legend.position = "top") +
  facet_wrap(~year)
ggsave("img/slutposition_1998_2001.png", width = 12, height = 9)


### Basic plotdensity ?ver andel outperformed
# ?r (2018-2021)
f1_dat_finished[f1_dat_finished$year > 2018, ] %>%
  filter(driver %in% c("hamilton", "raikkonen", "giovinazzi")) %>%
  ggplot(aes(x = prop_trans, fill = driver)) +
  geom_density(alpha = 0.5, bw = 0.1) +
  labs(
    x = "Smoothed proportion of outperformed drivers",
    y = "Densitet",
    title = "Givna f?rares resultat",
    subtitle = "Proportion of finished drivers outperformed",
    fill = ""
  ) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, vjust = 0.85)) +
  facet_wrap(~year)
ggsave("img/densitet_outperformed_2018_2021.png", width = 12, height = 9)

# ?r (1998-2001)
f1_dat_finished[f1_dat_finished$year > 1997 & f1_dat_finished$year < 2001, ] %>%
  filter(driver %in% c("michael_schumacher", "hakkinen", "barrichello")) %>%
  ggplot(aes(x = prop_trans, fill = driver)) +
  geom_density(alpha = 0.5, bw = 0.1) +
  labs(
    x = "Smoothed proportion of outperformed drivers",
    y = "Densitet",
    title = "Givna f?rares resultat",
    subtitle = "Andel ",
    fill = ""
  ) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, vjust = 0.85)) +
  facet_wrap(~year)
ggsave("img/densitet_outperformed_1998_2001.png", width = 12, height = 9)




