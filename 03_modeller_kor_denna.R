setwd("C:/Users/claes/OneDrive/Universitet/Statistik Forts�ttningskurs/STAH11 Kandidatuppsats/Test_egen_kval_och_grid")

library(tidyverse)
library(brms)
library(cmdstanr)
library(readr)

# inläsning data
f1_dat_finished  <- read_rds("dat/f1_dat_finished.rds")
f1_dat_kval <- read_rds("dat/f1_dat_kval_bearbetad.rds")

##### Modeller för racet #####
# basic model
fit_basic <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 | driver:year) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_basic)
write_rds(fit_basic, "fit/fit_basic.rds")

# väder model
fit_vader <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 | driver:year) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_vader)
write_rds(fit_vader, "fit/fit_vader.rds")


# bantyp
fit_circuit <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 | driver:year) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_circuit)
write_rds(fit_circuit, "fit/fit_circuit.rds")



# väder + bantyp
fit_weather_circuit <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 | driver:year) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_weather_circuit)
write_rds(fit_weather_circuit, "fit/fit_weather_circuit.rds")



#väder + bantyp + gridposition
fit_weather_circuit_grid <- brm(
  formula = prop_trans ~ 0 + (1 + gridprop_trans | constructor ) + (1 + weather_type | driver) + (1 | driver:year) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_weather_circuit_grid)
write_rds(fit_weather_circuit_grid, "fit/fit_weather_circuit_grid.rds")



##### Modeller för kvalet #####
# basic model
fit_basic_kval <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 | driver:year) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_kval,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_basic_kval)
write_rds(fit_basic_kval, "fit/fit_basic_kval.rds")

# väder model
fit_vader_kval <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 | driver:year) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_kval,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_vader_kval)
write_rds(fit_vader_kval, "fit/fit_vader_kval.rds")

# bantyp
fit_circuit_kval <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 | driver:year) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_kval,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_circuit_kval)
write_rds(fit_circuit_kval, "fit/fit_circuit_kval.rds")

# väder + bantyp
fit_weather_circuit_kval <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 | driver:year) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_kval,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_weather_circuit_kval)
write_rds(fit_weather_circuit_kval, "fit/fit_weather_circuit_kval.rds")