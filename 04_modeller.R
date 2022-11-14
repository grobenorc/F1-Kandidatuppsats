setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Final_modell")

library(tidyverse)
library(brms)
library(cmdstanr)
library(readr)

# inläsning data
f1_dat_finished  <- read_rds("dat/f1_dat_finished.rds")
f1_dat_kval <- read_rds("dat/f1_dat_kval_bearbetad.rds")

##### Modeller för racet #####

# Vi tittar först på modeller där enbart har random effects, alltså förare och team
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
fit_vader_circuit <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 | driver:year) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_vader_circuit)
write_rds(fit_vader_circuit, "fit/fit_vader_circuit.rds")



# Vi tittar sedan på en mixed modell där vi har startposition som en fixed effect, samtidigt som förare och team är random effects
# basic model
fit_basic_gridprop <- brm(
  formula = prop_trans ~ 0 + gridprop_trans + (1 | driver) + (1 | driver:year) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_basic_gridprop)
write_rds(fit_basic_gridprop, "fit/fit_basic_gridprop.rds")

# väder model
fit_vader_gridprop <- brm(
  formula = prop_trans ~ 0 + gridprop_trans + (1 + weather_type | driver) + (1 | driver:year) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_vader_gridprop)
write_rds(fit_vader_gridprop, "fit/fit_vader_gridprop.rds")


# bantyp
fit_circuit_gridprop <- brm(
  formula = prop_trans ~ 0 + gridprop_trans + (1 | driver) + (1 | driver:year) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_circuit_gridprop)
write_rds(fit_circuit_gridprop, "fit/fit_circuit_gridprop.rds")



# väder + bantyp
fit_vader_circuit_gridprop <- brm(
  formula = prop_trans ~ 0 + gridprop_trans + (1 + weather_type | driver) + (1 | driver:year) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_vader_circuit_gridprop)
write_rds(fit_vader_circuit_gridprop, "fit/fit_vader_circuit_gridprop.rds")







##### Modeller för kvalet -----
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

