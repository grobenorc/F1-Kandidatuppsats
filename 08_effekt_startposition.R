
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Testmapp")
library(tidyverse)
library(brms)
library(cmdstanr)
library(readr)
library(sjPlot)



f1_dat_finished  <- read_rds("dat/f1_dat_finished_ink_grid.rds")
constructors <- read_csv("dat/f1db_csv/constructors.csv")
drivers <- read_csv("dat/f1db_csv/drivers.csv")
f1_dat_finished_filter <- f1_dat_finished[f1_dat_finished$year >= 2017, ] #Datamaterial som enbart tar ut 2017-
fit_prov_3 <- read_rds("fit/fit_prov_3.rds")
fit_prov_3 <- brm(prop_trans ~ 0 + (1 | driver) + (1 | driver:year) + (1 | constructor) + (1 | constructor:year),
                  family = Beta(),
                  data = f1_dat_finished_filter,
                  backend = "cmdstanr",
                  chains = 3,
                  iter = 1500)
write_rds(fit_prov_3, "fit/fit_prov_3.rds")



#Hämtning av koefficienter
f1_dat_finished  <- read_rds("dat/f1_dat_finished_ink_grid.rds")
constructors <- read_csv("dat/f1db_csv/constructors.csv")
drivers <- read_csv("dat/f1db_csv/drivers.csv")
f1_dat_finished_filter <- f1_dat_finished[f1_dat_finished$year >= 2017, ] #Datamaterial som enbart tar ut 2017-
fit_prov_3 <- read_rds("fit/fit_prov_3.rds")




## Hämtning och sortering av team-koefficienter ##
fit_prov_team_coef <- data.frame(ranef(fit_prov_3)[[2]])
fit_prov_team_coef$year <- as.factor(sapply(str_extract_all(rownames(fit_prov_team_coef), "\\d+"),tail,1))
fit_prov_team_coef$constructorRef <- as.factor(substr(rownames(fit_prov_team_coef),1,nchar(rownames(fit_prov_team_coef))-5))
fit_prov_team_coef <- left_join(fit_prov_team_coef, constructors[,c(1,2,3)], by="constructorRef")
fit_prov_team_coef$constructor <- fit_prov_team_coef$constructorRef


## Hämtning och sortering av förar-koefficienter ##
fit_prov_driver_coef <- data.frame(ranef(fit_prov_3)[[4]])
fit_prov_driver_coef$year <- as.factor(sapply(str_extract_all(rownames(fit_prov_driver_coef), "\\d+"),tail,1))
fit_prov_driver_coef$driverRef <- as.factor(substr(rownames(fit_prov_driver_coef),1,nchar(rownames(fit_prov_driver_coef))-5))
fit_prov_driver_coef <- left_join(fit_prov_driver_coef, drivers[,c(1,2,4)], by="driverRef")
fit_prov_driver_coef$driver <- fit_prov_driver_coef$driverRef
fit_prov_driver_coef$driver.Estimate.Intercept <- fit_prov_driver_coef$Estimate.Intercept

#Merge av estimerade koefficienter
# Result information ----
f1_dat_finished_filter$year <- as.factor(f1_dat_finished_filter$year)
fit_koef <- left_join(f1_dat_finished_filter, fit_prov_team_coef[, c(1, 5, 9)], by=c("year", "constructor"))
fit_koef <- left_join(fit_koef, fit_prov_driver_coef[, c(10, 5, 9)], by=c("year", "driver"))







fit_prov_4 <- brm(prop_trans ~ 0 + (1 | gridprop_trans) + (1 | Estimate.Intercept) + (1 | driver.Estimate.Intercept),
                  family = Beta(),
                  data = fit_koef,
                  backend = "cmdstanr",
                  chains = 3,
                  iter = 2500)
write_rds(fit_prov_4, "fit/fit_prov_4.rds")

sfit <- summary(fit_prov_4, prob = 0.89)
ranef_summary <- rbind(
  "Constructor" = sfit$random$Estimate.Intercept,
  "Driver" = sfit$random$driver.Estimate.Intercept,
  "Grid" = sfit$random$gridprop_trans
)[1:3, 1:2]
xtable::xtable(ranef_summary)


# how much of variance is due to Constructor&driver-advantage (Estimate.intercept)?
colSums(ranef_summary[1:2,]^2)/colSums(ranef_summary^2)
# and how much due to the grid?
colSums(ranef_summary[3,]^2)/colSums(ranef_summary^2)



fit_prov_4 <- brm(prop_trans ~ 0 + (1 | gridprop_trans) + (1 | gridprop_trans:year) + (1 | Estimate.Intercept) + (1 | driver.Estimate.Intercept),
                  family = Beta(),
                  data = fit_koef,
                  backend = "cmdstanr",
                  chains = 3,
                  iter = 2500)



