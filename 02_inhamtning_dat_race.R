setwd("---")

library(tidyverse)
library(lubridate)
library(jsonlite)
library(glue)
library(rvest)

tab_races        <- read_csv("dat/f1db_csv/races.csv")
tab_circuits     <- read_csv("dat/f1db_csv/circuits.csv")
# Results information
tab_results      <- read_csv("dat/f1db_csv/results.csv")
tab_drivers      <- read_csv("dat/f1db_csv/drivers.csv")
tab_constructors <- read_csv("dat/f1db_csv/constructors.csv")
tab_status       <- read_csv("dat/f1db_csv/status.csv")
tab_qualifying   <- read_csv("dat/f1db_csv/qualifying.csv") 
tab_seasons      <- read_csv("dat/f1db_csv/seasons.csv")
race_info        <- read_rds("dat/race_info.rds")



## Select columns ----
race_dat <- race_info %>%
  select(raceId, year, round, circuitRef, country, weather_type, circuit_type)

# Result information ----
results_dat <-
  tab_results %>%
  left_join(tab_drivers, by = "driverId") %>%
  left_join(tab_constructors, by = "constructorId") %>%
  left_join(tab_status, by = "statusId") %>%
  select(raceId, positionText, positionOrder, grid, fastestLapTime, driverRef, constructorRef, status, milliseconds, laps)

# Joining & cleaning ----
f1_dat <-
  race_dat %>%
  left_join(results_dat, by = "raceId") %>%
  select(-raceId) %>%
  rename(circuit = circuitRef, driver = driverRef, constructor = constructorRef,
         position = positionOrder, fastest_lab = fastestLapTime) %>%
  select(driver, constructor, year, round, circuit, position, grid, weather_type, circuit_type, milliseconds, laps, status) %>%
  mutate(year = as.integer(year), round = as.integer(round), position = as.integer(position), grid = as.integer(grid), laps = as.integer(laps), milliseconds = as.numeric(milliseconds))


# Dubbelkollar nedan så att det ej finns NA
which(is.na(f1_dat$circuit_type))
which(is.na(f1_dat$weather_type))
which(is.na(f1_dat$driver))
which(is.na(f1_dat$constructor))
which(is.na(f1_dat$year))
which(is.na(f1_dat$round))
which(is.na(f1_dat$circuit))
which(is.na(f1_dat$position))
which(is.na(f1_dat$circuit_type))
which(is.na(f1_dat$milliseconds))

write_rds(f1_dat, "dat/f1_dat.rds")
