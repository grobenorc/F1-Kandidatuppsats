setwd("~/Documents/F1_sim")


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

#Datumintervall for data
all_years <- interval(ymd("1950-01-01"), ymd("2021-12-31"))

# Race information ----
## function to info from wikipedia ----
wiki_info <- function(url) {
  cat(glue("Downloading info from {url}..."),"\n")
  
  infobox <- tryCatch({
    read_html(url) %>%
      html_element(".infobox") %>%
      html_table(trim = TRUE) %>%
      .[,1:2] %>%
      set_names(c("Property", "Value"))
  }, error = function(e) NA)
  
  if (length(infobox) == 1 && is.na(infobox)) return(NA)
  
  weather_txt <- tryCatch({
    res <-
      infobox %>%
      filter(Property == "Weather") %>%
      pull(Value)
    stopifnot(length(res) > 0)
    res
  }, error = function(e) NA)
  
  circuit_txt <- tryCatch({
    res <-
      infobox %>%
      filter(Property == "Course") %>%
      pull(Value)
    stopifnot(length(res) > 0)
    res
  }, error = function(e) NA)
  
  return(list(circuit_txt = circuit_txt, weather_txt = weather_txt))
}

race_info <-
  tab_races %>%
  filter(date %within% all_years) %>%
  left_join(tab_circuits, by = "circuitId", suffix = c("", "_circuit")) %>%
  mutate(enrichment = lapply(url, wiki_info)) %>%
  unnest_wider(enrichment) %>%
  mutate(
    weather_type = ifelse(str_detect(tolower(weather_txt), "(wet|rain)"), "wet", "dry"),
    circuit_type = ifelse(str_detect(tolower(circuit_txt), "street"), "street", "permanent"),
  )

## Manually add missing weather info ----
which(is.na(race_info$weather_type))
weather_missing_idx <- which(is.na(race_info$weather_type))
race_info[455, "weather_type"] <- "wet" # San marino 1984
race_info[595, "weather_type"] <- "wet" # Monaco 1974
race_info[773, "weather_type"] <- "wet" # Portugese 1958
weather_missing_idx <- which(is.na(race_info$weather_type))
race_info[weather_missing_idx, "weather_type"] <- "dry"
which(is.na(race_info$weather_type))

##### Denna nedan har blivit fel med vi tar ut de våta enligt ovan #####
#race_info[weather_missing_idx, "weather_type"] <- c(
#  "dry", # European Grand Prix 2006
#  "dry", # British Grand Prix 1995
#  "dry", # Spanish Grand Prix 1993
#  "dry", # Hungarian Grand Prix 1993
#  "dry", # French Grand Prix 1991
#  "dry", # Portuguese Grand Prix 1990
#  "dry", # German Grand Prix 1987
#  "dry", # Hungarian Grand Prix 1986
#  "dry", # San Marino Grand Prix 1985
#  "dry", # European Grand Prix 1985
# "dry", # Belgian Grand Prix 1984
# "wet", # San Marino Grand Prix 1984
# "dry", # British Grand Prix 1984
# "dry", # San Marino Grand Prix 1983
# "dry", # Belgian Grand Prix 1983
# "dry", # British Grand Prix 1983
# "dry", # San Marino Grand Prix 1982
# "dry", # British Grand Prix 1982
# "dry", # British Grand Prix 1980
# "dry", # Spanish Grand Prix 1977
# "dry", # Spanish Grand Prix 1976
# "dry", # Belgian Grand Prix 1976
# "dry", # French Grand Prix 1976
# "dry", # Austrian Grand Prix 1976
# "dry", # Italian Grand Prix 1976
# "dry", # South African Grand Prix 1974
# "dry", # Spanish Grand Prix 1974
# "dry", # Belgian Grand Prix 1974
# "wet", # Monaco Grand Prix 1974
# "dry", # Swedish Grand Prix 1974
# "dry", # Dutch Grand Prix 1974
# "dry", # French Grand Prix 1974
# "dry", # British Grand Prix 1974
# "dry", # Austrian Grand Prix 1974
# "dry", # Italian Grand Prix 1974
# "dry", # Argentine Grand Prix 1973
# "dry", # Monaco Grand Prix 1973
#"dry", # Swedish Grand Prix 1973
# "dry", # Austrian Grand Prix 1973
# "dry", # Italian Grand Prix 1973
# "dry", # Argentine Grand Prix 1972
# "dry", # Spanish Grand Prix 1972
# "dry", # Belgian Grand Prix 1972
# "dry", # French Grand Prix 1972
# "dry", # German Grand Prix 1972
#  "dry", # French Grand Prix 1971
# "dry", # German Grand Prix 1971
# "dry", # Italian Grand Prix 1971
# "dry", # Belgian Grand Prix 1970
# "dry", # British Grand Prix 1970
# "dry", # German Grand Prix 1970
# "dry", # Austrian Grand Prix 1970
# "dry", # Mexican Grand Prix 1970
# "dry", # German Grand Prix 1969
# "dry", # South African Grand Prix 1967
# "dry", # Monaco Grand Prix 1967
# "dry", # Dutch Grand Prix 1967
# "dry", # Belgian Grand Prix 1967
# "dry", # French Grand Prix 1967
# "dry", # British Grand Prix 1967
# "dry", # Mexican Grand Prix 1967
# "dry", # sout africa 1965
# "dry", # monaco 1965
# "dry", # dutch 1965
# "dry", # italian 1965
# "dry", # mexican 1965
# "dry", # monaco 1965
# "dry", # mexican 1964
# "dry", # monaco 1960
# "dry", # indianapolis 1960
# "dry", # dutch gp 1960 
# "dry", # belgian gp 1960
# "dry", # portugese 1960
# "dry", # indianapolis 1959
# "dry", # argetine 1958
# "dry", # monaco 1958
# "dry", # dutch 1958
# "dry", # indianapolis 1958
# "dry", # french 1958
# "dry", # british 1958
# "dry", # german 1958
# "wet", # portugese 1958
# "dry", # italian 1958
# "dry", # argentine 1957
# "dry", # monaco 1957
# "dry", # french gp 1957
# "dry", # german gp 1957
# "dry", # italian gp 1957
# "dry", # french 1956
# "dry", # german 1956 
# "dry", # monaco 1955
# "dry", # belgian 1955
# "dry", # italian 1955
# "dry", # monaco 1950
# "dry", # chinese 2013
# "dry", # bahrain 2013
# "dry", # belgian 2013
# "dry", # bahrain 2017
# "dry", # sochi 2017
# "dry", # spain 2017
# "dry", # monaco 2017
# "dry", # brazil 2017
# "dry", # bahrain 2018
# "dry", # shanghai 2018
# "dry", # monaco 2018
# "dry", # hungary 2018
# "dry", # belgium 2018
# "dry", # italy 2018
# "dry", # singapore 2018
# "dry", # russia 2018
# "dry", # japan 2018
# "dry", # usa 2018
# "dry", # mexico 2018
# "dry", # brazil 2018
# "dry", # abu dhabi 2018
# "dry", # australia 2019
# "dry", # china 2019
# "dry", # azerbaijan 2019
# "dry", # spain 2019
# "dry" # brazil 2021
#)

#####

## Manually add missing circuit info ----
circuit_missing_idx <- which(is.na(race_info$circuit_type))
circuit_type_missing <- race_info[c(circuit_missing_idx), ] 
which(is.na(race_info$circuit_type))

vilka_banor <- unique(circuit_type_missing$circuitRef)
# "dijon = permanent"
# "zolder = permanent"
# "anderstopr = permanent"
# "interlagos = permanent"

race_info[circuit_missing_idx, "circuit_type"] <- c(
  "permanent", # zolder 1982
  "permanent", # zolder 1982
  "permanent", # zolder 1980
  "permanent", # zolder 1979
  "permanent", # dijon  1979
  "permanent", # zolder 1978
  "permanent", # zolder 1977
  "permanent", # dijon  1977
  "permanent", # zolder 1976
  "permanent", # anderstorp  1976
  "permanent", # zolder 1975
  "permanent", # anderstorp  1975
  "permanent"  # brazil  2021
)

which(is.na(race_info$circuit_type))


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
