
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Test_egen_kval_och_grid")
#Deskriptiv dataanalys
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(ggrepel)
library(viridis)
library(circlize)
library(readr)

##### Data inladdning #####
results                 <- read_csv("dat/f1db_csv/results.csv")
races                   <- read_csv("dat/f1db_csv/races.csv")
circuits                <- read_csv("dat/f1db_csv/circuits.csv")
drivers                 <- read_csv("dat/f1db_csv/drivers.csv")
driversStandings        <- read.csv("dat/f1db_csv/driver_standings.csv")
constructors            <- read_csv("dat/f1db_csv/constructors.csv")
constructorStandings    <- read_csv("dat/f1db_csv/constructor_standings.csv")
constructorResults      <-read_csv("dat/f1db_csv/constructor_results.csv")

##### Databearbetning #####
# Results
results$fastestLapSpeed <- as.numeric(results$fastestLapSpeed)
convertFastestLap <- function(x){
  if(length(x)>0){
    curMinute<-as.numeric(strsplit(x,":")[[1]][1])
    curSecond<-as.numeric(strsplit(strsplit(x,":")[[1]][2],"\\.")[[1]][1])
    return(curMinute*60 + curSecond)
  }
  else if(length(x)==0){
    return(NA)
  }
}
results$fastestLapTimeNum <- sapply(results$fastestLapTime, convertFastestLap)

# Races
races$date<-as.Date(races$date,"%Y-%m-%d")
races$name<-gsub(" Grand Prix","",races$name)
results_2<-left_join(
  results %>% dplyr::select(-time, -fastestLapTime), 
  races %>% dplyr::select(-time, -url), 
  by='raceId')

# Circuits
races <- left_join(races %>% select(-name,-url), circuits %>% select(-url), by='circuitId')

##### Statistik omkörningar över tid #####
results_omkorningar_1 <- results_2 
results_omkorningar_1$positionText <- as.numeric(results_omkorningar_1$positionText)  
results_omkorningar <- results_omkorningar_1 %>% drop_na(positionText)
summering_omkorningar <- results_omkorningar %>% arrange(name, grid) %>%
  group_by(raceId) %>% 
  mutate(rank.grid = rank(grid), rank.position= rank(positionOrder), netto.overtakes = rank(positionOrder)- rank(grid))
summering_omkorningar$absolut_overtakes <- abs(summering_omkorningar$netto.overtakes)
summering_omkorningar_results <- summering_omkorningar %>%
  group_by(name, year) %>%
  summarize(med_omkorningar = median(absolut_overtakes, na.rm=TRUE)) %>%
  ggplot(aes(x = factor(year), y = med_omkorningar, color = med_omkorningar)) +
  geom_boxplot(alpha = .25) + theme_fivethirtyeight() +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(min(summering_omkorningar$year),max(summering_omkorningar$year),10)) + 
  labs(title = "Förändring grid/slutposition per år",
       subtitle = "Absoluttal median grupperad per race") +
  guides(color = FALSE)
summering_omkorningar_results
ggsave("desk_img/omkorningar_per_ar.png", plot = summering_omkorningar_results, width = 12, height = 6, bg = "white")


##### Deskriptiv statistik fÃ¶rare #####
drivers$age_driver <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(drivers$dob, "%Y"))
drivers<-left_join(drivers %>% select(-url), driversStandings,by='driverId')


results_3 <- left_join(
  results, 
  drivers %>% dplyr::rename(number_drivers = number) %>% select(-points, -position, -positionText),
  by=c('driverId','raceId')) 

results_3 <- left_join(results_3,races %>% select(-time), by='raceId')

winsDis <- results_3 %>% 
  filter(position==1) %>% 
  group_by(driverRef, circuitRef) %>% 
  summarize(count=n()) %>%
  mutate(allWins = sum(count)) %>%
  ggplot(aes(x=allWins)) +
  geom_histogram(bins=50) + theme_fivethirtyeight() + ggtitle("Distribution of the number of victories")
plot(winsDis)


#Barplot av de tio fÃ¶rarna med flest vinster, ABSOLUTTAL
winsBar_15 <- results_3 %>% 
  filter(position==1) %>%
  group_by(driverRef, country) %>% 
  summarise(count=n()) %>%
  mutate(allWins = sum(count))
top15_driver_win <- as.numeric(sort(unique(winsBar_15$allWins), decreasing = TRUE)[15])

winsBar_15_1 <- results_3 %>% 
  dplyr::filter(position==1) %>% 
  dplyr::group_by(driverRef, country) %>% 
  dplyr::summarize(count=n()) %>%
  dplyr::mutate(allWins = sum(count)) %>% 
  dplyr::filter(allWins > top15_driver_win)

winsBar_15_1_plot <- winsBar_15_1 %>%
  dplyr::group_by(driverRef, country) %>% 
  ggplot(aes(x=reorder(driverRef, allWins),y= count)) +
  geom_bar(aes(fill=country),stat='identity',color='white',size=.1) + 
  coord_flip() + theme_fivethirtyeight() + 
  scale_fill_manual(name="",values = viridis::viridis(33)) +
  guides(fill=guide_legend(ncol=5)) + 
  theme(legend.text= element_text(size=8),
        legend.key.size = unit(.1, "cm"),
        legend.position=c(.8,.1)) + 
  labs(title="Antal vinster per förare",
       subtitle="De 15 bästa")
plot(winsBar_15_1_plot)
ggsave("desk_img/vinster_per_forare_topp_15.png", plot = winsBar_15_1_plot, width = 9, height = 6, bg = "white")


#Barplot av de tio fÃ¶rarna med flest vinster, % av antal starter
results_4 <- left_join(
  results, 
  drivers %>% dplyr::rename(number_drivers = number) %>% select(-points, -position, -positionText),
  by=c('driverId','raceId')) 
results_4 <- left_join(results_4,races %>% select(-time), by='raceId')
results_4$vinst <- ifelse(results_4$position == 1, 1, 0)

winsBar_15_per <- results_4 %>% 
  group_by(driverRef) %>% 
  summarize(count = n(), vinster = sum(vinst)) %>%
  mutate(vinstprocent = vinster / count) %>%
  filter(count > 5)
top15_driver_win_per <- as.numeric(sort(unique(winsBar_15_per$vinstprocent), decreasing = TRUE)[15])

winsBar_15_per <- winsBar_15_per %>% 
  dplyr::filter(vinstprocent >= top15_driver_win_per)

winsBar_15_per_plot <- ggplot(data=winsBar_15_per, aes(x= reorder(driverRef, vinstprocent), y = vinstprocent)) +
  geom_bar(aes(fill=driverRef), stat="identity", color = 'white', size = .1) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_fivethirtyeight() +
  scale_fill_manual(name="",values = viridis::viridis(15)) +
  labs(title="Andel vinster per race per förare i %",
       subtitle="De 15 bästa")
plot(winsBar_15_per_plot)
ggsave("desk_img/vinstprocent_per_forare_topp_15.png", plot = winsBar_15_per_plot, width = 9, height = 6, bg = "white")





##### Deskriptiv statistik konstruktÃ¶r #####
constructorResults<-left_join(
  constructorResults, 
  races %>% rename(name_races = name), by='raceId')
constructorResults <- left_join(constructorResults, constructors %>% select(-url) %>% rename(name_constructor = name), by='constructorId')
constructorResults <- left_join(constructorResults, constructorStandings %>% rename(point_constructor = points) %>% select(-positionText), by=c('constructorId','raceId'))


  
winConstructors <- constructorResults %>% 
  filter(wins == 1, name_constructor %in% c('Ferrari','McLaren','Williams','Brabham','BRM', 'Tyrrell', 'Red Bull', 'Renault', 'Benetton', 'Mercedes', 'Team Lotus', 'Ligier', 'Jordan', 'Lotus F1', 'March' )) %>%
  group_by(name_constructor) %>% 
  summarize(count=n()) %>% 
  filter(count>0) %>%
  ggplot(aes(x=reorder(name_constructor, count),y= count,fill=count)) +
  geom_bar(stat='identity',color='white',size=.1) + 
  coord_flip() + theme_fivethirtyeight() + 
  scale_fill_gradientn(name="",colors = viridis::viridis(15)) +
  guides(fill=guide_legend(ncol=3)) + 
  theme(legend.text= element_text(size=10),
        legend.key.size = unit(.1, "cm"),
        legend.position=c(.65,.20)) + 
  labs(title="Antal vinster per Team",
       subtitle="De 15 bästa") + guides(fill=F)
plot(winConstructors)
ggsave("desk_img/vinst_per_konstruktor_topp_15.png", plot = winConstructors, width = 9, height = 6, bg = "white")


#Barplot av de tio med konstruktÃ¶rerna hÃ¶gst vinstprocent % av antal starter
winsTeam <- constructorResults %>% 
  group_by(name_constructor, year) %>% 
  summarize(count = n(), vinster = max(wins)) %>%
  mutate(vinstprocent = vinster / count) %>%
  filter(vinster > 0)

winsTeam_per <- winsTeam  %>% 
  summarize(count = sum(count), vinster = sum(vinster)) %>%
  mutate(vinstprocent = vinster / count) %>%
  filter(count > 5)
top15_driver_team_per <- winsTeam_per[order(winsTeam_per$vinstprocent, decreasing = TRUE), ]



winsTeam_15_per <- winsTeam_per %>% 
  dplyr::filter(vinstprocent >= top10_driver_team_per)

winsTeam_15_per_plot <- ggplot(data=top15_driver_team_per[1:15, ], aes(x= reorder(name_constructor, vinstprocent), y = vinstprocent)) +
  geom_bar(aes(fill=name_constructor), stat="identity", color = 'white', size = .1) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_fivethirtyeight() +
  scale_fill_manual(name="",values = viridis::viridis(15)) +
  labs(title="Andel vinster per race per team i %",
       subtitle="De 15 bästa")
plot(winsTeam_15_per_plot)
ggsave("desk_img/vinstprocent_per_team_topp_15.png", plot = winsTeam_15_per_plot, width = 9, height = 6, bg = "white")



##### Chordiagram pÃ¥ relation mellan fÃ¶rare och team #####

### Data ###
results <- read_csv("dat/f1db_csv/results.csv")
results$fastestLapSpeed <- as.numeric(results$fastestLapSpeed)
convertFastestLap <- function(x){
  if(length(x)>0){
    curMinute<-as.numeric(strsplit(x,":")[[1]][1])
    curSecond<-as.numeric(strsplit(strsplit(x,":")[[1]][2],"\\.")[[1]][1])
    return(curMinute*60 + curSecond)
  }
  else if(length(x)==0){
    return(NA)
  }
}
results$fastestLapTimeNum <- sapply(results$fastestLapTime, convertFastestLap)
## Races
races   <- read_csv("dat/f1db_csv/races.csv")
races$date<-as.Date(races$date,"%Y-%m-%d")
races$name<-gsub(" Grand Prix","",races$name)
results_2<-left_join(
  results %>% dplyr::select(-time, -fastestLapTime), 
  races %>% dplyr::select(-time, -url), 
  by='raceId')
## circuits
circuits   <- read_csv("dat/f1db_csv/circuits.csv")
races <- left_join(races %>% select(-name,-url), circuits %>% select(-url), by='circuitId')
## Drivers
drivers   <- read_csv("dat/f1db_csv/drivers.csv")
drivers$age_driver <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(drivers$dob, "%Y"))

#load driversStandings
driversStandings <- read.csv("dat/f1db_csv/driver_standings.csv")
drivers<-left_join(drivers %>% select(-url), driversStandings,by='driverId')

results_3 <- left_join(
  results, 
  drivers %>% dplyr::rename(number_drivers = number) %>% select(-points, -position, -positionText),
  by=c('driverId','raceId')) 

results_3 <- left_join(results_3,races %>% select(-time), by='raceId')

constructors <- read_csv("dat/f1db_csv/constructors.csv")
constructorStandings <- read_csv("dat/f1db_csv/constructor_standings.csv")
constructorResults<-read_csv("dat/f1db_csv/constructor_results.csv")
constructorResults<-left_join(
  constructorResults, 
  races %>% rename(name_races = name), by='raceId')
constructorResults <- left_join(constructorResults, constructors %>% select(-url) %>% rename(name_constructor = name), by='constructorId')
constructorResults <- left_join(constructorResults, constructorStandings %>% rename(point_constructor = points) %>% select(-constructorStandingsId), by=c('constructorId','raceId'))



