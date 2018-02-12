# ecobici/pollution/weather/transportsystem data compilation

library(tidyverse)
library(lubridate)
# load("./data/ecobici_data.RData")
load("./data/ecobici_ridership.RData")

# for(i in 1:89){
#  print(head(data_store[[i]]$Fecha_Retiro))
# }
# Last 11 of these are in a different format.

# fix_date <- function(x){
#  paste(
#    str_extract(x, "(?<=/)[0-9]*$"),
#    str_extract(x, "(?<=/)[0-9]*(?=/)"),
#    str_extract(x, "^[0-9]*(?=/)"),
#  sep="-")
# }

# for(i in 1:length(data_store)){
#  if( str_detect(data_store[[i]]$Fecha_Retiro[1], "/") ){
#    data_store[[i]] <- data_store[[i]] %>% mutate_at(vars(Fecha_Retiro, Fecha_Arribo), funs(fix_date))
#    print(i)
#  }
# }

# setwd("/Users/RebecadeBuen/Google Drive/ecobici data project")
# sample_ecobici_0517 <- read_csv("2017-05.csv") 

# head(sample_ecobici_0517)
# str(sample_ecobici_0517)

# ecobici_ridership <- do.call(rbind, data_store)
# ecobici_ridership <-  bind_rows(data_store)
# save(ecobici_ridership, file="E:/Projects/ecobici/data/ecobici_ridership.RData")

# load("E:/Projects/ecobici/data/ecobici_ridership.RData")


# ecobici_ridership_sample <- ecobici_ridership %>% sample_n(100000) %>%
#  mutate(date=parse_date_time(as.character(Fecha_Retiro), "ymd"), hour=hour(Hora_Retiro)) %>%
#  filter(!is.na(date), !is.na(hour)) %>%
#  select(-Fecha_Retiro, -Hora_Retiro, -Fecha_Arribo, -Hora_Arribo)  # Creating date and hour variables for grouping based on departure time/date

# ecobici_ridership_sample[which(is.na(ecobici_ridership_sample$departure_DT)),]
# use the above line to find rows that have date issues (did not parse)


# Use this code to tabulate departure stations, arrange in ascending order, print first 50.
stations_to_drop <- rbind(ecobici_ridership %>% count(Ciclo_Estacion_Retiro) %>% arrange(n) %>% filter(n < 2500) %>% distinct(Ciclo_Estacion_Retiro) %>% rename(station=Ciclo_Estacion_Retiro), 
                          ecobici_ridership %>% count(Ciclo_Estacion_Arribo) %>% arrange(n) %>% filter(n < 2500) %>% distinct(Ciclo_Estacion_Arribo) %>% rename(station=Ciclo_Estacion_Arribo))


#for data clean later, find out what station 1002 and 1001 is and make sure it is OK to remove levels
#ALSO, CHECK FOR OTHER WIERD LEVELS

`%!in%` <- Negate(`%in%`)

#ecobici_ridership_sample <-
#  ecobici_ridership_sample %>% filter(Ciclo_Estacion_Retiro %!in% stations_to_drop$station,
#                                      Ciclo_Estacion_Arribo %!in% stations_to_drop$station)

# now we group by time and station
#MAKE SURE STATION SPATIAL DATA IS MATCHING BY ID NOT BY POSITION IN COLUMN
ecobici_ridership_orig <- ecobici_ridership
ecobici_ridership <- ecobici_ridership_orig %>%
  #  mutate(departure_DT = paste(Fecha_Retiro, 
  #                              Hora_Retiro, sep = " "), 
  #         arrival_DT =   paste(Fecha_Arribo, 
  #                              Hora_Arribo, sep = " ")) %>%
  #    mutate(departure_DT=parse_date_time(departure_DT, orders="ymd HMS"),
  #         arrival_DT=parse_date_time(arrival_DT, orders="ymd HMS")) %>%
  mutate(date_dep=parse_date_time(as.character(Fecha_Retiro), "ymd"), hour_dep=hour(Hora_Retiro),
         date_arr=parse_date_time(as.character(Fecha_Arribo), "ymd"), hour_arr=hour(Hora_Arribo)) %>%
  filter(!is.na(date_dep), !is.na(hour_dep), !is.na(date_arr), !is.na(hour_arr)) %>%
  select(-Fecha_Retiro, -Hora_Retiro, -Fecha_Arribo, -Hora_Arribo) %>% 
  filter(Ciclo_Estacion_Retiro %!in% stations_to_drop$station,
         Ciclo_Estacion_Arribo %!in% stations_to_drop$station)

ecobici_ridership_departures <- ecobici_ridership %>% 
  group_by(Ciclo_Estacion_Retiro, date_dep, hour_dep) %>% 
  summarize(departures=n()) %>% 
  ungroup() %>% 
  rename(station=Ciclo_Estacion_Retiro, date=date_dep, hour=hour_dep)

ecobici_ridership_arrivals <- ecobici_ridership %>% 
  group_by(Ciclo_Estacion_Arribo, date_arr, hour_arr) %>% 
  summarize(arrivals=n()) %>%
  ungroup() %>% 
  rename(station=Ciclo_Estacion_Arribo, date=date_arr, hour=hour_arr)

all_days <- date(seq.Date(min(date(ecobici_ridership$date_dep)), max(date(ecobici_ridership$date_dep)), by="day"))
all_hours <- 0:23
all_stations <- unique(c(ecobici_ridership$Ciclo_Estacion_Retiro,
                       ecobici_ridership$Ciclo_Estacion_Arribo))

full_grid <- expand.grid(hour=all_hours, date=all_days, station=all_stations)

ecobici_station_hour <- full_grid %>% 
  full_join(ecobici_ridership_departures %>% mutate(date=date(date)), by=c("station", "date", "hour")) %>% 
  full_join(ecobici_ridership_arrivals %>% mutate(date=date(date)), by=c("station", "date", "hour"))
ecobici_station_hour[is.na(ecobici_station_hour)] <- 0
# Want a data frame that has station, date, and hour columns with rows for every possible station hour.

# ecobici_station_hour$date[ecobici_station_hour$date %!in% unique(full_grid$date)]
# ecobici_station_hour$hour[ecobici_station_hour$hour %!in% unique(full_grid$hour)]
# ecobici_station_hour$station[ecobici_station_hour$station %!in% unique(full_grid$station)]                           
ecobici_station_hour <- ecobici_station_hour %>%
  mutate(date_hour=date+dhours(hour))

# ggplot(ecobici_station_hour, aes(x=date_hour, y=departures, group=station)) + geom_line()


save(ecobici_station_hour, file="./data/ecobici_station_hour.RData")
