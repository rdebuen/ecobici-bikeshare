# station locations, nearest pollution and nearest weather data
#install.packages("jsonlite")

#this script will determine the station-level characteristics of the ecobici system
# nearest weather/pollution stations
# number of metro, metrobus, bus or other within given radius

#the output is a file of ecobici station locations, first and second nearest pollution/weather stations,
#and number of public transit stops within 100m radius

# setwd("/Users/RebecadeBuen/Google Drive/ecobici data project")

library(jsonlite)
library(dplyr)
library(readr)
library(rgeos)
library(sp)
library(geosphere)
library(tidyverse)
library(ggmap)

#install.packages("rgeos")
#create dataframe for ecobici stations including ID, name, location (lat,lon)

data <- fromJSON("./data/stations.json")
ecobici_stations <- as.data.frame(data$stations)
str(ecobici_stations)
ecobici_stations <- data.frame(ecobici_id=ecobici_stations$id, 
                               ecobici_name=ecobici_stations$name, 
                          ecobici_lat=ecobici_stations$location$lat, 
                          ecobici_lon=ecobici_stations$location$lon)
ecobici_stations$ecobici_id <- as.character(ecobici_stations$ecobici_id)

#str(ecobici_stations)

# create dataframe with latitude and longitude of pollution stations

poll_stations = read_csv("./data/cat_estacion.csv", skip = 1) %>% 
                select(poll_id = cve_estac, 
                            poll_lon = longitud, 
                            poll_lat = latitud, 
                            poll_alt = alt,
                            poll_status = obs_estac) %>%
  #find acive stations
                  filter(is.na(poll_status))  %>% 
  #remove column of NAs left after filtering active stations
                  subset(select=-c(poll_status, poll_alt))

head(poll_stations)


# create dataframe with latitude and longitude of weather stations

wet_stations = read_csv("./data/cat_estacion_weather.csv", skip = 1) %>% 
  select(wet_id = cve_estac, 
         wet_lon = longitud, 
         wet_lat = latitud, 
         wet_alt = alt,
         wet_status = obs_estac) %>%
  #find acive stations
  filter(is.na(wet_status))  %>% 
  #remove column of NAs left after filtering active stations
  subset(select=-c(wet_status, wet_alt))

#checked if weather stations and pollution stations are the same
wet_stations == poll_stations

# they are the same, I will just use poll_stations as reference

# In this section I will determine which weather/pollution stations are nearest 
# to the bikesare stations to get the most accurate environmental data.

#set1
set1 <- ecobici_stations
coordinates(set1) <- ~ecobici_lon + ecobici_lat

#set2
set2 <- poll_stations
coordinates(set2) <- ~poll_lon + poll_lat

set1sp <- SpatialPoints(set1)
set2sp <- SpatialPoints(set2)

which.second.min <- function(x){
  return(which(x==min(x[ x > min(x)])))
}

which.second.min(c(3,4,5,6,7))

set1$nearest_in_set2_num <- apply(gDistance(set1sp, set2sp, byid=TRUE), 2, which.min)
set1$nearest_in_set2 <- set2$poll_id[set1$nearest_in_set2_num]


set1$second_nearest_in_set2_num <- apply(gDistance(set1sp, set2sp, byid=TRUE), 2, which.second.min)
set1$second_nearest_in_set2<- set2$poll_id[set1$second_nearest_in_set2_num]

#find the public transportation stops in 200m radius of bikeshare stations

PublicTransportData <- read_csv("./data/PublicTransportation.csv", guess_max=1000000)
str(PublicTransportData)
which(is.na(as.numeric(PublicTransportData$stop_lat)))

transit_stops <- data_frame(PT_agency_id = PublicTransportData$agency_id, 
                               PT_stop_name = PublicTransportData$stop_name,
                               PT_stop_id = PublicTransportData$stop_id,
                               PT_stop_lat = as.numeric(PublicTransportData$stop_lat), 
                               PT_stop_lon = PublicTransportData$stop_lon) %>% 
                  filter(!is.na(PT_stop_lat), !is.na(PT_stop_lon)) %>%
  mutate_at(vars(PT_stop_lat, PT_stop_lon), funs(as.numeric))


#set spatial coordinates for transit stops 

set3 <- transit_stops %>% distinct(PT_stop_id, .keep_all=TRUE)
coordinates(set3) <- ~PT_stop_lon + PT_stop_lat
head(set3)

set3sp <- SpatialPoints(set3)

gWithinDistance(set1sp, set3sp, dist=200)

# function to count number of public transportation stations
# within a given radius (dist). X is bikeshare stops, Y is nearby points to count.
# create 2 lists of matrices, each list a set of matrices
# matrix for each coordinate of BD first list is bs coords repeated
# compared to matrix of other points 
# estimates distance between bike station and everything in the Y matrix
# produces list of all distances
# count which one is under given ditance

#function by CCL
get_distance_list <- function(x,y,dist){
  matrix_x <- x@coords
  matrix_y <- y@coords
  x_list <- vector("list", dim(x)[1])
  y_list <- vector("list", dim(x)[1])
  for(i in 1:dim(matrix_x)[1]){
    x_list[[i]] <- matrix(matrix_x[i,], nrow=dim(y)[1], ncol=2, byrow=T)
    y_list[[i]] <- y
  }
  distance_list <- vector("list", dim(x)[1])
  for(i in 1:dim(matrix_x)[1]){
    distance_list[[i]] <- distGeo(x_list[[i]], y_list[[i]])
  }
  distance_count <- vector("integer", dim(x)[1])
  for(i in 1:dim(matrix_x)[1]){
    distance_count[i] <- sum(distance_list[[i]] < dist)
  }
  return(distance_count)
}

set1@data$any_within_100 <- get_distance_list(set1, set3, 100)
set1@data$any_MB_100 <- get_distance_list(set1, set3[set3@data$PT_agency_id=="MB",], 100)

plot(set1)
 points(set3, col="red")
 
 #simple visualization spatial data
 qmplot(data = ecobici_stations , 
        x = ecobici_lon, y = ecobici_lat, 
        color = I("firebrick"), alpha = I(0.5)) + 
   geom_point(data=transit_stops, aes(x=PT_stop_lon, y=PT_stop_lat)) 
 
 
 #Put data together
 
 fixed_spatial_data <- data_frame(ecobici_id = ecobici_stations$ecobici_id, 
                                  ecobici_lat= ecobici_stations$ecobici_lat,
                                  ecobici_lon= ecobici_stations$ecobici_lon,
                                  nearest_poll= set1$nearest_in_set2,
                                  second_nearest_poll = set1$second_nearest_in_set2,
                                  stations_within_100m = set1$any_within_100) ##%>% save(file="Data.Rda")
                                  

