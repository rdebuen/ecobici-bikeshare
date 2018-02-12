# pollution/weather/ compilation

library(tidyverse)
library(lubridate)

#load pollution data
pollution_2016 <-as.data.frame(read_csv("./data/contaminantes_2016.CSV", skip = 10)) 
pollution_2017 <-as.data.frame(read_csv("./data/contaminantes_2017.CSV", skip = 10)) 


#load weather data
weather_2016 <- as.data.frame(read_csv("./data/meteorologia_2016.CSV", skip = 10))
weather_2017 <-as.data.frame(read_csv("./data/meteorologia_2016.CSV", skip = 10))


#data frames with same columns, join with rbind
poll_weather <- rbind(pollution_2016, pollution_2017, weather_2016, weather_2017)

#change date format to y-m-d 
separate(data = poll_weather, col = date, into = c("date", "time"), sep = " ")

poll_weather$date <- as.Date(poll_weather$date, format= "%Y/%m/%d") 
poll_weather$time <- strptime(poll_weather$time, format = "%H:%M")


