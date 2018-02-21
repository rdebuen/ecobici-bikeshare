

#In this script important dates are constructed as dummy variables
rm(list = ls())
library(lubridate)
library(tidyverse)

#create matrix to populate

date_time <-
  seq(as.POSIXct("2016-1-1"), as.POSIXct("2017-8-31"), "hours")
contingency_day <-
  federal_holidays <-
  school_breaks <-
  long_pollution_episodes <-
  sticker_regulation <-
  high_pollution_day_regulation <- rep(NA, length(date_time))

dates_df <-
  data.frame(
    date_time,
    contingency_day,
    federal_holidays,
    school_breaks,
    long_pollution_episodes,
    sticker_regulation,
    high_pollution_day_regulation
  )



# Construct date intervals in each relevant category

#official environmental contingency dates from M
# http://www.aire.df.gob.mx/descargas/ultima-hora/calidad-aire/pcaa/pcaa-historico-contingencias.pdf

contingencyDates <-
  data.frame(HPDR_start =
               as.POSIXct(
                 c(
                   "2016-8-11 16:00:00",
                   "2016-7-8 17:00:00",
                   "2016-5-31 16:00:00",
                   "2016-5-27 15:00:00",
                   "2016-5-24 15:00:00",
                   "2016-5-20 15:00:00",
                   "2016-5-14 17:00:00",
                   "2016-5-2 15:00:00",
                   "2016-4-5 17:00:00",
                   "2016-3-14 16:00:00"
                 )
               ),
             HPDR_end = as.POSIXct(
               c(
                 "2016-8-12 17:00:00",
                 "2016-7-9 15:00:00",
                 "2016-6-01 18:00:00",
                 "2016-5-28 18:00:00",
                 "2016-5-24 21:00:00",
                 "2016-5-21 17:00:00",
                 "2016-5-15 15:00:00",
                 "2016-5-5 17:00:00",
                 "2016-4-6 17:00:00",
                 "2016-3-17 16:00:00"
               )
             ))

#official federal holidays during stidy period
federalHolidayDates <- data.frame(FH_start =
                                    as.POSIXct(
                                      c(
                                        "2016-01-01 00:00:00",
                                        "2016-02-01 00:00:00",
                                        "2016-03-21 00:00:00",
                                        "2016-03-24 00:00:00",
                                        "2016-03-25 00:00:00",
                                        "2016-11-02 00:00:00",
                                        "2017-01-01 00:00:00",
                                        "2016-01-06 00:00:00",
                                        "2016-02-06 00:00:00",
                                        "2017-03-20 00:00:00",
                                        "2017-04-13 00:00:00",
                                        "2017-04-14 00:00:00",
                                        "2017-04-14 00:00:00",
                                        "2017-05-01 00:00:00",
                                        "2017-09-16 00:00:00",
                                        "2017-11-02 00:00:00",
                                        "2017-11-20 00:00:00",
                                        "2017-12-25 00:00:00"
                                      )
                                    ),
                                  FH_end =
                                    as.POSIXct(
                                      c(
                                        "2016-01-01 23:00:00",
                                        "2016-02-01 23:00:00",
                                        "2016-03-21 23:00:00",
                                        "2016-03-24 23:00:00",
                                        "2016-03-25 23:00:00",
                                        "2016-11-02 23:00:00",
                                        "2017-01-01 23:00:00",
                                        "2016-01-06 23:00:00",
                                        "2016-02-06 23:00:00",
                                        "2017-03-20 23:00:00",
                                        "2017-04-13 23:00:00",
                                        "2017-04-14 23:00:00",
                                        "2017-04-14 23:00:00",
                                        "2017-05-01 23:00:00",
                                        "2017-09-16 23:00:00",
                                        "2017-11-02 23:00:00",
                                        "2017-11-20 23:00:00",
                                        "2017-12-25 23:00:00"
                                      )
                                    ))

#official school breaks during study peridos
schoolBreakDates <- data.frame(SB_start =
                                 as.POSIXct(
                                   c(
                                     "2016-01-01 00:00:00",
                                     "2016-03-22 00:00:00",
                                     "2016-07-16 00:00:00",
                                     "2016-12-19 00:00:00",
                                     "2017-04-10 00:00:00",
                                     "2017-06-27 00:00:00",
                                     "2016-09-16 00:00:00",
                                     "2016-11-21 00:00:00",
                                     "2016-11-02 00:00:00",
                                     "2016-02-01 00:00:00",
                                     "2016-05-01 00:00:00",
                                     "2016-05-05 00:00:00",
                                     "2017-02-06 00:00:00",
                                     "2017-03-20 00:00:00",
                                     "2017-05-01 00:00:00",
                                     "2017-05-05 00:00:00",
                                     "2017-05-15 00:00:00"
                                   )
                                 ),
                               
                               SB_end =
                                 as.POSIXct(
                                   c(
                                     "2016-01-06 23:00:00",
                                     "2016-04-04 23:00:00",
                                     "2016-08-22 23:00:00",
                                     "2017-01-01 23:00:00",
                                     "2017-04-21 23:00:00",
                                     "2017-08-21 23:00:00",
                                     "2016-09-16 23:00:00",
                                     "2016-11-21 23:00:00",
                                     "2016-11-02 23:00:00",
                                     "2016-02-01 23:00:00",
                                     "2016-05-01 23:00:00",
                                     "2016-05-05 23:00:00",
                                     "2017-02-06 23:00:00",
                                     "2017-03-20 23:00:00",
                                     "2017-05-01 23:00:00",
                                     "2017-05-05 23:00:00",
                                     "2017-05-15 23:00:00"
                                   )
                                 ))


# a special pollution/restriction episode was declared april1-june30 2016 where addittional restrictions applied
longPollutionEpisodeDates <- data_frame(
                                    PE_start = as.POSIXct("2016-04-05 00:00:00"),
                                    PE_end = as.POSIXct("2016-06-30 30:00:00")
                                  )

#changes in driving restriction regulation rules
stickerRegulationDates <- data_frame(SR_start = as.POSIXct(c(
                                    "2016-01-01 00:00:00",
                                    "2016-07-01 00:00:00"
                                  )),
                                  SR_end = as.POSIXct(c(
                                    "2016-06-30 23:00:00",
                                    "2017-08-31 23:00:00"
                                  )))
                                

#changes in rules to declare an environmental contingency
pollDayRegChangeDates <- data_frame(start = as.POSIXct(c("2016-07-01 00:00:00",
                                                         "2016-07-01 00:00:00")),
                                    end = as.POSIXct(c("2016-12-3 23:00:00",
                                                       "2017-08-31 23:00")))


#this function checks if dates are in given intervals (as declared above)
extract_hour_seq <- 
  function(x){
   as.POSIXct(                                  # Need output to be a date
    unlist(                                     # Need to turn list into vector
     apply(x, 1, function(x) {        # Need to apply function over rows (1)
      seq.POSIXt(as.POSIXct(x[1]),   # Time sequence from here...
                 as.POSIXct(x[2]),     # to here...
                 by="hour")}                    # by hours.
      )), 
    origin = "1970-01-01 00:00:00" )            # Set origin at universal UNIX origin
}

#create dummmy variables for dates within interval
dates_df$contingency_day <- 
  if_else(date_time %in% extract_hour_seq(contingencyDates), 1, 0)

dates_df$federal_holidays <- 
  if_else(date_time %in% extract_hour_seq(federalHolidayDates), 1, 0)

dates_df$school_breaks <-
  if_else(date_time %in% extract_hour_seq(schoolBreakDates), 1, 0)

dates_df$long_pollution_episodes <- 
  if_else(date_time %in% extract_hour_seq(longPollutionEpisodeDates), 1, 0)

dates_df$sticker_regulation  <-  
  if_else(date_time %in% extract_hour_seq(stickerRegulationDates), 1, 0)

dates_df$high_pollution_day_regulation <- 
  if_else(date_time %in% extract_hour_seq(pollDayRegChangeDates), 1, 0)

#save DF as CSV
write.csv(dates_df,'control_dates.csv')


