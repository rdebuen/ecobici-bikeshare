#Experiment to make a visualization that captures the different changes that impact the
# Mexico city driving restriction policy and the changes made in my study persiod (2016-june 2017)

rm(list = ls())
library(vistime)
library(ggplot2)
setwd("/Users/RebecadeBuen/Google Drive/ecobici data project")
HNC.data <- data.frame(
Notes = c("Starts at IMECA=176"," Starts at IMECA=150", 
                 "Based on car age","Based on emissions tests","Restriction applied to all cars","","","","",
                 "","","","", "", ""),
  
 Type_of_change = c("Environmental contingency rules","Environmental contingency rules",
                    "Exemption sticker number rules","Exemption sticker number rules",
                    "Temporary ruling ",
                    "Environmental contingency",
                    "Environmental contingency",
                    "Environmental contingency",
                    "Environmental contingency",
                    "Environmental contingency",
                    "Environmental contingency",
                    "Environmental contingency",
                    "Environmental contingency",
                    "Environmental contingency",
                    "Environmental contingency"),
  start_date = as.Date(c("01/01/2016","06/04/2016","01/01/2016","1/07/2016", "05/04/2016",
                         "11/08/2016","08/07/2016","31/05/2016", "27/05/2016","24/05/2016",
                         "20/05/2016", "14/05/2016", "02/05/2016", "05/04/2016","14/03/2016"), "%d/%m/%Y"),
  end_date = as.Date(c("06/04/2016","31/12/2016","30/06/2016", "31/12/2016","30/06/2016", "12/08/2016", 
                       "09/07/2016","01/06/2016", "28/05/2016", "25/05/2016", "21/05/2016", "15/05/2016",
                      "05/05/2016", "06/04/2016", "17/03/2016"), "%d/%m/%Y"),
color = c("#FFAAAA",
          "#D46A6A",
          "#801515",
          "#FFAAAA",
          "#AA3939",
          "#550000",
          "#550000",
          "#550000",
          "#550000",
          "#550000",
          "#550000",
          "#550000",
          "#550000",
          "#550000",
          "#550000"),
  stringsAsFactors = FALSE
  )

vistime(HNC.data, events = "Notes", groups = "Type_of_change", 
        start = "start_date", end = "end_date", colors = "color")

 
