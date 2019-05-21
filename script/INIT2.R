pacman::p_load(RMySQL, dplyr, lubridate, ggplot2, readr)

###############################################################################

newDF <- read.csv("combined_DF.csv")

###############################################################################

# Data conversion:
newDF$Global_active_power <- newDF$Global_active_power / 60 #converting from kW/min to kW/h

newDF$Global_reactive_power <- newDF$Global_reactive_power / 60 #converting from kW/min to kW/h

newDF$Kitchen <- newDF$Kitchen / 1000 #converting from W/hour to kW/hour

newDF$Laundry <- newDF$Laundry / 1000 #converting from W/hour to kW/hour

newDF$WaterHeater_AirConditioner <- newDF$WaterHeater_AirConditioner / 1000 #converting from W/hour to kW/hour

write.csv(x = newDF, file = "converted_DF.csv")

###############################################################################

newDF <- read.csv("converted_DF.csv")

###############################################################################

#Energy not measured by submeters:
newDF$excess_energy <- newDF$Global_active_power - newDF$Kitchen - newDF$Laundry - newDF$WaterHeater_AirConditioner

###############################################################################

#Pricing for peak and offpeak:

newDF$price <- c(NA)

newDF$price <- ifelse (between(newDF$hour,left = 2, right = 7) | between(newDF$hour, left = 14, right = 17), yes = 0.123, no = 0.158)

###############################################################################