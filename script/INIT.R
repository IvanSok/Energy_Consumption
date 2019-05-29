# Libraries: --------------------------------------------------------------
pacman::p_load(RMySQL, dplyr, lubridate, ggplot2, readr, plotly, ggfortify, forecast, padr, DescTools,
               stats, xts, prophet, purrr)
###############################################################################
# Github setup ------------------------------------------------------------
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(current_path)
###############################################################################
# Connecting the database -------------------------------------------------
con = dbConnect(MySQL(),
                 user = 'deepAnalytics',
                 password = 'Sqltask1234!',
                 dbname = 'dataanalytics2018',
                 host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
                
###############################################################################
# Creating dataframes per year --------------------------------------------

yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")

# Creating a Primary (multi year) Data Frame:
newDF <- bind_rows(yr_2007, yr_2008, yr_2009)
###############################################################################
# Preprocessing -----------------------------------------------------------

# #Combine Date and Time attribute values in a new attribute column:
 newDF$DateTime <- paste(newDF$Date, newDF$Time)
 newDF$Date <- date(newDF$DateTime)
 
# #Move the DateTime attribute within the dataset:
 newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF) -1))]

# #Convert DateTime from POSIXlt to POSIXct:
 newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
 
# #Add the time zone:
 attr(newDF$DateTime, "tzone") <- "Europe/Paris"

# #Exctracting "year" attribute from DateTime:
 newDF$year <- year(newDF$DateTime)
 
# #Exctracting "quarter" attribute from DateTime:
 newDF$quarter <- quarter(newDF$DateTime, fiscal_start = 3)
 
# #Exctracting "month" attribute from DateTime:
 newDF$month <- month(newDF$DateTime)
 
# #Exctracting "week" attribute from DateTime:
 newDF$week <- week(newDF$DateTime)
# 
# #Exctracting "weekday" attribute from DateTime:
 newDF$weekday <- wday(newDF$DateTime)

# #Exctracting "day" attribute from DateTime:
 newDF$day <- day(newDF$DateTime)
 
# #Exctracting "hour" attribute from DateTime:
 newDF$hour <- hour(newDF$DateTime)
 
# #Exctracting "minute" attribute from DateTime:
 newDF$minute <- minute(newDF$DateTime)

# # Renaming submeter columns by room:
 newDF <- newDF %>% rename(Kitchen = Sub_metering_1,
                           Laundry = Sub_metering_2,
                           WaterHeater_AirConditioner = Sub_metering_3)
 
# # Renaming quarters:
 newDF$quarter <- as.factor(newDF$quarter)
 class(newDF$quarter)
 levels(newDF$quarter)
 levels(newDF$quarter) <- c("Spring", "Summer", "Autumn", "Winter")



# # Filtering out 2010 data in newDF:
newDF <-filter(newDF, year != 2010)
 

Off_peak <- read_csv("NormalFares.csv")

Peak <- read_csv("PeakFares.csv")

newDF$Peak <- c(NA)


###############################################################################
# Plots -------------------------------------------------------------------

tail(newDF)

# Plot of sub meter consumtions per year/month:



Sum_newDF <- newDF %>% group_by(year, month) %>% summarise( Kitchen = sum(Kitchen),
                                                            Laundry = sum(Laundry),
                                                            WaterHeater_AirConditioner = sum(WaterHeater_AirConditioner),
                                                            Global_active_power = sum(Global_active_power),
                                                            Global_reactive_power = sum(Global_reactive_power),
                                                            Global_intensity = sum(Global_intensity)) 





ggplot(Sum_newDF, aes(x = month)) +
  geom_line(aes(y = Sum_newDF$Kitchen, col = "Kitchen")) + geom_point((aes(y= Sum_newDF$Kitchen, col = "Kitchen"))) +
  geom_line(aes(y = Sum_newDF$Laundry, col = "Laundry")) + geom_point((aes(y= Sum_newDF$Laundry, col = "Laundry"))) +
  geom_line(aes(y = Sum_newDF$WaterHeater_AirConditioner, col = "WaterHeater_AirConditioner")) +
  geom_line(aes(y = Sum_newDF$Global_active_power, col = "Global_active_power")) +
  geom_line(aes(y = Sum_newDF$Global_reactive_power, col = "Global_reactive_power")) +
  geom_line(aes(y = Sum_newDF$Global_intensity, col = "Global_intensity")) +
  geom_point((aes(y= Sum_newDF$WaterHeater_AirConditioner, col = "WaterHeater_AirConditioner"))) +
  geom_point((aes(y= Sum_newDF$Global_active_power, col = "Global_active_power"))) +
  geom_point((aes(y= Sum_newDF$Global_reactive_power, col = "Global_reactive_power"))) +
  geom_point((aes(y= Sum_newDF$Global_intensity, col = "Global_intensity"))) +
  facet_wrap(~year) + scale_x_continuous(breaks = c(1:12), labels = c(1:12), limits = c(1,12)) +
  ylab("Energy consumtion")


# Plot per hour:


Sum_newDF <- newDF %>% group_by(hour) %>% summarise( Kitchen = sum(Kitchen),
                                                     Laundry = sum(Laundry),
                                                     WaterHeater_AirConditioner = sum(WaterHeater_AirConditioner),
                                                     Global_active_power = sum(Global_active_power),
                                                     Global_reactive_power = sum(Global_reactive_power),
                                                     Global_intensity = sum(Global_intensity)) 


ggplot(Sum_newDF, aes(x = hour)) +
  geom_line(aes(y = Sum_newDF$Kitchen, col = "Kitchen")) + geom_point((aes(y= Sum_newDF$Kitchen, col = "Kitchen"))) +
  geom_line(aes(y = Sum_newDF$Laundry, col = "Laundry")) + geom_point((aes(y= Sum_newDF$Laundry, col = "Laundry"))) +
  geom_line(aes(y = Sum_newDF$WaterHeater_AirConditioner, col = "WaterHeater_AirConditioner")) +
  geom_line(aes(y = Sum_newDF$Global_active_power, col = "Global_active_power")) +
  geom_line(aes(y = Sum_newDF$Global_reactive_power, col = "Global_reactive_power")) +
  geom_line(aes(y = Sum_newDF$Global_intensity, col = "Global_intensity")) +
  geom_point((aes(y= Sum_newDF$WaterHeater_AirConditioner, col = "WaterHeater_AirConditioner"))) +
  geom_point((aes(y= Sum_newDF$Global_active_power, col = "Global_active_power"))) +
  geom_point((aes(y= Sum_newDF$Global_reactive_power, col = "Global_reactive_power"))) +
  geom_point((aes(y= Sum_newDF$Global_intensity, col = "Global_intensity"))) +
  scale_x_continuous(breaks = c(1:24), labels = c(1:24), limits = c(1,24)) +
  ylab("Energy consumtion")




# Plot per weekday:

Sum_newDF <- newDF %>% group_by(weekday) %>% summarise( Kitchen = sum(Kitchen),
                                                        Laundry = sum(Laundry),
                                                        WaterHeater_AirConditioner = sum(WaterHeater_AirConditioner),
                                                        Global_active_power = sum(Global_active_power),
                                                        Global_reactive_power = sum(Global_reactive_power),
                                                        Global_intensity = sum(Global_intensity)) 


ggplot(Sum_newDF, aes(x = weekday)) +
  geom_line(aes(y = Sum_newDF$Kitchen, col = "Kitchen")) + geom_point((aes(y= Sum_newDF$Kitchen, col = "Kitchen"))) +
  geom_line(aes(y = Sum_newDF$Laundry, col = "Laundry")) + geom_point((aes(y= Sum_newDF$Laundry, col = "Laundry"))) +
  geom_line(aes(y = Sum_newDF$WaterHeater_AirConditioner, col = "WaterHeater_AirConditioner")) +
  geom_line(aes(y = Sum_newDF$Global_active_power, col = "Global_active_power")) +
  geom_line(aes(y = Sum_newDF$Global_reactive_power, col = "Global_reactive_power")) +
  geom_line(aes(y = Sum_newDF$Global_intensity, col = "Global_intensity")) +
  geom_point((aes(y= Sum_newDF$WaterHeater_AirConditioner, col = "WaterHeater_AirConditioner"))) +
  geom_point((aes(y= Sum_newDF$Global_active_power, col = "Global_active_power"))) +
  geom_point((aes(y= Sum_newDF$Global_reactive_power, col = "Global_reactive_power"))) +
  geom_point((aes(y= Sum_newDF$Global_intensity, col = "Global_intensity"))) +
  scale_x_continuous(breaks = c(1:7), labels = c(1:7), limits = c(1,7)) +
  ylab("Energy consumtion")


# Plot per quarter:

Sum_newDF <- newDF %>% group_by(quarter) %>% summarise( Kitchen = sum(Kitchen),
                                                        Laundry = sum(Laundry),
                                                        WaterHeater_AirConditioner = sum(WaterHeater_AirConditioner),
                                                        Global_active_power = sum(Global_active_power),
                                                        Global_reactive_power = sum(Global_reactive_power),
                                                        Global_intensity = sum(Global_intensity)) 


ggplot(Sum_newDF, aes(x = quarter)) +
  geom_line(aes(y = Sum_newDF$Kitchen, col = "Kitchen", group = 1)) + geom_point((aes(y= Sum_newDF$Kitchen, col = "Kitchen"))) +
  geom_line(aes(y = Sum_newDF$Laundry, col = "Laundry", group = 1)) + geom_point((aes(y= Sum_newDF$Laundry, col = "Laundry"))) +
  geom_line(aes(y = Sum_newDF$WaterHeater_AirConditioner, col = "WaterHeater_AirConditioner", group = 1)) +
  geom_line(aes(y = Sum_newDF$Global_active_power, col = "Global_active_power", group = 1)) +
  geom_line(aes(y = Sum_newDF$Global_reactive_power, col = "Global_reactive_power", group = 1)) +
  geom_line(aes(y = Sum_newDF$Global_intensity, col = "Global_intensity", group = 1)) +
  geom_point((aes(y= Sum_newDF$WaterHeater_AirConditioner, col = "WaterHeater_AirConditioner"))) +
  geom_point((aes(y= Sum_newDF$Global_active_power, col = "Global_active_power"))) +
  geom_point((aes(y= Sum_newDF$Global_reactive_power, col = "Global_reactive_power"))) +
  geom_point((aes(y= Sum_newDF$Global_intensity, col = "Global_intensity"))) +
  ylab("Energy consumtion")



# Plot per quarter of excess energy:

Sum_newDF <- newDF %>% group_by(quarter) %>% summarise( excess_energy = sum(excess_energy)) 


ggplot(Sum_newDF, aes(x = quarter)) +
  geom_line(aes(y = Sum_newDF$excess_energy, col = "excess_energy", group = 1)) + geom_point((aes(y= Sum_newDF$excess_energy, col = "excess_energy"))) +
  ylab("Energy consumtion")


newDF <- read.csv("combined_DF.csv")



###############################################################################
# Data Conversion ---------------------------------------------------------

newDF$Global_active_power <- newDF$Global_active_power / 60 #converting from kW/min to kW/h

newDF$Global_reactive_power <- newDF$Global_reactive_power / 60 #converting from kW/min to kW/h

newDF$Kitchen <- newDF$Kitchen / 1000 #converting from W/hour to kW/hour

newDF$Laundry <- newDF$Laundry / 1000 #converting from W/hour to kW/hour

newDF$WaterHeater_AirConditioner <- newDF$WaterHeater_AirConditioner / 1000 #converting from W/hour to kW/hour


#Energy not measured by submeters:
newDF$excess_energy <- newDF$Global_active_power - newDF$Kitchen - newDF$Laundry - newDF$WaterHeater_AirConditioner


#Pricing for peak and offpeak:
newDF$price <- c(NA)

newDF$price <- ifelse (between(newDF$hour,left = 2, right = 7) | between(newDF$hour, left = 14, right = 17),
                       yes = 0.123, no = 0.158)

newDF$GAP_cost <- newDF$Global_active_power * newDF$price
newDF$Kitchen_cost <- newDF$Kitchen * newDF$price
newDF$Laundry_cost <- newDF$Laundry * newDF$price
newDF$WH_AC_cost <- newDF$WaterHeater_AirConditioner * newDF$price

###############################################################################
# Removing/filling NAs ----------------------------------------------------

#Finding gaps in data and filling them with NA:
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
attr(newDF$DateTime, "tzone") <- "Europe/Paris"

#Deleting X, X.1, Date, Time columns in dataframe:
newDF$X.1 <- NULL
newDF$X <- NULL
newDF$Date <- NULL
newDF$Time <- NULL

newDF <- pad(newDF, break_above = 3)

#Filling in id column:
newDF$id <- c(1:nrow(newDF))


#Filling NA values with mode of the attribute column:
for (i in 3:ncol(newDF)) {
  newDF[which(is.na(newDF[,i])),i] <- Mode(newDF[,i])
}


###############################################################################
# Granularity -------------------------------------------------------------
newDF$Date <- date(newDF$DateTime)

newDF$DayYear <- floor_date(newDF$Date, unit = "day")
newDF$MonthYear <- floor_date(newDF$Date, unit = "month")
newDF$WeekYear <- floor_date(newDF$Date, unit = "week")

group <- c('DayYear', 'WeekYear', 'MonthYear')

granularity <- c()

for (i in group){
  granularity[[i]] <- newDF %>% group_by_at(i) %>% summarise(Kitchen = sum(Kitchen),
                                                          Laundry = sum(Laundry),
                                                          WH_AC = sum(WaterHeater_AirConditioner),
                                                          GAP = sum(Global_active_power),
                                                          DateTime = Mode(DateTime)[1])
  granularity[[i]]$Date <- date(granularity[[i]]$DateTime)
  
}


#write.csv(house070809month, "house070809month.csv")

#write.csv(house070809week, "house070809week.csv")

#write.csv(house070809day, "house070809day.csv")

names(granularity$DayYear)[1] <- "ds"
names(granularity$DayYear)[5] <- "y"
names(granularity$WeekYear)[1] <- "ds"
names(granularity$WeekYear)[5] <- "y"
names(granularity$MonthYear)[1] <- "ds"
names(granularity$MonthYear)[5] <- "y"

###############################################################################
# MODELS/Performance: -----------------------------------------------------------

# PROPHET:
m <- lapply(granularity, prophet)
future <- lapply(m, function(x) make_future_dataframe(m = x, period = 365))
forecast <- map2(m, future, predict)

plots <- map2(m, forecast, dyplot.prophet)
plots$DayYear
plots$WeekYear
plots$MonthYear

prophet.cv <- cross_validation(m$DayYear, initial = 730, period = 180, horizon = 365, units = 'days')
head(prophet.cv)
prophet.perf <- performance_metrics(prophet.cv)
mean(prophet.perf$mape)


