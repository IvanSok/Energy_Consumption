# Libraries: --------------------------------------------------------------
pacman::p_load(RMySQL, dplyr, lubridate, ggplot2, readr, plotly, ggfortify, forecast, padr, DescTools,
               stats, xts, prophet)
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

newDF$price <- ifelse (between(newDF$hour,left = 2, right = 7) | between(newDF$hour, left = 14, right = 17), yes = 0.123, no = 0.158)

###############################################################################
# Removing/filling NAs ----------------------------------------------------

#Finding gaps in data and filling them with NA:
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
attr(newDF$DateTime, "tzone") <- "Europe/Paris"
newDF <- pad(newDF, break_above = 2)

#Deleting X, X.1, Date, Time columns in dataframe:
newDF$X.1 <- NULL
newDF$X <- NULL
newDF$Date <- NULL
newDF$Time <- NULL

#Filling in id column:
newDF$id <- c(1:nrow(newDF))


#Filling NA values with mode of the attribute column:
for (i in 3:ncol(newDF)) {
  newDF[which(is.na(newDF[,i])),i] <- Mode(newDF[,i])
}


###############################################################################
# Granularity -------------------------------------------------------------

newDF$Date <- date(newDF$DateTime)


house070809week <- newDF %>% group_by(year,week) %>% summarise(Kitchen = sum(Kitchen),
                                                               Laundry = sum(Laundry),
                                                               WH_AC = sum(WaterHeater_AirConditioner),
                                                               GAP = sum(Global_active_power))

house070809day <- newDF %>% group_by(Date) %>% summarise(GAP = sum(Global_active_power))

names(house070809day)[1] <- "ds"
names(house070809day)[2] <- "y"
house070809day$ds <- as.Date.character(house070809day$ds)

###############################################################################
# Creating Time series & decomposition: -----------------------------------

# Data frame for calculations:

ddd <- house070809week

## Create TS object with Global active power:
tsGAP_070809 <- ts(ddd$GAP, frequency = 53, start = c(2007,1))
autoplot(tsGAP_070809, colour = "red", xlab = "Time", ylab = "Watt Hours",
         main = "Global_Active_Power")

fitGAP <- tslm(tsGAP_070809 ~trend + season)
summary(fitGAP)

forecastfitGAP <- forecast(fitGAP, h = 20)
plot(forecastfitGAP)

forecastfitGAP_c <- forecast(fitGAP, h = 20, level = c(80,90))
plot(forecastfitGAP_c,ylab = "Watt-Hours", xlab = "Time")


## Decomposing Time series:

components070809_GAP <- stl(tsGAP_070809,s.window = 'periodic' )
autoplot(components070809_GAP)

seasonal <- components070809_GAP$time.series[,1]
trend <- components070809_GAP$time.series[,2]
random <- components070809_GAP$time.series[,3]

plot(seasonal)
###############################################################################
# Splitting data ----------------------------------------------------------
train_set <- window(tsGAP_070809, start = 2007, end = c(2008,53))
test_set <- window(tsGAP_070809, start = 2009)

###############################################################################
# MODELS: -----------------------------------------------------------

#Holt-Winters:

# Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot:
tsGAP_070809Adjusted <- tsGAP_070809 - seasonal
autoplot(tsGAP_070809Adjusted)

# Train and test sets:
train_tsGAP_070809Adjusted <- window(tsGAP_070809Adjusted, end = c(2008,53))
test_tsGAP_070809Adjusted <- window(tsGAP_070809Adjusted, start = 2009)

HWMODEL <- HoltWinters(train_tsGAP_070809Adjusted,beta = FALSE,gamma = FALSE)
HW_forecast <- forecast(HWMODEL,h = 53)

holtacc <- accuracy(HW_forecast,test_tsGAP_070809Adjusted)

# HoltWinters forecast & plot:
ts_GAP_HW070809for <- forecast(ts_GAP_HW070809, h = 25)
plot(ts_GAP_HW070809for, ylab = "Watt-Hours", xlab = "Time - GAP")

# Forecast HoltWinters with diminished confidence levels
ts_GAP_HW070809forC <- forecast(ts_GAP_HW070809, h=25, level=c(10,25))

## Plot only the forecasted area
plot(ts_GAP_HW070809forC, ylab= "Watt-Hours", xlab="Time - GAP", start(2010))

# Absolute error:
absolute_error_HW <- sum(abs(random))/length(random)



# AUTOARIMA:
arimafit <- auto.arima(train_set)
arimaforecast2009 <- forecast(arimafit,h = 53)
arimaacc <- accuracy(f = arimaforecast2009,test_set)
plot(arimaforecast2009)


# PROPHET:
prophet <- prophet(house070809day, daily.seasonality = TRUE)
future <- make_future_dataframe(prophet, periods = 365)
forecast <- predict(prophet, future)
plot(prophet, forecast, pch = 24, cex = 3)
prophet_plot_components(prophet, forecast)

prophet.cv <- cross_validation(prophet, initial = 730, period = 180, horizon = 365, units = 'days')
head(prophet.cv)
prophet.perf <- performance_metrics(prophet.cv)
mean(prophet.perf$mape)
prophet.perf
#CrossValidation for other models:
crossvalidation <- c()
vector <- c(meanf,rwf,naive)
accuracycv <- NULL
for (i in vector) {
  crossvalidation <- tsCV(y = tsGAP_070809,h = 53,forecastfunction = i)
  accuracycv <- rbind(accuracycv,accuracy(crossvalidation,tsGAP_070809))
}
rownames(accuracycv) <- c("meanf","rwf","naive")

accuracycv
arimaacc
holtacc



