install.packages("RMySQL")
library(RMySQL)

###############################################################################
## EXAMPLE WORKFLOW:
#Connecting the database: 
con = dbConnect(MySQL(),
                user = 'deepAnalytics',
                password = 'Sqltask1234!',
                dbname = 'dataanalytics2018',
                host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')



dbListTables(con) #list tables contained in the database

dbListFields(con,'iris') #list attributes contained in the table

irisALL <- dbGetQuery(con, "SELECT * FROM iris") #quering the whole databse

irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

###############################################################################

## TASK:

dbListTables(con)

dbListFields(con, 'yr_2006')

# Creating dataframes per year:
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

###############################################################################

## Analysing each dataframe:

# 2006: (not a full year)
str(yr_2006)
summary(yr_2006)
head(yr_2006) # Start date/time are 2006-12-16 17:24:00
tail(yr_2006) # End date/time are 2006-12-31 23:59:00

# 2007: (full year)
str(yr_2007)
summary(yr_2007)
head(yr_2007) # Start date/time are 2007-01-01 00:00:00
tail(yr_2007) # End date/time are 2007-12-31 23:59:00

# 2008: (full year)
str(yr_2008)
summary(yr_2008)
head(yr_2008) # Start date/time are 2008-01-01 00:00:00
tail(yr_2008) # End date/time are 2008-12-31 23:59:00

# 2009: (full year)
str(yr_2009)
summary(yr_2009)
head(yr_2009) # Start date/time are 2009-01-01 00:00:00
tail(yr_2009) # End date/time are 2009-12-31 23:59:00

# 2010: (not a full year)
str(yr_2010)
summary(yr_2010)
head(yr_2010) # Start date/time are 2010-01-01 00:00:00
tail(yr_2010) # End date/time are 2010-11-26 21:02:00 


print('hi')


