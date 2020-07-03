#My Edits to Claire's script, with my file locations, etc
#If all files are not in the same folder as R project...
Water<-data.frame(read.csv("C:\\Users\\nicangie\\Documents\\Ang\\Projects_and_Samples\\SOSTPRO\\Sostpro_2018_Temp\\HoboLoggers\\HoboLoggers\\Coastal\\6Coast_Water.csv",
                           header=F,stringsAsFactors=FALSE,
                           colClasses=c(rep("character",4),rep("NULL",5)),
                           na.string=c("","null","NaN","X")))

library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(knitr)

#This works if all .csv files are in same folder as R project. 
#11Coast Sites: B1,B2,B3,B4,B5,C1,C3,C4,R1,R2,R3. 
#11Interior sites: B1,B2,B3,B4,B5,B6,C1,C2,C3,R1,R2.
#When processing add Water = Water[,-c(5,6,7)], to remove extra columns

###Read in data and get rows and columns sorted, change the file name and csv output manually
site.name <- "B1Coast"

#Water
Water<-data.frame(read.csv("B1Coast_Water.csv",
                           header=F,stringsAsFactors=FALSE,
                           colClasses=c(rep("character",4),rep("NULL",5)),
                           na.string=c("","null","NaN","X")))
colnames(Water) <- c("Record", "DateTime", "Temp.C", "Light.Lux")
Water = Water[-c(1,2),]


Water$DateTime <- strptime(Water$DateTime, "%m/%d/%y %I:%M:%S %p")
Water$DateTime <- as.POSIXct(Water$DateTime, origin="1970-01-01", tz="GMT")
Water$Temp.C <- as.numeric(as.character(Water$Temp.C))
Water$Light.Lux <- as.numeric(as.character(Water$Light.Lux))

#Subset by deployment dates
dates <-read.csv("LoggerDates2019.csv")
dates$Start <- strptime(dates$Start, "%m/%d/%Y")
dates$End <- strptime(dates$End, "%m/%d/%Y")
dates$Start <- as.POSIXct(dates$Start, origin="1970-01-01", tz="GMT")
dates$End <- as.POSIXct(dates$End, origin="1970-01-01", tz="GMT")

####Temporarily skipping
#site.date <- dates %>%
 # filter(Site == site.name) %>%
 # head
#int <- interval(site.date$Start, site.date$End)
#Water <- Water[Water$DateTime %within% int,]
####

#Replace light values = 0 (night time) with NA
Water$Light.Lux[Water$Light.Lux==0] <- NA

#Get summary of data
summary(Water)

#By day - show range in hourly variables by day.Function 'cut' divides X into intervals
Water$Day <- cut(Water$DateTime, breaks = "day")

###### Calculate daily max temp
#original ex:WaterMean<-aggregate(cbind(Temp.C, Light.Lux)~Day, Water, mean)

Daily<- group_by(Water,Day) %>% 
  summarize(DayT_Mean = mean(Temp.C),
            DayT_Max = max(Temp.C))

## Add Running means to df (zoo pckg) John asked for 7 days. Gives 38 values
Daily$Tmax_7daymean<-rollmean(Daily$DayT_Max, 7,na.pad = T, align="right") #align =center gives NAs in first 3 and last 3 values, align left=, at the end and so on.

#Subset by new logger-overlapping dates:
#First convert Day to a Date again
Daily$Day <- as.Date(Daily$Day, format = "%Y-%m-%d")

#Ran this
Daily <- Daily %>%
  filter(Day >= as.Date('2019-06-11') & Day <= as.Date('2019-07-03')) #note these intervals are for COAST only

#Add a column of site name, just in case for later merging
Daily$Site<-site.name

#Save as file
setwd("C:\\Users\\nicangie\\Documents\\Ang\\Projects_and_Samples\\SOSTPRO\\2019_Data\\Sostpro_2019_Temp\\DailyTMaxOutput")
write.csv(Daily, "B1Coast_TMax.csv",row.names = F)
#Remember to set working directory back!
setwd("C:\\Users\\nicangie\\Documents\\Ang\\Projects_and_Samples\\SOSTPRO\\2019_Data\\Sostpro_2019_Temp")

#Repeat lines 15-82 for other sites just adjusting file name.


####################### END   ###################

###################################################################
#####################################    INTERIOR STARTS HERE   
###################################################################
###################################################################

#### For interior Sites, make sure change subsetting dates:
#Continue here, solving subsetting issues this way or creating new Date file with the correct interval
Daily <- Daily %>%
  filter(Day >= as.Date('2019-05-29') & Day <= as.Date('2019-06-24')) #