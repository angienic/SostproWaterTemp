library(tidyverse)

##########################################    2019     ##########################################
TBoth2019<-data.frame(read.csv("C:\\Users\\nicangie\\Documents\\Ang\\Projects_and_Samples\\SOSTPRO\\WaterTempAll\\SostproWaterTemp\\Manual_merge_7dmax_Both2019.csv"))
#Worked! I <3 <3 <3 plyr!!! 
Tmean<-data.frame(
TBoth2019 %>%
  group_by(Site) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE))

Tsd<-data.frame(
  TBoth2019 %>%
    group_by(Site) %>%
    summarise_if(is.numeric, sd, na.rm = TRUE))

T_all2019<-bind_cols(Tmean, Tsd)

#renaming newname=oldname
T_all2019 %>% 
  rename(
    MeanTemp_mean=DayT_Mean,  MaxT_mean =DayT_Max,   Roll7dTmax_mean=Tmax_7daymean, MeanTemp_sd=DayT_Mean1,MaxT_sd=DayT_Max1,Roll7dTmax_sd=Tmax_7daymean1
    )
#reorder (to be variable_mean, variable_sd for each col) 
T_all2019ordered<-data.frame(T_all2019[,c(1,2,6,3,7,4,8)])
#rename (worked in tibble but didn't save??)
#T_all2019ordered %>% 
 # rename(MeanTemp_mean=DayT_Mean, MeanTemp_sd=DayT_Mean1, MaxT_mean =DayT_Max, MaxT_sd=DayT_Max1,Roll7dTmax_mean=Tmax_7daymean, Roll7dTmax_sd=Tmax_7daymean1)

# Save with right names
colnames(T_all2019ordered) <- c("Site", "MeanTemp_mean","MeanTemp_sd", "MaxT_mean","MaxT_sd","Roll7dTmax_mean","Roll7dTmax_sd")
write.csv(T_all2019ordered, file="Temp2019forCompiledcorrect.csv")

##########################################    2018     ##########################################
TBoth2018<-data.frame(read.csv("C:\\Users\\nicangie\\Documents\\Ang\\Projects_and_Samples\\SOSTPRO\\WaterTempAll\\SostproWaterTemp\\Manual_merge_7dmax_Both2018.csv"))
Tmean2018<-data.frame(
  TBoth2018 %>%
  group_by(Site) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE))

Tsd2018<-data.frame(
  TBoth2018 %>%
    group_by(Site) %>%
    summarise_if(is.numeric, sd, na.rm = TRUE))

T_all2018<-bind_cols(Tmean2018, Tsd2018)

#renaming newname=oldname
T_all2018 %>% 
  rename(
    MeanTemp_mean=DayT_Mean,  MaxT_mean = DayT_Max, Roll7dTmax_mean=Tmax_7daymean, MeanTemp_sd=DayT_Mean1,MaxT_sd=DayT_Max1,Roll7dTmax_sd=Tmax_7daymean1
    )
#reorder (to be: Variable_mean, Variable_sd for each col) and save 
write.csv(T_all2018[,c(1,2,6,3,7,4,8)], file="Temp2018forCompiled.csv")



