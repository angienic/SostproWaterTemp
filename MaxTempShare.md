---
title: "Sostpro Max. Temp."
author: "A.Nicolas"
date: "3/2/2020"
output: html_document
---




```{ load Coast data, include=TRUE}

Coast_Tmax<-read.csv("C:\\Users\\nicangie\\Documents\\Ang\\Projects_and_ Samples\\SOSTPRO\\Sostpro_2018_Temp\\Manual_merge_7dmax_coast.csv",
                      na.string=c("","null","NaN","X"))
Coast_Tmax$Day <- as.Date(Coast_Tmax$Day, format = "%m-%d-%Y")
Coast_Tmax$Tmax_7daymean<- as.numeric(as.character(Coast_Tmax$Tmax_7daymean)) #Warning: To convert factors to numeric or integer, first convert to character
str(Coast_Tmax)
#organize for plotting
Coast_Tmax$Type <- factor(Coast_Tmax$Type,levels = c("Reference", "Buffer", "Clearcut"))
```



## Maximum daily temperatures 2018 
#### Coast
![plot of chunk boxplots](figure/boxplots-1.png)![plot of chunk boxplots](figure/boxplots-2.png)

```
## Warning: Removed 24 rows containing non-finite values (stat_boxplot).
```

![plot of chunk boxplots](figure/boxplots-3.png)

#### Interior



