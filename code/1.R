install.packages("data.table")
library(data.table)
library(ggplot2)
library(dplyr)

setwd("C:/Users/haochen shi/OneDrive/Desktop/Built enviroment/cod")

lcl1=read.csv("C:/Users/haochen shi/OneDrive/Desktop/Built enviroment/archive (3)/Power-Networks-LCL-June2015(withAcornGps)v2_2.csv")

head(lcl1)

lcl1=na.omit(lcl1)

ggplot(lcl1, aes(x=DateTime,y=KWH.hh..per.half.hour.))+geom_point()

sampled_df <- sample_frac(lcl1, 0.01)
ggplot(sampled_df, aes(x=DateTime,y=KWH.hh..per.half.hour.))+geom_point()

library(dplyr)
library(lubridate)

# Assuming your data is in a data.frame called 'df'
df$DateTime <- ymd_hms(lcl1$DateTime) # convert to Date-Time object, ensure your format matches "2014-02-28 00:00:00"
df$Date <- as.Date(lcl1$DateTime) # extract date
daily_kwh <- df %>%
  group_by(LCLid, Date) %>%
  summarize(DailyKWH = sum(KWH.hh.per.half.hour., na.rm = TRUE))

# View the aggregated daily data
print(daily_kwh)
