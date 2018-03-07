setwd("/Users/yuhuansu/Downloads")
df = read.csv('LeadsFull_Verified_CleanV3.csv')

#summary statistics
dim(df)
head(df)
summary(df)

#map plotting - United States
install.packages('maps')
library(ggplot2)
library(maps)
#subset the geographic dataframe and correct the mistake in data
library('dplyr')
geography <- df %>% select(Longitude, Latitude) %>% filter(Longitude> -130 & Latitude<50)

#plotting the United States map and the datapoints
us <- map_data("state")
mapplot2 <- ggplot() +geom_polygon(data=us, aes(x=long, y=lat, group=group), color="white")
mapplot2
mapplot2 <- mapplot2 + geom_point(data=geography, aes(x=geography$Longitude, y=geography$Latitude), color="skyblue") 
mapplot2



