#PREPARE
#Used for most statistical functions
install.packages("tidyverse")
library("tidyverse")
#Used for data cleaning
install.packages("janitor")
library("janitor")
#used for filtering,arranging,mutating,summarizing and grouping data
install.packages("dplyr")
library("dplyr")
#The scales packages provides the internal scaling infrastructure used by ggplot2, and gives you tools to override the default breaks, 
#labels, transformations and palettes.
install.packages("scales")
library(scales)
#geosphere is an R package for spherical trigonmetry. That is compute distance, direction, area, and related quantities with longitude / latitude data
install.packages("geosphere")
library("geosphere")
#The readr package makes it easy to get rectangular data out of comma separated (csv), tab separated (tsv) or fixed width files (fwf) and into R
install.packages("readr")
library("readr")
#The goal of the forcats package is to provide a suite of tools that solve common problems with factors, including changing the order of levels or the values.
install.packages("forcats")
library("forcats")
#Enables working easier with date and time
install.packages("lubridate")
library("lubridate")
#loading data
df<-read.csv("D:/Tripdata/202203-divvy-tripdata/202203-divvy-tripdata.csv")
View(df)
structure(df)
glimpse(df)
install.packages("skimr")
library("skimr")
skim_without_charts(df)
#importing data in R
Mar23<-read_csv("C:/Trip_data/202302-divvy-tripdata.csv")
Mar23$ended_at <- as.character(Mar23$ended_at)
Mar23$started_at <- as.character(Mar23$started_at)
Feb23<-read_csv("C:/Trip_data/202301-divvy-tripdata.csv")
Feb23$ended_at <- as.character(Feb23$ended_at)
Feb23$started_at <- as.character(Feb23$started_at)
Jan23<-read_csv("C:/Trip_data/202212-divvy-tripdata.csv")
Jan23$ended_at <- as.character(Jan23$ended_at)
Jan23$started_at <- as.character(Jan23$started_at)
Dec22<-read_csv("C:/Trip_data/202211-divvy-tripdata.csv")
Dec22$ended_at <- as.character(Dec22$ended_at)
Dec22$started_at <- as.character(Dec22$started_at)
Nov22<-read_csv("C:/Trip_data/202210-divvy-tripdata.csv")
Nov22$ended_at <- as.character(Nov22$ended_at)
Nov22$started_at <- as.character(Nov22$started_at)
Oct22<-read_csv("C:/Trip_data/202209-divvy-publictripdata.csv")
Oct22$ended_at <- as.character(Oct22$ended_at)
Oct22$started_at <- as.character(Oct22$started_at)
Sep22<-read_csv("C:/Trip_data/202208-divvy-tripdata.csv")
Sep22$ended_at <- as.character(Sep22$ended_at)
Sep22$started_at <- as.character(Sep22$started_at)
#convert data to character inorder to merge

Aug22<-read_csv("C:/Trip_data/202207-divvy-tripdata.csv")
Aug22$ended_at <- as.character(Aug22$ended_at)
Aug22$started_at <- as.character(Aug22$started_at)
Jul22<-read_csv("C:/Trip_data/202206-divvy-tripdata.csv")
Jul22$ended_at <- as.character(Jul22$ended_at)
Jul22$started_at <- as.character(Jul22$started_at)
Jun22<-read_csv("C:/Trip_data/202205-divvy-tripdata.csv")
Jun22$ended_at <- as.character(Jun22$ended_at)
Jun22$started_at <- as.character(Jun22$started_at)

May22<-read_csv("C:/Trip_data/202204-divvy-tripdata.csv")
May22$ended_at <- as.character(May22$ended_at)
May22$started_at <- as.character(May22$started_at)
Apr22<-read_csv("C:/Trip_data/202203-divvy-tripdata.csv")

Apr22$ended_at <- as.character(Apr22$ended_at)
Apr22$started_at <- as.character(Apr22$started_at)
trip_data<-bind_rows(May22,Apr22,Jun22,Jul22,Sep22,Oct22,Nov22,Dec22,Jan23,Feb23,Mar23,Aug22)
View(trip_data)
#PROCESS
trip_data <- remove_empty(trip_data,which =c("rows"))
trip_data <- remove_empty(trip_data,which =c("cols"))
dim(trip_data)
skim_without_charts(trip_data)
colnames(trip_data)
trip_data %>% count(end_station_name)
View(trip_data)
#omiting na values
omit_trip_data<-na.omit(trip_data)
skim_without_charts(omit_trip_data)
View(omit_trip_data)
sapply(omit_trip_data,function(x) sum(is.na(x)))
#Data is partially clean now its doesnot contain any missing value but it maybe contain duplicates Removing Duplicates
omit_trip_data %>% distinct(ride_id,.keep_all= TRUE)
#change structure of start date and end date
str(omit_trip_data$day)
omit_trip_data$started_at <- ymd_hms(omit_trip_data$started_at)
omit_trip_data$ended_at <- ymd_hms(omit_trip_data$ended_at)

#add columns for date,month,day of the week in the data frame
omit_trip_data$date<-as.Date(omit_trip_data$started_at)
omit_trip_data$month<-format(as.Date(omit_trip_data$date),"%y-%m")
omit_trip_data$day<-format(as.Date(omit_trip_data$date),"%d")
omit_trip_data$day_of_week<-format(as.Date(omit_trip_data$date),"%A")
omit_trip_data$day<-format(as.Date(omit_trip_data$date),"%d")
View(omit_trip_data)
omit_trip_data$ride_duration<-difftime(omit_trip_data$ended_at,omit_trip_data$started_at,units="auto")
#calculate ride distance
omit_trip_data$ride_distance <- distGeo(matrix(c(omit_trip_data$start_lng, omit_trip_data$start_lat), ncol=2), matrix (c(omit_trip_data$end_lng, omit_trip_data$end_lat), ncol=2))/1000

ridedata <- filter(omit_trip_data, ride_distance > 0) %>% filter(ride_duration>0) 
ridedata$rideable_type<-str_replace_all(ridedata$rideable_type,"docket_bike","electrical_bike")
View(ridedata)
count(filter(trip_data,member_casual=="casual"))
#casual 2365120 members
count(filter(trip_data,member_casual=="member"))
#annual 3463964 members
View(p)
#Analyze
ridedata_summary <- ridedata %>% group_by(member_casual) %>%
  summarize(ride_count=length(ride_id),
  average_ride_duration=mean(ride_duration),
  median_duration=median(ride_duration),
  max_ride_duration=max(ride_duration),
  min_ride_duration=min(ride_duration),
  average_ride_distance=mean(ride_distance),
  median_distance=median(ride_distance),
  max_ride_distance=max(ride_distance),
  min_ride_distance=min(ride_distance))
View(ridedata_summary)
ridedata_cat <- ridedata %>% group_by(member_casual,month) %>%
  summarize(ride_count=length(ride_id),
            average_ride_duration=mean(ride_duration),
            median_duration=median(ride_duration),
            max_ride_duration=max(ride_duration),
            min_ride_duration=min(ride_duration),
            average_ride_distance=mean(ride_distance),
            median_distance=median(ride_distance),
            max_ride_distance=max(ride_distance),
            min_ride_distance=min(ride_distance))
view(ridedata_cat)
#by user type and week
ridedata_cat2 <- ridedata %>% 
  group_by(member_casual,day_of_week) %>%
  summarize(ride_count = length(ride_id), average_ride_duration = mean(ride_duration), median_duration = median(ride_duration), 
            max_ride_duration = max(ride_duration), min_ride_duration = min(ride_duration),
            average_ride_distance = mean(ride_distance), median_distance = median(ride_distance), 
            max_ride_distance = max(ride_distance), min_ride_distance = min(ride_distance))
ridedata_cat2$day_of_week <- factor(ridedata_cat2$day_of_week,
                                    levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ridedata_cat2a <- ridedata_cat2[order(ridedata_cat2$day_of_week), ]
view(ridedata_cat2a)
#group  by rideable type
ridedata_cat3<- ridedata %>% 
  group_by(member_casual,rideable_type) %>%
  summarize(ride_count = length(ride_id), average_ride_duration = mean(ride_duration), median_duration = median(ride_duration), 
            max_ride_duration = max(ride_duration), min_ride_duration = min(ride_duration),
            average_ride_distance = mean(ride_distance), median_distance = median(ride_distance), 
            max_ride_distance = max(ride_distance), min_ride_distance = min(ride_distance))
View(ridedata_cat3)

#plot chart
#ride duration by month
ggplot(ridedata_cat)+geom_col(mapping=aes(x=month,y=average_ride_duration,fill=member_casual),position="dodge")+labs(x="Month",y="Average ride length(secs)",title="Ride length by month",fill="User Type")
#ride count vs month
ggplot(ridedata_cat)+geom_col(mapping=aes(x=month,y=ride_count,fill=member_casual),position="dodge")+labs(x="Month",y="Number of rides",title="Number of rides by month",fill="User type")
#ride length vs the day of the week
ggplot(ridedata_cat2a)+geom_col(mapping=aes(x=day_of_week,y=average_ride_duration,fill=member_casual),position="dodge")+labs(x="Day of the week",y="Average ride duration",title="Ride length by day",fill="User type")
#ride count vs the day of the week
ggplot(ridedata_cat2a)+
  geom_col(mapping = aes(x = day_of_week, y = ride_count,fill = member_casual),position = "dodge")+
  labs(x="Day of the week",y="Number of rides",title="Number of rides by day",fill="User type")
#ride type vs the day of the week
ggplot(ridedata_cat3)+
  geom_col(mapping = aes(x = rideable_type, y = ride_count,fill = member_casual),position = "dodge")+
  labs(x="Rideable type",y="Number of rides",title="Number of rides by rideable type",fill="User type")

