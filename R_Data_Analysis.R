#Installing the requid paclges
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')

#Calling the required packges
library(tidyverse)
library(janitor)
library(lubridate)

#Importing the raw data
January2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202401-divvy-tripdata.csv")
February2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202402-divvy-tripdata.csv")
March2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202403-divvy-tripdata.csv")
April2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202404-divvy-tripdata.csv")
May2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202405-divvy-tripdata.csv")
June2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202406-divvy-tripdata.csv")
July2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202407-divvy-tripdata.csv")
August2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202408-divvy-tripdata.csv")
September2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202409-divvy-tripdata.csv")
October2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202410-divvy-tripdata.csv")
November2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202411-divvy-tripdata.csv")
December2024 <-read.csv("/Users/yashbhatt/Documents/Case Study/Case Study CSV/202412-divvy-tripdata.csv")

# Viewing the data
str(January2024)
str(February2024)
str(March2024)
str(April2024)
str(May2024)
str(June2024)
str(July2024)
str(August2024)
str(September2024)
str(October2024)
str(November2024)
str(December2024)

#Merging all the monthly files to a combined CSV
merged_df <- rbind(January2024, February2024, March2024, April2024, May2024, June2024, July2024, August2024, September2024, October2024, November2024, December2024)

#Verifying the number of rows of the merged file to the number of rows of the individual files
rowtotal <- sum(
  nrow(January2024), 
  nrow(February2024), 
  nrow(March2024), 
  nrow(April2024), 
  nrow(May2024), 
  nrow(June2024), 
  nrow(July2024),
  nrow(August2024), 
  nrow(September2024),
  nrow(October2024),
  nrow(November2024),
  nrow(December2024))
print(rowtotal)
print(nrow(merged_df))

#Formatting the time from yyyy-dd-mm HH:MM:SS to the individual columns and adding a 'day_of_week' column
merged_df$date <-as.Date(merged_df$started_at)
merged_df$month <- format(as.Date(merged_df$date), "%b")
merged_df$day <-format(as.Date(merged_df$date), "%d")
merged_df$year <-format(as.Date(merged_df$date), "%Y")
merged_df$day_of_week <-format(as.Date(merged_df$date), "%A")
head(merged_df)

#Removing empty and duplicate rows
merged_df_NA_removed <- merged_df[merged_df$start_station_name != "" & merged_df$end_station_name != "", ]
merged_df_duplicates_removed <- merged_df_NA_removed[!duplicated(merged_df_NA_removed$ride_id), ]
print(paste("Removed", nrow(merged_df_NA_removed) - nrow(merged_df_duplicates_removed), "duplicate rows"))

#Creating a column to determine the ride length in minutes
merged_df_V2 <- mutate(merged_df_duplicates_removed, ride_length = difftime(ended_at, started_at, units = "mins"))
colnames(merged_df_V2)

#Filtering trips with ride length <0
nrow(merged_df_V2[merged_df_V2$ride_length < 0,])
merged_df_V3 <- merged_df_V2[!merged_df_V2$ride_length <0,]
glimpse(merged_df_V3)

#determining the number of members vs casual riders
rider_type_total <- table(merged_df_V3$member_casual)
View(rider_type_total)

#Performing statistical analysis
trip_stats <- merged_df_V3 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length), standard_deviation = sd(ride_length), median_ride_length = median(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))
head(trip_stats)

#creating a function to determine mode of week (learnt from: tutorialspoint.com)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
weekday_mode <- getmode(merged_df_V3$day_of_week)
print(weekday_mode)

#Determining the most popular day by type of rider
merged_df_V3$day_of_week <- ordered(merged_df_V3$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
merged_df_V3 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(rider_type_total = n(), average_ride_length = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)

#Determining the most popular month
popular_month <- merged_df_V3 %>% 
  group_by(month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(-number_of_rides)
View(popular_month)

#Finding the most popular station
station_mode <-getmode(merged_df_V3$start_station_name)
print(station_mode)

#Determining the most popular station for casual riders
popular_start_station_casual <- merged_df_V3 %>% 
  filter(member_casual == 'casual') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(-number_of_starts)
head(popular_start_station_casual)

#Determining the most popular station for members
popular_start_station_members <- merged_df_V3 %>% 
  filter(member_casual == 'member') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(-number_of_starts)
head(popular_start_station_members)

#Saving the final version as csv
getwd()
setwd("/Users/yashbhatt/Documents/Case Study/")
write_csv(merged_df_V3, "R Data Analysis\\tripdata_analysis_2020.csv")

#Visualization of rider types
merged_df_V3 %>% 
  group_by(member_casual) %>% 
  summarise(total_rider_type= n()) %>% 
  ggplot(aes(x = member_casual, y = total_rider_type, fill = member_casual)) + geom_col(position = "dodge") + geom_text(aes(label = total_rider_type, vjust = -0.25))

#Visualization of the rider types ride duration
rider_type_average_duration <- merged_df_V3 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))

rider_type_average_duration %>% 
  ggplot(aes(x = member_casual, y = average_ride_length, fill = member_casual)) + geom_col(position = "dodge") + geom_text(aes(label = average_ride_length, vjust = -0.25))

#Visualization of the usage by member and casual riders by the weekday
merged_df_V3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_riders = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill= member_casual)) + geom_col(position = "dodge")

#Visualization of the number of trips by members and casual riders bt the weekday
merged_df_V3 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")

#Visualization of the usage by member and casual riders by the months
merged_df_V3$month <- ordered(merged_df_V3$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

merged_df_V3 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length) ) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides, angle = 90)) +
  facet_wrap(~member_casual)

#Visualization of number of trips by the rider type by month
merged_df_V3 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides, angle = 90)) +
  facet_wrap(~member_casual)
