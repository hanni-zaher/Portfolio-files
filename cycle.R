#installing and loading packages 
install.packages("tidyverse")                 
library(tidyverse)
library(conflicted)
install.packages("xlsx")
library("xlsx")

conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")


install.packages("readxl")
library("readxl")


#reading combined excel sheet into R 
all_trips<- excel_sheets("year_to_date.xlsx") %>% 
  map_df(~read_xlsx("year_to_date.xlsx",.))


all_trips <- rename(all_trips, day_of_week = day_of_the_week)

#checking colunmns and data types
View(all_trips)
head(all_trips$ride_length)
tail(all_trips)
nrow(all_trips)
dim(all_trips)
str(all_trips)

#removing empty columns
all_trips <- subset(all_trips , select = -c(16,17)) 


#creating day, month and year columns
all_trips$date <-as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")


#formatting weekdays to weekday names instead of 1-7
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")               
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)



min(all_trips$ride_length)
#converting ride length to numeric for calculations
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#removing negative values and headquarter trips by the company
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),] 

#removing columns that wont be used for analysis
all_trips_v2 <- subset(all_trips , select = -c(start_lat,end_lat,start_lng,end_lng,start_station_id,end_station_id, end_station_name)) 
#remove missing values 
all_trips_v2 <- na.omit(all_trips_v2)
View(all_trips_v2)


#min,max,median, and mean values for ride length
summary(all_trips_v2$ride_length) 

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean) 
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)  
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#organizing days in order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

rides_by_usetype <-all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>% #creates weekday feild
  group_by(member_casual,weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% #calculates the number of rides and average duration 
  arrange(member_casual,weekday) %>% #sorts data 
  filter(!is.na(weekday)) %>% #filter out missing data
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge")
  
average_ride <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  filter(!is.na(weekday)) %>% #filter out missing data
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

print(rides_by_usetype)
print(average_ride)

#creating a table to find the busiest stations for casual riders
stations <- all_trips_v2 %>% 
  select(member_casual, start_station_name) %>% 
  filter(member_casual == "casual") %>% 
  count(start_station_name)

print(stations)

#exporting a spreadsheet for year to date spreadsheet and stations names for casual members for further visualization and analysis

write.csv(all_trips_v2,file = 'year_to_date_finished.csv' ) #exporting data to csv format 
write_csv(stations, file = 'casual_stations.csv') #saving stations to target for casual members to identify opportunity

View(all_trips_v2)

                                    
#exporting a spreadsheet for average ride lengths for further visualization and analysis
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv') #saving number of trip per day to a csv file  



 