# Loading the necessary libraries in order to work with the data

library(tidyverse)
library(lubridate)
library(readr)
library(data.table)
library(janitor)
#----------------------------------------------------------------------------------------------------------

# Passing through all the csv files for the months between 2022 through 2023 using the read csv function in R library readr

tripdata_2022_01 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202201-divvy-tripdata\\202201-divvy-tripdata.csv")
tripdata_2022_02 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202202-divvy-tripdata.zip")
tripdata_2022_03 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202203-divvy-tripdata.zip")
tripdata_2022_04 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202204-divvy-tripdata.zip")
tripdata_2022_05 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202205-divvy-tripdata.zip")
tripdata_2022_06 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202206-divvy-tripdata.zip")
tripdata_2022_07 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202207-divvy-tripdata.zip")
tripdata_2022_08 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202208-divvy-tripdata.zip")
tripdata_2022_09 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202209-divvy-tripdata.zip")
tripdata_2022_10 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202210-divvy-tripdata.zip")
tripdata_2022_11 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202211-divvy-tripdata.zip")
tripdata_2022_12 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202212-divvy-tripdata.zip")
tripdata_2023_01 <- read_csv("C:\\Users\\nihaa\\OneDrive\\Jupyter Notebook Files\\Projects\\202301-divvy-tripdata.zip")

#---------------------------------------------------------------------------------------------------------------------------
# Combining all the data sets above into one data frame using bind_rows function

all_trips <- bind_rows(tripdata_2022_01,tripdata_2022_02,tripdata_2022_03,tripdata_2022_04,tripdata_2022_05,tripdata_2022_06,tripdata_2022_07,tripdata_2022_08,tripdata_2022_09,tripdata_2022_10,tripdata_2022_11,tripdata_2022_12,tripdata_2023_01)
#---------------------------------------------------------------------------------------------------------------------------
# Drop all null values in the newly joined data set 

all_trips_clean <- drop_na(all_trips)

#---------------------------------------------------------------------------------------------------------------------------
# Lets also remove any duplicates in our data set 

all_trips_clean <- unique(all_trips_clean)

# Lets rename column names so they clearly express what the data is
clean_names(all_trips_clean)
colnames(all_trips_clean)

#---------------------------------------------------------------------------------------------------------------------------

# Now lets separate the dates into year, month, week, and day of the week 
all_trips_clean$date <- as.Date(all_trips_clean$started_at)
all_trips_clean$month <- format(as.Date(all_trips_clean$started_at),"%m")
all_trips_clean$day <-format(as.Date(all_trips_clean$started_at),"%d")
all_trips_clean$year <-format(as.Date(all_trips_clean$started_at),"%Y")
all_trips_clean$day_of_week <-format(as.Date(all_trips_clean$started_at),"%A")

glimpse(all_trips_clean)

#---------------------------------------------------------------------------------------------------------------------------

# Lets calculate some valuable information from the data given which includes the distance traveled and the speed of rides

install.packages("geosphere")
library(geosphere)
all_trips_clean$Distance_Traveled <- distHaversine(all_trips_clean[,c("start_lng","start_lat")],
                                                   all_trips_clean[,c("end_lng","end_lat")])

all_trips_clean$Ride_Length <- difftime(all_trips_clean$ended_at,all_trips_clean$started_at)

# Here I am showing only the trips where the length of the ride was > 0 since there were some entries that were negative

all_trips_clean <- all_trips_clean[all_trips_clean$Ride_Length > 0, ]

#---------------------------------------------------------------------------------------------------------------------------
# Now lets transform data for meaningful insights!
#---------------------------------------------------------------------------------------------------------------------------

# I want to show the number of rides taken 
Subscription_Type_Means <- all_trips_clean %>% 
  mutate(weekday = factor(weekdays(date),levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  group_by(member_casual,weekday) %>%
  arrange(member_casual,weekday) %>% 
  summarize(num_of_rides = n(),average_dist = mean(Distance_Traveled),average_time = mean(Ride_Length))
View(Subscription_Type_Means) 
ggplot(data= Subscription_Type_Means)+
  geom_col(mapping= aes(x= weekday,y= num_of_rides,fill = member_casual), position = "dodge")+
  labs(title = "Number of Rides by Day of Week and User Type",
       x = "Day of Week",
       y = "Number of Rides")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
#---------------------------------------------------------------------------------------------------------------------------
# I want to show the monthly  average distance traveled
monthly_distance<- all_trips_clean %>%
  mutate(month_name = month.name[month(date)])%>% 
  group_by(member_casual,month_name) %>%
  arrange(member_casual,month_name) %>% 
  summarize(num_of_rides = n(),average_dist = mean(Distance_Traveled),average_time = mean(Ride_Length))

ggplot(data= monthly_distance)+
  geom_col(mapping= aes(x= factor(month_name,levels=month.name),y= average_dist,fill = member_casual), position = "dodge")+
  labs(title = "Average Distance of Rides by Month and User Type",
       x = "Month", y = "Average Distance")+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
#---------------------------------------------------------------------------------------------------------------------------
# I want to show now the average time spent on the bike riding
monthly_time<- all_trips_clean %>%
  mutate(month_name = month.name[month(date)])%>% 
  group_by(member_casual,month_name) %>%
  arrange(member_casual,month_name) %>% 
  summarize(num_of_rides = n(),average_dist = mean(Distance_Traveled),average_time = mean(Ride_Length))

ggplot(data= monthly_distance)+
  geom_col(mapping= aes(x= factor(month_name,levels=month.name),y= average_time,fill = member_casual), position = "dodge")+
  labs(title = "Average Time of Rides by Month and User Type",
       x = "Month", y = "Average Time in Seconds")+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
#--------------------------------------------------------------------------------------------------------------------------
# I also want to know which type of bikes are casual and annual members using
bike_rides <- all_trips_clean %>% 
  group_by(member_casual, rideable_type) %>% 
  count() %>% 
  ungroup()

ggplot(bike_rides, aes(x = member_casual, y = n, fill = rideable_type)) + 
  geom_col(position = "dodge") +
  labs(title = "Type of Bike Used by Member",
       x = "Member Type", y = "Number of Rides",
       fill = "Rideable Type")
#--------------------------------------------------------------------------------------------------------------------------

