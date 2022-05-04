library(tidyverse)
library(here)
library(dplyr)
library(janitor)
library(lubridate)
library(geosphere)
glimpse(tripdata)

# Cleaning database

clean_data <- tripdata %>%
  clean_names() %>%  #clean name of the columns
  drop_na() %>% #drop NULL values
  separate(ended_at, c('end_date', 'end_time'), sep =' ')%>% # separate date and time in two columns
  separate(started_at, c('start_date', 'start_time'), sep =' ')

clean_data$start_date <- as.Date(clean_data$start_date, format = "%Y-%m-%d") #convert type from character to date
clean_data$start_time <- parse_time(clean_data$start_time, format="%H:%M:%S") #convert type from character to time
clean_data$end_date <- as.Date(clean_data$end_date, format = "%Y-%m-%d") #convert type from character to date
clean_data$end_time <- parse_time(clean_data$end_time, format="%H:%M:%S") #convert type from character to time

head(clean_data)

# Arrange data by date and time
clean_data <- clean_data %>%
  arrange(start_date, start_time, end_date, end_time)

# add day of the week
clean_data<- mutate(clean_data, day_start = wday(start_date, label = TRUE, abbr = FALSE), .after = start_date)

#separating members and casual users
member_tibble <- clean_data %>% filter(member_casual== "member")
casual_tibble <- clean_data %>% filter(member_casual== "casual")


#calculate the amount of time for casual users
int_date_cas <- interval(casual_tibble$start_date, casual_tibble$end_date)
int_hours_cas <- interval(casual_tibble$start_time, casual_tibble$end_time)
duration <- time_length(as.duration(int_date_cas), unit = "minutes") +
  time_length(as.duration(int_hours_cas), unit = "minutes")

#create a new column for duration in casual_tibble
casual_tibble<- mutate(casual_tibble, duration_min = time_length(as.duration(int_date_cas), unit = "minutes") + 
           time_length(as.duration(int_hours_cas), unit = "minutes"), .after = end_time)

# some of the values are negative and this is wrong
casual_tibble <- subset(casual_tibble, duration_min>0)

# calculate the distance between start and end points for casual_tibble
slon <- as.vector(casual_tibble$start_lng)
slat <- as.vector(casual_tibble$start_lat)
elon <- as.vector(casual_tibble$end_lng)
elat <- as.vector(casual_tibble$end_lat)
distance <- c()
i=1
for(i in  1:67150){
  start_point <- c(slon[i], slat[i])
  end_point <- c(elon[i], elat[i])
  point_mat <- matrix(c(end_point, start_point), ncol =2 )
  geospatial_dist <- as.integer(distGeo(start_point, end_point, a=6378137, f=1/298.257223563)) #calculate distance as meter
  distance <- append(distance, geospatial_dist)
  i=i+1
}

casual_tibble <- mutate(casual_tibble, distance_m = distance, .after = end_lng)
#View(casual_tibble)

#calculate the amount of time for members
int_date_mem <- interval(member_tibble$start_date, member_tibble$end_date)
int_hours_mem <- interval(member_tibble$start_time, member_tibble$end_time)
duration_m <- time_length(as.duration(int_date_mem), unit = "minutes") +
  time_length(as.duration(int_hours_mem), unit = "minutes") + duration

#create a new column for duration in member_tibble
member_tibble<- mutate(member_tibble, duration_min = time_length(as.duration(int_date_mem), unit = "minutes") + 
                         time_length(as.duration(int_hours_mem), unit = "minutes"), .after = end_time)


# some of the values are negative and this is wrong
casual_tibble <- subset(casual_tibble, duration_min>0)


#summarize data related to duration in casual and member table
casualsummary_date <- casual_tibble %>%
  group_by(start_date) %>%
  summarise(sum(duration_min),max(duration_min) ,min(duration_min) , mean(duration_min), median(duration_min))

membersummary_date <- member_tibble %>%
  group_by(start_date) %>%
  summarise(sum(duration_min),max(duration_min) ,min(duration_min) ,mean(duration_min), median(duration_min))

casualsummary_wday <- casual_tibble %>%
  group_by(day_start) %>%
  summarise(sum(duration_min),max(duration_min) ,min(duration_min) , mean(duration_min), median(duration_min))

membersummary_wday <-member_tibble %>%
  group_by(day_start) %>%
  summarise(sum(duration_min),max(duration_min) ,min(duration_min) , mean(duration_min), median(duration_min))


