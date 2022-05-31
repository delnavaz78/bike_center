library(tidyverse)
library(here)
library(dplyr)
library(janitor)
library(lubridate)
library(geosphere)
library(ggbeeswarm)
library(ggplot2)
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

#calculate the amount of time for members--------------------------------------------------------------------------
int_date_mem <- interval(member_tibble$start_date, member_tibble$end_date)
int_hours_mem <- interval(member_tibble$start_time, member_tibble$end_time)
duration_m <- time_length(as.duration(int_date_mem), unit = "minutes") +
  time_length(as.duration(int_hours_mem), unit = "minutes") + duration

#create a new column for duration in member_tibble
member_tibble<- mutate(member_tibble, duration_min = time_length(as.duration(int_date_mem), unit = "minutes") + 
                         time_length(as.duration(int_hours_mem), unit = "minutes"), .after = end_time)


# some of the values are negative and this is wrong
member_tibble <- subset(member_tibble, duration_min>0)


# calculate the distance between start and end points for member_tibble
slon <- as.vector(member_tibble$start_lng)
slat <- as.vector(member_tibble$start_lat)
elon <- as.vector(member_tibble$end_lng)
elat <- as.vector(member_tibble$end_lat)
distance <- c()
i=1
for(i in  1:nrow(member_tibble)){
  start_point <- c(slon[i], slat[i])
  end_point <- c(elon[i], elat[i])
  geospatial_dist <- as.integer(distGeo(start_point, end_point, a=6378137, f=1/298.257223563)) #calculate distance as meter
  distance <- append(distance, geospatial_dist)
  i=i+1
}

member_tibble <- mutate(member_tibble, distance_m = distance, .after = end_lng)


#plot of the outcomes

casual_tibble%>%
  ggplot(mapping = aes(x=start_date, y=duration_min))+
  geom_point()

casual_tibble%>%
  ggplot(mapping = aes(x=start_date, y=distance_m))+
  geom_point()

member_tibble%>%
  ggplot(mapping = aes(x=start_date, y=duration_min))+
  geom_point()

member_tibble%>%
  ggplot(mapping = aes(x=start_date, y=distance_m))+
  geom_point()

#there are some outliers and we should deal with them

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

casual_tibble <- remove_outliers(casual_tibble, c('duration_min', 'distance_m'))

member_tibble <- remove_outliers(member_tibble, c('duration_min', 'distance_m'))

#summarize data related to duration in casual and member table-------------------------------------------------------
casualsummary_date <- casual_tibble %>%
  group_by(start_date) %>%
  summarise(sum_dur = sum(duration_min),max_dur=max(duration_min) ,min_dur=min(duration_min) , mean_dur=mean(duration_min), med_dur=median(duration_min),
            sum_dis=sum(distance_m), max_dis=max(distance_m), min_dis=min(distance_m), mean_dis=mean(distance_m), med_dis=median(distance_m))

membersummary_date <- member_tibble %>%
  group_by(start_date) %>%
  summarise(sum_dur = sum(duration_min),max_dur=max(duration_min) ,min_dur=min(duration_min) , mean_dur=mean(duration_min), med_dur=median(duration_min),
            sum_dis=sum(distance_m), max_dis=max(distance_m), min_dis=min(distance_m), mean_dis=mean(distance_m), med_dis=median(distance_m))

casualsummary_wday <- casual_tibble %>%
  group_by(day_start) %>%
  summarise(sum_dur = sum(duration_min),max_dur=max(duration_min) ,min_dur=min(duration_min) , mean_dur=mean(duration_min), med_dur=median(duration_min),
            sum_dis=sum(distance_m), max_dis=max(distance_m), min_dis=min(distance_m), mean_dis=mean(distance_m), med_dis=median(distance_m))

membersummary_wday <-member_tibble %>%
  group_by(day_start) %>%
  summarise(sum_dur = sum(duration_min),max_dur=max(duration_min) ,min_dur=min(duration_min) , mean_dur=mean(duration_min), med_dur=median(duration_min),
            sum_dis=sum(distance_m), max_dis=max(distance_m), min_dis=min(distance_m), mean_dis=mean(distance_m), med_dis=median(distance_m))

#Visualization of outcomes-------------------------------------------------------------------------------------

g1 <- ggplot(casualsummary_date, mapping = aes(x=start_date))
g1 <- g1 + geom_line(casualsummary_date,mapping = aes(y=mean_dur), colour= "red") 
g1 <- g1 + geom_line(membersummary_date,mapping = aes(y=mean_dur), colour= "blue") 
g1 <- g1 + labs(title="mean of duration in members and casual users", y=" ")
g1 <- g1 + annotate(geom="text",x=as.Date("2022-03-29"), y=14, label= "casual", colour="red")
g1 <- g1 + annotate(geom="text",x=as.Date("2022-03-29"), y=9, label= "member", colour="blue")
g1

g2 <- ggplot(casualsummary_date, mapping = aes(x=start_date))
g2 <- g2 + geom_line(casualsummary_date,mapping = aes(y=med_dur), colour= "red") 
g2 <- g2 + geom_line(membersummary_date,mapping = aes(y=med_dur), colour= "blue") 
g2 <- g2 + labs(title="median of duration in members and casual users", y=" ")
g2 <- g2 + annotate(geom="text",x=as.Date("2022-03-29"), y=12, label= "casual", colour="red")
g2 <- g2 + annotate(geom="text",x=as.Date("2022-03-29"), y=8, label= "member", colour="blue")
g2


g3 <- ggplot(casualsummary_date, mapping = aes(x=start_date))
g3 <- g3 + geom_line(casualsummary_date,mapping = aes(y=mean_dis), colour= "red") 
g3 <- g3 + geom_line(membersummary_date,mapping = aes(y=mean_dis), colour= "blue") 
g3 <- g3 + labs(title="mean of distance in members and casual users", y=" ")
g3 <- g3 + annotate(geom="text",x=as.Date("2022-03-29"), y=1800, label= "casual", colour="red")
g3 <- g3 + annotate(geom="text",x=as.Date("2022-03-29"), y=1600, label= "member", colour="blue")
g3

g4 <- ggplot(casualsummary_date, mapping = aes(x=start_date))
g4 <- g4 + geom_line(casualsummary_date,mapping = aes(y=med_dis), colour= "red") 
g4 <- g4 + geom_line(membersummary_date,mapping = aes(y=med_dis), colour= "blue") 
g4 <- g4 + labs(title="median of distance in members and casual users", y=" ")
g4 <- g4 + annotate(geom="text",x=as.Date("2022-03-29"), y=1600, label= "casual", colour="red")
g4 <- g4 + annotate(geom="text",x=as.Date("2022-03-29"), y=1350, label= "member", colour="blue")
g4

