##install and load packages##
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(skimr)
library(janitor)

##I loaded the data i felt would be helpful, The 6 datasets were given names that will make it easy to use.##
activity <- read.csv("~/Bellabeat/archive (2)/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
calories <- read.csv("~/Bellabeat/archive (2)/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
intensities <- read.csv("~/Bellabeat/archive (2)/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
##heartrate <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")##
sleepDay <- read.csv("~/Bellabeat/archive (2)/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight <- read.csv("~/Bellabeat/archive (2)/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

##Converted date columns to so keep things consisent and easy to use.##
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
glimpse(activity)
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
glimpse(activity)
activity$ActivityDate=as.Date(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date=as.Date(activity$date, format="%m/%d/%Y")
glimpse(activity)
glimpse(intensities)
intensities$ActivityDay=as.Date(intensities$ActivityDay, format="%m/%d/%y", tz=Sys.timezone())
sleepDay$SleepDay=as.POSIXct(sleepDay$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleepDay$date <- format(sleepDay$SleepDay, format = "%m/%d/%y")
sleepDay$date=as.Date(sleepDay$date, "%m/%d/%y")
glimpse(sleepDay)
## Did a search for duplicate data, sleepDay was the only data set with double entries, ##
## I removed the 3 rows and renamed the dataset sleepDay1##
## The heartrate dataset kept crashing Rcloud, did not check for dupicates##
sleepDay <- sleepDay[!duplicated(sleepDay),]
view(sleepDay)
## installed and loaded the "mice" package##
#started to check for missing information##
install.packages("mice")
library(mice)
view(weight)
md.pattern(weight) 
weight$Fat = NULL
md.pattern(activity)
md.pattern(calories)
md.pattern(intensities)
md.pattern(sleepDay)  
md.pattern(weight)  
## No more missing data##
view(activity)
## I use the Summarise function along with n_distinct to get the number of rows under "ID" column ##
## for every dataset##
activity %>%
  summarise(activity_participants = n_distinct(activity$Id))
calories %>% 
  summarise(calories_participants = n_distinct(calories$Id))
intensities %>% 
  summarise(intensities_participants = n_distinct(intensities$Id))
sleepDay %>% 
  summarise(sleepDay_participants = n_distinct(sleepDay$Id))
weight %>% 
  summarise(weight_participants = n_distinct(weight$Id))

activity %>% 
  select(TotalSteps, TotalDistance, SedentaryMinutes, Calories) %>% 
  summary()
calories$Calories %>% 
  summary()
intensities %>% 
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>% 
  summary()
sleepDay %>% 
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% 
  summary()
(458.5-419.2)/60 ### sleepDay info tells us that the mean time spent in bed awake is .65 of an hour)
Combined_data_outer <- merge(sleepDay, activity, by="Id", all = TRUE)
n_distinct(Combined_data_outer$Id)
View(Combined_data_outer)

##chart shows sedentary vs total steps,##
##i have added annotatios that  break down how many participants are not meeting the Mayo clinic recomendations of 10,000 steps.##
##i have also chosen colors that match the BellaBeats  theme.##
ggplot(data = activity, aes(x=TotalSteps, y=SedentaryMinutes)) +  geom_point(color="#CC6699") + geom_smooth(color="pink") + 
  geom_vline(xintercept = 10000, linetype="dashed", color="lightblue", size=1.5) +
  labs(title="Sedentary Minutes vs Total Steps", size=1, caption = "Mayo clinic recomends 10,000+ a day") + 
  annotate("label", x=5000,y=200, label= "637/940 do not meet recommendations, 68%", size=4, fill="gray") + 
  annotate("label", x=18000, y=200, label = "303/940 exceed recommendations, 32%", size=4, fill="#66CC99") +
  theme(plot.title = element_text(size=25,hjust=0.5))
ggsave("sedvstep.jpg")
ggplot(data=sleepDay, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point(color="#FF3399") + geom_smooth(color="pink") + 
  geom_vline(xintercept = 420, linetype="dashed", color="lightblue", size=1.5) +
  labs(title="Minutes Asleep Vs Time In Bed") + theme(plot.title = element_text(size=25,hjust=0.5)) +
  annotate("label", x=400,y=150, label ="CDC recommends 7hrs (420minutes) of sleep for adults over the age of 18", size=4, fill=("#99CCFF"))
View(sleepDay %>% 
  summarise(Id,TotalMinutesAsleep, TotalTimeInBed))
colnames(sleepDay)
sleepDay %>% 
  sleepHR <- sleepDay$TotalMinutesAsleep/60
view(sleepHR)
mean(sleepHR) * 60
sleepDay$TotalMinutesAsleep >= 420
head(sleepDay$TotalMinutesAsleep)
sum(sleepDay$TotalMinutesAsleep >= 420)
sum(sleepDay$TotalMinutesAsleep <= 420)
### 411 recorded, 229 get less than recommended 420, 182 meet it.#####
ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + geom_point(color="#FF3399") + 
  geom_smooth(color="pink") + labs(title="Total Steps vs. Calories") +
  theme(plot.title = element_text(size=25,hjust=0.5))
colnames(activity)
intensities$ActiveIntensity <- (intensities$VeryActiveMinutes)/60
view(intensities$ActiveIntensity)
##i combined weight and intensities by ID, i then changed the date format to Hours, Minutes, seconds##
combined_data <- merge(weight, intensities, by="Id", all=TRUE)
combined_data$time <- format(combined_data$Date, format = "%H:%M:%S")
ggplot(data=combined_data, aes(x=time, y=ActiveIntensity)) +
  geom_histogram(stat = "identity", fill='darkblue') + 
  theme(axis.text.x =element_text(angle=90)) + labs(title="Total Very Active Intensity vs. Time")
colnames(activity)