## PREPARE

install.packages("tidyverse")
install.packages("janitor")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("forcats")
install.packages("lubridate")
install.packages("tinytex")
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(lubridate)
library(tinytex)

##PROCESS

daily_activity <- read_csv("C:/Users/tamon/Desktop/Google Analytics Course/Case Study/Dataset/dailyActivity_merged.csv")
sleep_day <- read_csv("C:/Users/tamon/Desktop/Google Analytics Course/Case Study/Dataset/sleepDay_merged.csv")

head(daily_activity)

## Cleaning & Formatting

daily_activity %>%
  duplicated() %>%
  sum()

sleep_day %>%
  duplicated() %>%
  sum()

## Remove duplicate rows
sleep_day <- sleep_day %>%
  distinct() %>%
  drop_na()

sleep_day %>%
  duplicated() %>%
  sum()


## Checking distinct IDs

n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)

## Make all columns lower case
daily_activity <- rename_with(daily_activity, tolower)
sleep_day <- rename_with(sleep_day,tolower)

## make all columns as date for ease of merge

daily_activity <- daily_activity %>%
  rename(date = activitydate)
head(daily_activity)

sleep_day <- sleep_day %>%
  rename(date = sleepday)
head(sleep_day)

daily_activity <- daily_activity %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
view(daily_activity)

sleep_day <- sleep_day %>%
  mutate(date = as.Date(date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
view(sleep_day)


##steps categories
stepcategories <- daily_activity %>% 
  group_by(id) %>% 
  summarise(avg_step= mean(totalsteps)) %>% 
  mutate (active_type=case_when (
    avg_step <5000 ~ "Sedentary",
    avg_step >=5000 & avg_step< 8000 ~"Lightly Active",
    avg_step>=8000 & avg_step <12000~"Active",
    avg_step >=12000 ~ 'Highly Active'))
view(stepcategories)

ggplot(data = stepcategories)+
  geom_bar(mapping = aes(x=active_type,fill=active_type))+
  labs(title = "Categories Of Average Steps")

##step_daily_active
step_daily_activity <- daily_activity %>%
  group_by(id) %>%
  summarize(days_step = n_distinct(date))
head(step_daily_activity)

daily_step_by_category <- step_daily_activity %>%
  mutate(device_usage = case_when(
    days_step <=15 ~ 'Low Usage',
    days_step >15 & days_step <= 25 ~ 'Normal Usage',
    days_step >25 ~ 'High Usage',
    TRUE~'Otherwise'))

p <- ggplot(data = daily_step_by_category)+
  geom_bar(mapping = aes(x = device_usage, fill=device_usage))+
  labs(title = "Device Usage for Steps")
p

##Usage_sleep_day
usage_sleep_day <- sleep_day %>%
  group_by(id) %>%
  summarize(days_sleep = n_distinct(date))
head(usage_sleep_day)

daily_sleep_by_category <- usage_sleep_day %>%
  mutate(device_usage_sleep = case_when(
    days_sleep <=15 ~ 'Low Usage',
    days_sleep >15 & days_sleep <= 25 ~ 'Normal Usage',
    days_sleep >25 ~ 'High Usage',
    TRUE~'Otherwise'))

q <- ggplot(data = daily_sleep_by_category)+
  geom_bar(mapping = aes(x = device_usage_sleep, fill=device_usage_sleep))+
  labs(title = "Device Usage for Sleep")
q

ggarrange(p,q)

##Total Steps and total calories burned

a<-ggplot(data = daily_activity)+
  geom_point(mapping = aes(x = totalsteps, y = calories),color="blue")+
  geom_smooth(mapping = aes(x = totalsteps, y = calories),color="red")+
  labs(title = "Total Daily Steps vs Calories Burned",x="Total Steps", y="Calories Burned")
a

##Total active minutes and total calories burned

daily_activity_new <-daily_activity %>% 
  mutate(total_active_minutes= veryactiveminutes+fairlyactiveminutes+lightlyactiveminutes)

b<-ggplot(data = daily_activity_new)+
  geom_point(mapping = aes(x = total_active_minutes, y = calories),color="purple")+
  geom_smooth(mapping = aes(x = total_active_minutes, y = calories),color="black")+
  labs(title = "Total Active Minutes vs Calories Burned",x="Total Active Minutes", y="Calories Burned")
b

ggarrange(a,b)

## Below Normal Sleep Hours

avg_minutes_asleep <- sleep_day %>%
  group_by(id) %>%
  summarize(avg_asleep = mean(totalminutesasleep))


avg_minutes_asleep1 <- avg_minutes_asleep %>%
  mutate(sleep_per_day = case_when(
    avg_asleep <=420 ~ 'Below Normal Sleep Hours',
    avg_asleep >421 & avg_asleep <= 480 ~ 'Normal Sleep Hours',
    avg_asleep >480 ~ 'Above Normal Hours',
    TRUE~'Otherwise'))

ggplot(data = avg_minutes_asleep1)+
  geom_bar(mapping = aes(x = sleep_per_day, fill=sleep_per_day))