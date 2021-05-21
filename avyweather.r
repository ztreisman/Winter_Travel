
# Load libraries
library(tidyverse)
library(lubridate)
library(car)
library(MASS)
library(gam)

## Load CAIC data

# Add. changes: change spaces to underscores, date to date_time

caic17 <- read_csv("Data_for_Zack/CAIC_Data_2017-2020/caic2017.csv")
caic18 <- read_csv("Data_for_Zack/CAIC_Data_2017-2020/caic2018.csv")
caic19 <- read_csv("Data_for_Zack/CAIC_Data_2017-2020/caic2019.csv")

caic17$date <- date(caic17$date_time)
caic17 <- caic17 %>% group_by(date) %>%
  summarise(rating_above = unique(rating_above),
            rating_near = unique(rating_near),
            rating_below = unique(rating_below)) %>%
  mutate(rating_above = factor(rating_above, levels = 1:4),
         rating_near = factor(rating_near, levels = 1:4),
         rating_below =  factor(rating_below, levels = 1:4))
caic18$date <- date(caic18$date_time)
caic18 <- caic18 %>% group_by(date) %>%
  summarise(rating_above = unique(rating_above),
            rating_near = unique(rating_near),
            rating_below = unique(rating_below)) %>%
  mutate(rating_above = factor(rating_above, levels = 1:4),
         rating_near = factor(rating_near, levels = 1:4),
         rating_below =  factor(rating_below, levels = 1:4))
caic19$date <- date(caic19$date_time)
caic19 <- caic19 %>% group_by(date) %>%
  summarise(rating_above = unique(rating_above),
            rating_near = unique(rating_near),
            rating_below = unique(rating_below)) %>%
  mutate(rating_above = factor(rating_above, levels = 1:4),
         rating_near = factor(rating_near, levels = 1:4),
         rating_below =  factor(rating_below, levels = 1:4))


## Load SNOTEL data

# Add. changes: simplify headers

snotel17 <- read_csv("Data_for_Zack/SNOTEL_2017-2020/snotel1718.csv", skip = 7)
snotel18 <- read_csv("Data_for_Zack/SNOTEL_2017-2020/snotel1819.csv", skip = 7)
snotel19 <- read_csv("Data_for_Zack/SNOTEL_2017-2020/snotel1920.csv", skip = 7)

## Create lag variables for snowfall/ melt

snotel17 <- snotel17 %>% 
  mutate(lag1snow = lag(change_depth, default = 0),
         lag2snow = lag(change_depth, n=2, default = 0),
         lag3snow = lag(change_depth, n=3, default = 0),
         lag4snow = lag(change_depth, n=4, default = 0),
         past2snow = change_depth+lag1snow,
         past3snow = past2snow+lag2snow,
         past4snow = past3snow+lag3snow,
         past5snow = past4snow+lag4snow)
snotel18 <- snotel18 %>% 
  mutate(lag1snow = lag(change_depth, default = 0),
         lag2snow = lag(change_depth, n=2, default = 0),
         lag3snow = lag(change_depth, n=3, default = 0),
         lag4snow = lag(change_depth, n=4, default = 0),
         past2snow = change_depth+lag1snow,
         past3snow = past2snow+lag2snow,
         past4snow = past3snow+lag3snow,
         past5snow = past4snow+lag4snow)
snotel19 <- snotel19 %>% 
  mutate(lag1snow = lag(change_depth, default = 0),
         lag2snow = lag(change_depth, n=2, default = 0),
         lag3snow = lag(change_depth, n=3, default = 0),
         lag4snow = lag(change_depth, n=4, default = 0),
         past2snow = change_depth+lag1snow,
         past3snow = past2snow+lag2snow,
         past4snow = past3snow+lag3snow,
         past5snow = past4snow+lag4snow)


## Merge by year

avyweather1718 <- list(caic17, snotel17) %>% reduce(full_join, by="date")
avyweather1718$year <- "y1718"
avyweather1819 <- list(caic18, snotel18) %>% reduce(full_join, by="date")
avyweather1819$year <- "y1819"
avyweather1920 <- list(caic19, snotel19) %>% reduce(full_join, by="date")
avyweather1920$year <- "y1920"

## combine years

avyweather <- bind_rows(list(avyweather1718,avyweather1819,avyweather1920))
avyweather$year <- factor(avyweather$year)

## Add some variables

avyweather$week <- factor(wday(avyweather$date), labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
avyweather$weekend <- factor(ifelse(avyweather$week %in% c("Sat", "Sun"), "weekend", "weekday"))

