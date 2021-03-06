#############
## 
## Winter Travel Data Collection Initiative
## Zack Treisman
## 12/7/20
## modified 1/11/21
##
#############


# Load libraries
library(tidyverse)
library(lubridate)
library(car)
library(MASS)
library(gam)

# Load trailhead data

# Changes to spreadsheets: remove spaces from directory names, Zach -> Zack,  convert to csv, simplify names, remove summaries and other items at bottom of some files
# Use read.csv instead of read_csv because Non-motorized has that dash and read_csv makes that a quoted string instead of converting it to a dot. Probably not worth the trouble to fix in the spreadsheets.

# Snow bike to motorized: for 17-18 there were no changes for Brush Creek TH, Gothic, Slate, or Snodgrass. For 18-19 there were no changes to Brush Creek TH or Slate. 


brush17 <- read.csv("Data_for_Zack/2017-2018/brush.csv")
brush_rd17 <- read.csv("Data_for_Zack/2017-2018/brush_rd.csv")
cement17 <- read.csv("Data_for_Zack/2017-2018/cement.csv")
gothic17 <- read.csv("Data_for_Zack/2017-2018/gothic.csv")
kebler17 <- read.csv("Data_for_Zack/2017-2018/kebler.csv")
slate17 <- read.csv("Data_for_Zack/2017-2018/slate.csv")
snodgrass17 <- read.csv("Data_for_Zack/2017-2018/snodgrass.csv")
washington17 <- read.csv("Data_for_Zack/2017-2018/washington.csv")

brush17$Hybrid <- NA

trailheads1718 <- bind_rows(list(brush17, brush_rd17,cement17, gothic17, kebler17, slate17, snodgrass17, washington17))
trailheads1718$Date <- parse_date_time(trailheads1718$Date, orders = c("ymd", "mdy"))
trailheads1718 <- trailheads1718 %>% rename(date = Date)




brush18 <- read.csv("Data_for_Zack/2018-2019/brush.csv")
cement18 <- read.csv("Data_for_Zack/2018-2019/cement.csv")
gothic18 <- read.csv("Data_for_Zack/2018-2019/gothic.csv")
kebler18 <- read.csv("Data_for_Zack/2018-2019/kebler.csv")
slate18 <- read.csv("Data_for_Zack/2018-2019/slate.csv")
snodgrass18 <- read.csv("Data_for_Zack/2018-2019/snodgrass.csv")
washington18 <- read.csv("Data_for_Zack/2018-2019/washington.csv")
trailheads1819 <- bind_rows(list(brush18,cement18, gothic18, kebler18, slate18, snodgrass18, washington18))
trailheads1819$Date <- parse_date_time(trailheads1819$Date, orders = c("ymd", "mdy"))
trailheads1819 <- trailheads1819 %>% rename(date = Date)



brush19 <- read.csv("Data_for_Zack/2019-2020/brush.csv")
cement19 <- read.csv("Data_for_Zack/2019-2020/cement.csv")
gothic19 <- read.csv("Data_for_Zack/2019-2020/gothic.csv")
kebler19 <- read.csv("Data_for_Zack/2019-2020/kebler.csv")
slate19 <- read.csv("Data_for_Zack/2019-2020/slate.csv")
snodgrass19 <- read.csv("Data_for_Zack/2019-2020/snodgrass.csv")
washington19 <- read.csv("Data_for_Zack/2019-2020/washington.csv")

kebler19$Hybrid<-NA
slate19$Hybrid<-NA
snodgrass19$Hybrid<-NA
snodgrass19$Motorized<-NA

trailheads1920 <- bind_rows(list(brush19, cement19, gothic19, kebler19, slate19, snodgrass19, washington19))
trailheads1920$Date <- parse_date_time(trailheads1920$Date, orders = c("ymd", "mdy"))
trailheads1920 <- trailheads1920 %>% rename(date = Date)


## Load CAIC data

# Add. changes: change spaces to underscores, date to date_time

caic17 <- read_csv("Data_for_Zack/CAIC_Data_2017-2020/caic2017.csv")
caic17 <- caic17[-1,] # No other data for 2017-12-21
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
snotel17 <- snotel17[-1,] # No other data for 2017-12-21
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

winter1718 <- list(trailheads1718, caic17, snotel17) %>% reduce(full_join, by="date")
winter1819 <- list(trailheads1819, caic18, snotel18) %>% reduce(full_join, by="date")
winter1920 <- list(trailheads1920, caic19, snotel19) %>% reduce(full_join, by="date")

## Join all years

winter1718$year <- "w1718"
winter1819$year <- "w1819"
winter1920$year <- "w1920"

winter_travel <- bind_rows(list(winter1718,winter1819,winter1920))
winter_travel$year <- factor(winter_travel$year)

## Fix some capitalization issues 

winter_travel <- winter_travel %>% mutate(Non.motorized = coalesce(Non.Motorized, Non.motorized))
winter_travel$Non.Motorized <- NULL

winter_travel[winter_travel$Trailhead=="Slate River","Trailhead"] <- "Slate River Rd"

## Pivot to long format

winter_travel <- pivot_longer(winter_travel, 
                           c("Non.motorized", "Mechanized", "Motorized", "Hybrid"), 
                           names_to = "modality", values_to = "user.count")
winter_travel$modality <- factor(winter_travel$modality)


## Add some variables

winter_travel$week <- factor(wday(winter_travel$date), labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
winter_travel$weekend <- factor(ifelse(winter_travel$week %in% c("Sat", "Sun"), "weekend", "weekday"))

## Summaries to check reasonableness

summary(winter_travel)

## Save the data to csv

write.csv(winter_travel, file = "data/winter_travel.csv")

#  1. How do trail visitor counts correlate to an increase in snowfall during the previous day(s)? 
#     (what patterns of snowfall yield the most visitors/changes in visitation)

ggplot(winter_travel, aes(change_depth, user.count, color = Trailhead))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.5)+
  facet_grid(modality~year)

ggplot(winter_travel, aes(change_depth, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()+
  facet_grid(modality~Trailhead)

ggplot(winter_travel, aes(past3snow, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()+
  facet_grid(modality~Trailhead)

ggplot(winter_travel, aes(past5snow, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()+
  facet_grid(modality~Trailhead)

glm1 <- glm(user.count ~ modality*Trailhead+change_depth+past2snow+past3snow+past5snow, family = poisson, data=winter_travel)
Anova(glm1, test.statistic = "LR")
summary(glm1)
hist(resid(glm1))

coef(summary(glm1))[c(12, 15),]
confint(glm1)[c(12, 15),]
# Looks like about a 1%-2% decrease in users per cm of new snow, and an additional 0.4%-0.9% decrease per cm of new snow in the past 5 days.


#  2. How does an increased avalanche risk impact the numbers of hybrid and motorized visitors to specific trailheads?

ggplot(na.omit(winter_travel), aes(rating_near, user.count, color = Trailhead, fill = Trailhead))+
  geom_point(position=position_jitterdodge(jitter.width = 0.2))+
  geom_boxplot(color="black", outlier.shape = NA)+
  scale_y_log10()+
  facet_wrap(vars(modality))


glm2 <- glm(user.count ~ modality*Trailhead*rating_near, family = poisson, data=winter_travel)
Anova(glm2, test.statistic = "LR")
summary(glm2)


#  3. What combination of weather conditions yields the highest/lowest number of visitors?

ggplot(winter_travel, aes(air_temp, past5snow, size = user.count, color = rating_near))+
  geom_jitter(height = 0.2, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))

ggplot(winter_travel, aes(air_temp, change_depth, size = user.count, color = rating_near))+
  geom_jitter(height = 0.2, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))

ggplot(winter_travel, aes(air_temp, past5snow, size = user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  facet_grid(modality~Trailhead)

glm3 <- glm(user.count~air_temp+change_depth+snow_depth, family = poisson, data=winter_travel)
summary(glm3)

# Looks like with new snow there are fewer users. About 1.8% per cm of new snow. Warmer air temps also bring people out, about 1% per degree.


#  4. What sample size is needed to represent the entire season ?



#  5. Which days of the week have the highest number of visitors? Weekdays vs weekends?

glm5 <- glm(user.count~weekend, family = poisson, data=winter_travel)
summary(glm5)

glm5a <- glm(user.count~week, family = poisson, data=winter_travel)
summary(glm5a)

# Saturdays are about 12.5% busier than Sundays, weekdays are about half as busy as weekends.

#  6. How do visitation rates for different user groups vary seasonally?

ggplot(winter_travel, aes(year, user.count))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  facet_grid(modality~Trailhead)+
  scale_y_log10()



#  7. Which days of the year are busiest?

day_totals <- winter_travel %>% 
  group_by(date) %>%
  summarise(users = sum(user.count, na.rm = TRUE), 
            week = unique(week)) %>%
  arrange(desc(users))

head(day_totals, 10)

ggplot(day_totals, aes(date, users))+
  geom_line()+
  geom_point(aes(color=week))


#  8. Do different groups of users utilize the trail more during different times of the season?


ggplot(winter_travel, aes(date, user.count+1, color = Trailhead))+
  geom_point()+
  geom_smooth()+
  scale_y_log10()+
  facet_wrap(vars(modality))

gam6 <- gam(user.count ~ modality*s(date), family = poisson, data=winter_travel)
summary(gam6)

modality_day_totals <- winter_travel %>% 
  group_by(date, modality) %>%
  summarise(users = sum(user.count, na.rm = TRUE), 
            week = unique(week)) %>%
  arrange(desc(users))

head(modality_day_totals, 10)

ggplot(modality_day_totals, aes(date, users, color = modality))+
  geom_line()+
  geom_smooth()+
  scale_y_log10()


#  9. How did covid affect visitation rates and use patterns?
#  10. What are major year-to-year trends in visitation?
#  11. How do the days with the highest avalanche risk affect visitor rates?

high_risk <- as.numeric(winter_travel$rating_above)+as.numeric(winter_travel$rating_near)+as.numeric(winter_travel$rating_below) > 9
summary(glm(user.count~high_risk, data=winter_travel, family = poisson))
