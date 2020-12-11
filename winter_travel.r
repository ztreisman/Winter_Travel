#############
## 
## Winter Travel Data Collection Initiative
## Zack Treisman
## 12/7/20
## modified 12/8/20
##
#############


# Load libraries
library(tidyverse)
library(lubridate)
library(car)
library(MASS)
library(gam)

# Load trailhead data

# Changes: remove spaces from directory names, h -> k,  convert to csv, simplify names, remove summaries at bottom of some files

brush17 <- read.csv("Data_for_Zack/2017-2018/brush.csv")
cement17 <- read.csv("Data_for_Zack/2017-2018/cement.csv")
gothic17 <- read.csv("Data_for_Zack/2017-2018/gothic.csv")
kebler17 <- read.csv("Data_for_Zack/2017-2018/kebler.csv")
slate17 <- read.csv("Data_for_Zack/2017-2018/slate.csv")
snodgrass17 <- read.csv("Data_for_Zack/2017-2018/snodgrass.csv")
washington17 <- read.csv("Data_for_Zack/2017-2018/washington.csv")
trailheads1718 <- bind_rows(list(brush17,cement17, gothic17, kebler17, slate17, snodgrass17, washington17))


brush18 <- read.csv("Data_for_Zack/2018-2019/brush.csv")
cement18 <- read.csv("Data_for_Zack/2018-2019/cement.csv")
gothic18 <- read.csv("Data_for_Zack/2018-2019/gothic.csv")
kebler18 <- read.csv("Data_for_Zack/2018-2019/kebler.csv")
slate18 <- read.csv("Data_for_Zack/2018-2019/slate.csv")
snodgrass18 <- read.csv("Data_for_Zack/2018-2019/snodgrass.csv")
washington18 <- read.csv("Data_for_Zack/2018-2019/washington.csv")
trailheads1819 <- bind_rows(list(brush18,cement18, gothic18, kebler18, slate18, snodgrass18, washington18))
trailheads1819$Date <- ymd(trailheads1819$Date)
trailheads1819 <- trailheads1819 %>% rename(date = Date)



brush19 <- read.csv("Data_for_Zack/2019-2020/brush.csv")
cement19 <- read.csv("Data_for_Zack/2019-2020/cement.csv")
gothic19 <- read.csv("Data_for_Zack/2019-2020/gothic.csv")
kebler19 <- read.csv("Data_for_Zack/2019-2020/kebler.csv")
slate19 <- read.csv("Data_for_Zack/2019-2020/slate.csv")
snodgrass19 <- read.csv("Data_for_Zack/2019-2020/snodgrass.csv")
washington19 <- read.csv("Data_for_Zack/2019-2020/washington.csv")
trailheads1920 <- bind_rows(list(brush19, cement19, gothic19, kebler19, slate19, snodgrass19, washington19))

## Load CAIC data

# Add. changes: remove spaces

caic17 <- read.csv("Data_for_Zack/CAIC_Data_2017-2020/caic2017.csv")
caic18 <- read.csv("Data_for_Zack/CAIC_Data_2017-2020/caic2018.csv")
caic19 <- read.csv("Data_for_Zack/CAIC_Data_2017-2020/caic2019.csv")

caic18$date <- date(caic18$date_time)
caic18 <- caic18 %>% group_by(date) %>%
  summarise(rating_above = unique(rating_above),
            rating_near = unique(rating_near),
            rating_below = unique(rating_below)) %>%
  mutate(rating_above = factor(rating_above, levels = 1:4),
         rating_near = factor(rating_near, levels = 1:4),
         rating_below =  factor(rating_below, levels = 1:4))


## Load SNOTEL data

# Add. changes: simplify headers

snotel17 <- read.csv("Data_for_Zack/SNOTEL_2017-2020/snotel1718.csv", skip = 6)
snotel18 <- read.csv("Data_for_Zack/SNOTEL_2017-2020/snotel1819.csv", skip = 7)
snotel19 <- read.csv("Data_for_Zack/SNOTEL_2017-2020/snotel1920.csv", skip = 6)

## Create lag variables for snowfall/ melt

snotel18 <- snotel18 %>% 
  mutate(lag1snow = lag(change_depth, default = 0),
         lag2snow = lag(change_depth, n=2, default = 0),
         lag3snow = lag(change_depth, n=3, default = 0),
         lag4snow = lag(change_depth, n=4, default = 0),
         past2snow = change_depth+lag1snow,
         past3snow = past2snow+lag2snow,
         past4snow = past3snow+lag3snow,
         past5snow = past4snow+lag4snow)


## Merge by year

winter1718 <- list(trailheads1819, caic17, snotel17) %>% reduce(full_join, by="date")
winter1819 <- list(trailheads1819, caic18, snotel18) %>% reduce(full_join, by="date")
winter1920 <- list(trailheads1920, caic19, snotel19) %>% reduce(full_join, by="date")

## Pivot to long format

winter1718 <- pivot_longer(winter1718, 
                           c("Non.motorized", "Mechanized", "Motorized", "Hybrid"), 
                           names_to = "modality", values_to = "user.count")
winter1718$modality <- factor(winter1819$modality)

winter1819 <- pivot_longer(winter1819, 
                           c("Non.motorized", "Mechanized", "Motorized", "Hybrid"), 
                           names_to = "modality", values_to = "user.count")
winter1819$modality <- factor(winter1819$modality)

winter1920 <- pivot_longer(winter1819, 
                           c("Non.motorized", "Mechanized", "Motorized", "Hybrid"), 
                           names_to = "modality", values_to = "user.count")
winter1920$modality <- factor(winter1819$modality)

## Add some variables

winter1819$week <- factor(wday(winter1819$date), labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
winter1819$weekend <- ifelse(winter1819$week %in% c("Sat", "Sun"), "weekend", "weekday")

## Summaries to check reasonableness

summary(winter1819)


#  1. How do trail visitor counts correlate to an increase in snowfall during the previous day(s)? 
#     (what patterns of snowfall yield the most visitors/changes in visitation)

ggplot(winter1819, aes(change_depth, user.count, color = Trailhead))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.5)+
  facet_wrap(vars(modality))

ggplot(winter1819, aes(past3snow, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()+
  facet_grid(modality~Trailhead)

ggplot(winter1819, aes(past5snow, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()+
  facet_grid(modality~Trailhead)

glm1 <- glm(user.count ~ modality*Trailhead+change_depth+past2snow+past3snow+past5snow, family = poisson, data=winter1819)
Anova(glm1, test.statistic = "LR")
summary(glm1)
stepAIC(glm1)

#  2. How does an increased avalanche risk impact the numbers of hybrid and motorized visitors to specific trailheads?

ggplot(na.omit(winter1819), aes(rating_near, user.count, color = Trailhead, fill = Trailhead))+
  geom_point(position=position_jitterdodge(jitter.width = 0.2))+
  geom_boxplot(color="black", outlier.shape = NA)+
  scale_y_log10()+
  facet_wrap(vars(modality))


glm2 <- glm(user.count ~ modality*Trailhead*(rating_below+rating_near+rating_above), family = poisson, data=winter1819)
Anova(glm2, test.statistic = "LR")
summary(glm2)
stepAIC(glm2)

#  3. What combination of weather conditions yields the highest/lowest number of visitors?

ggplot(winter1819, aes(air_temp, past5snow, size = user.count, color = rating_near))+
  geom_jitter(height = 0.2, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))

ggplot(winter1819, aes(air_temp, past5snow, size = user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  facet_grid(modality~Trailhead)

glm3 <- glm(user.count~air_temp+change_depth+snow_depth, family = poisson, data=winter1819)
summary(glm3)

#  4. What sample size is needed to represent the entire season ?
#  5. Which days of the week have the highest number of visitors? Weekdays vs weekends?

glm5 <- glm(user.count~weekend, family = poisson, data=winter1819)
summary(glm5)

#  6. How do visitation rates for different user groups vary seasonally?


#  7. Which days of the year are busiest?

day_totals1819 <- winter1819 %>% 
  group_by(date) %>%
  summarise(users = sum(user.count, na.rm = TRUE), 
            week = unique(week)) %>%
  arrange(desc(users))

head(day_totals1819, 10)

ggplot(day_totals1819, aes(date, users))+
  geom_line()+
  geom_point(aes(color=week))


#  8. Do different groups of users utilize the trail more during different times of the season?


ggplot(winter1819, aes(date, user.count+1, color = Trailhead))+
  geom_point()+
  geom_smooth()+
  scale_y_log10()+
  facet_wrap(vars(modality))

gam6 <- gam(user.count ~ modality*s(date), family = poisson, data=winter1819)
summary(gam6)

modality_day_totals1819 <- winter1819 %>% 
  group_by(date, modality) %>%
  summarise(users = sum(user.count, na.rm = TRUE), 
            week = unique(week)) %>%
  arrange(desc(users))

head(modality_day_totals1819, 10)

ggplot(modality_day_totals1819, aes(date, users, color = modality))+
  geom_line()+
  geom_smooth()+
  scale_y_log10()


#  9. How did covid affect visitation rates and use patterns?
#  10. What are major year-to-year trends in visitation?
#  11. How do the days with the highest avalanche risk affect visitor rates?

high_risk <- as.numeric(winter1819$rating_above)+as.numeric(winter1819$rating_near)+as.numeric(winter1819$rating_below) > 9
summary(glm(user.count~high_risk, data=winter1819, family = poisson))
