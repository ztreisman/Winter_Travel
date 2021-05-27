#############
## 
## Winter Travel Data Collection Initiative
## Zack Treisman
## 12/7/20
## modified 4/26/21
##
#############


# Load libraries
library(tidyverse)
library(lubridate)
library(car)
library(MASS)
library(gam)
library(pscl)
library(mgcv)
library(glmmTMB)
library(glarma)

# Load trailhead data

# Changes to spreadsheets: remove spaces from directory names, Zach -> Zack, 
#   convert to csv (used in2csv), simplify names, 
#   remove summaries and other items at bottom of some files
# Use read.csv instead of read_csv because Non-motorized has that dash 
#   and read_csv makes that a quoted string instead of converting it to a dot. 
#   Probably not worth the trouble to fix in the spreadsheets.

# Snow bike to motorized: for 17-18 there were no changes for Brush Creek TH, 
#   Gothic, Slate, or Snodgrass. For 18-19 there were no changes to 
#   Brush Creek TH or Slate. 

# 2019-12-10 was entered as 2020

brush17 <- read.csv("Data_for_R/2017-2018/brush.csv")
brush_rd17 <- read.csv("Data_for_R/2017-2018/brush_rd.csv")
cement17 <- read.csv("Data_for_R/2017-2018/cement.csv")
gothic17 <- read.csv("Data_for_R/2017-2018/gothic.csv")
kebler17 <- read.csv("Data_for_R/2017-2018/kebler.csv")
slate17 <- read.csv("Data_for_R/2017-2018/slate.csv")
snodgrass17 <- read.csv("Data_for_R/2017-2018/snodgrass.csv")
washington17 <- read.csv("Data_for_R/2017-2018/washington.csv")

brush17$Hybrid <- NA

trailheads1718 <- bind_rows(list(brush17, brush_rd17,cement17, gothic17, 
                                 kebler17, slate17, snodgrass17, washington17))
trailheads1718$Date <- parse_date_time(trailheads1718$Date, 
                                       orders = c("ymd", "mdy"))
trailheads1718 <- trailheads1718 %>% rename(date = Date)




brush18 <- read.csv("Data_for_R/2018-2019/brush.csv")
cement18 <- read.csv("Data_for_R/2018-2019/cement.csv")
gothic18 <- read.csv("Data_for_R/2018-2019/gothic.csv")
kebler18 <- read.csv("Data_for_R/2018-2019/kebler.csv")
slate18 <- read.csv("Data_for_R/2018-2019/slate.csv")
snodgrass18 <- read.csv("Data_for_R/2018-2019/snodgrass.csv")
washington18 <- read.csv("Data_for_R/2018-2019/washington.csv")
trailheads1819 <- bind_rows(list(brush18,cement18, gothic18, kebler18, 
                                 slate18, snodgrass18, washington18))
trailheads1819$Date <- parse_date_time(trailheads1819$Date, 
                                       orders = c("ymd", "mdy"))
trailheads1819 <- trailheads1819 %>% rename(date = Date)



brush_rd19 <- read.csv("Data_for_R/2019-2020/brush_rd.csv")
cement19 <- read.csv("Data_for_R/2019-2020/cement.csv")
gothic19 <- read.csv("Data_for_R/2019-2020/gothic.csv")
kebler19 <- read.csv("Data_for_R/2019-2020/kebler.csv")
slate19 <- read.csv("Data_for_R/2019-2020/slate.csv")
snodgrass19 <- read.csv("Data_for_R/2019-2020/snodgrass.csv")
washington19 <- read.csv("Data_for_R/2019-2020/washington.csv")

trailheads1920 <- bind_rows(list(brush_rd19, cement19, gothic19, kebler19, 
                                 slate19, snodgrass19, washington19))
trailheads1920$Date <- parse_date_time(trailheads1920$Date, 
                                       orders = c("ymd", "mdy"))
trailheads1920 <- trailheads1920 %>% rename(date = Date)
trailheads1920[trailheads1920$date=="2020-12-10 UTC",]$date

## Join years

trailheads1718$year <- "w1718"
trailheads1819$year <- "w1819"
trailheads1920$year <- "w1920"

trailheads <- bind_rows(list(trailheads1718,trailheads1819,trailheads1920))
trailheads$year <- factor(trailheads$year)


## Fix some capitalization issues 

trailheads <- trailheads %>% mutate(Non.motorized = coalesce(Non.Motorized, Non.motorized))
trailheads$Non.Motorized <- NULL

trailheads[trailheads$Trailhead=="Slate River","Trailhead"] <- "Slate River Rd"
trailheads <- droplevels(trailheads)

## Pivot to long format

trailheads <- pivot_longer(trailheads, 
                              c("Non.motorized", "Mechanized", "Motorized", "Hybrid"), 
                              names_to = "modality", values_to = "user.count")
trailheads$modality <- factor(trailheads$modality)


## Add some variables

trailheads$week <- factor(wday(trailheads$date), 
                          labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
trailheads$weekend <- factor(ifelse(trailheads$week %in% c("Sat", "Sun"), 
                                    "weekend", "weekday"))

trailheads$has_sled <- factor(ifelse(trailheads$modality %in% c("Hybrid", "Motorized"), "sled", "no_sled"))

## Collapse by user group and location


all_modes <- trailheads %>% 
  group_by(date, Trailhead, year, week, weekend, has_sled) %>%
  summarize(user.count = sum(user.count, na.rm = TRUE))

all_locations <- trailheads %>% 
  group_by(date, modality, year, week, weekend, has_sled) %>%
  summarize(user.count = sum(user.count, na.rm = TRUE))

all_users <- trailheads %>% 
  group_by(date, year, week, weekend, has_sled) %>%
  summarize(user.count = sum(user.count, na.rm = TRUE))

## Load CAIC data

# Add. changes: change spaces to underscores, date to date_time

caic17 <- read_csv("Data_for_R/CAIC_Data_2017-2020/caic2017.csv")
caic17 <- caic17[-1,] # No other data for 2017-12-21
caic18 <- read_csv("Data_for_R/CAIC_Data_2017-2020/caic2018.csv")
caic19 <- read_csv("Data_for_R/CAIC_Data_2017-2020/caic2019.csv")

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

## Join years

caic <- bind_rows(list(caic17,caic18,caic19))


## Load SNOTEL data

# Add. changes: simplify headers

snotel17 <- read_csv("Data_for_R/SNOTEL_2017-2020/snotel1718.csv", skip = 7)
snotel17 <- snotel17[-1,] # No other data for 2017-12-21
snotel18 <- read_csv("Data_for_R/SNOTEL_2017-2020/snotel1819.csv", skip = 7)
snotel19 <- read_csv("Data_for_R/SNOTEL_2017-2020/snotel1920.csv", skip = 7)

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

snotel <- bind_rows(list(snotel17,snotel18,snotel19))

## Merge data

winter_travel <- list(trailheads, caic, snotel) %>% reduce(full_join, by="date")
all_locations <- list(all_locations, caic, snotel) %>% reduce(full_join, by="date")
all_modes <- list(all_modes, caic, snotel) %>% reduce(full_join, by="date")
all_users <- list(all_users, caic, snotel) %>% reduce(full_join, by="date")

## Summaries to check reasonableness

summary(winter_travel)
summary(all_modes)
summary(all_locations)
summary(all_users)

## How much of the user counts are zeros?

mean(winter_travel$user.count==0, na.rm=T)
tapply(winter_travel$user.count, 
       list(winter_travel$Trailhead, winter_travel$modality), 
       function(x){mean(x==0, na.rm=T)})

mean(all_modes$user.count==0, na.rm=T)
mean(all_locations$user.count==0, na.rm=T)
mean(all_users$user.count==0, na.rm=T)

## Save the data to csv

write.csv(winter_travel, file = "data/winter_travel.csv")
write.csv(all_modes, file="data/all_modes.csv")
write.csv(all_locations, file = "data/all_locations.csv")
write.csv(all_users, file="data/all_users.csv")

#  1. How do trail visitor counts correlate to an increase in snowfall during the previous day(s)? 
#     (what patterns of snowfall yield the most visitors/changes in visitation)

ggplot(winter_travel, aes(change_depth, user.count, color = Trailhead))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.5)+
  facet_grid(modality~year)+
  scale_y_log10()

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

ggplot(all_users, aes(change_depth, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()

ggplot(all_users, aes(air_temp, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()

ggplot(all_users, aes(lag2snow, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()

ggplot(all_users, aes(lag3snow, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()

ggplot(all_users, aes(lag4snow, user.count, color = rating_near))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()

pairs(winter_travel[,c("change_depth","past2snow","past3snow","past4snow","past5snow")])
pairs(winter_travel[,c("change_depth","lag2snow","lag3snow","lag4snow")])

ggplot(winter_travel, aes(change_depth, lag1snow))+geom_jitter(alpha=0.1)
ggplot(winter_travel, aes(change_depth, past2snow))+geom_jitter(alpha=0.1)

# Poisson models

glm0 <- glm(user.count ~ 1, data=all_users, family=poisson)
summary(glm0)

glm1 <- glm(user.count ~ change_depth+lag3snow+air_temp+weekend, family = poisson, data=all_users)
summary(glm1)
hist(resid(glm1))
confint(glm1)

# Looks like about a 3% decrease in users per cm of new snow, but a 1.4% increase per cm of snow 4 days ago.
# Also about an 0.5% increase in users per degC of air temp.
# Recent snow variables are not significant. 
# If there is a trend, generally the more snow, the fewer people are out. 

# Negative binomial models

glm0nb <- glm.nb(user.count ~ weekend, data=all_users)
summary(glm0nb)

glm1nb <- glm.nb(user.count ~ change_depth+past3snow+air_temp+weekend, data=all_users)
summary(glm1nb)
hist(resid(glm1nb))
confint(glm1nb)



# AIC says this is better.

acf(glm1nb$residuals)

# Add AR

y <- all_users[,"user.count"]
X <- all


glm2nbAR1 <- glmmTMB(user.count ~ change_depth+lag3snow+air_temp+weekend +ar1(date), data=all_users)
summary(glm1nb)
hist(resid(glm1nb))
confint(glm1nb)


#  2. How does an increased avalanche risk impact the numbers of hybrid and motorized visitors to specific trailheads?

table(winter_travel$rating_above, winter_travel$rating_near)
table(winter_travel$rating_near, winter_travel$rating_below)
table(winter_travel$rating_above,winter_travel$rating_below)


ggplot(na.omit(winter_travel), aes(rating_near, user.count, color = Trailhead, fill = Trailhead))+
  geom_point(position=position_jitterdodge(jitter.width = 0.2))+
  geom_boxplot(color="black", outlier.shape = NA)+
  scale_y_log10()+
  facet_wrap(~has_sled)

ggplot(na.omit(winter_travel), aes(rating_below, user.count, fill=rating_below))+
  geom_jitter()+
  geom_boxplot(outlier.shape = NA)+
  scale_y_log10()+
  facet_grid(has_sled~Trailhead)+
  scale_fill_manual(values = c("green", "yellow", "orange", "red"))

ggplot(na.omit(winter_travel), aes(rating_near, user.count, fill=rating_near))+
  geom_jitter()+
  geom_boxplot(outlier.shape = NA)+
  scale_y_log10()+
  facet_grid(has_sled~Trailhead)+
  scale_fill_manual(values = c("green", "yellow", "orange", "red"))

ggplot(na.omit(winter_travel), aes(rating_above, user.count, fill=rating_above))+
  geom_jitter()+
  geom_boxplot(outlier.shape = NA)+
  scale_y_log10()+
  facet_grid(has_sled~Trailhead)+
  scale_fill_manual(values = c("green", "yellow", "orange", "red"))


glm2 <- glm(user.count ~ Trailhead*rating_near, family = poisson, data=winter_travel[winter_travel$has_sled==TRUE,])
Anova(glm2, test.statistic = "LR")
summary(glm2)

glm2brushrd <- glm.nb(user.count ~ rating_near, 
                  data=winter_travel[(winter_travel$has_sled==TRUE)&(winter_travel$Trailhead=="Brush Creek Rd"),])
summary(glm2brushrd)

glm2brushth <- glm.nb(user.count ~ rating_near, 
                   data=winter_travel[(winter_travel$has_sled==TRUE)&(winter_travel$Trailhead=="Brush Creek Trailhead"),])
summary(glm2brushth)

glm2gothic <- glm.nb(user.count ~ rating_near, 
                  data=winter_travel[(winter_travel$has_sled==TRUE)&(winter_travel$Trailhead=="Gothic Rd"),])
summary(glm2gothic)

glm2cement <- glm.nb(user.count ~ rating_near, 
                  data=winter_travel[(winter_travel$has_sled==TRUE)&(winter_travel$Trailhead=="Cement Creek"),])
summary(glm2cement)

glm2kebler <- glm.nb(user.count ~ rating_near, 
                  data=winter_travel[(winter_travel$has_sled==TRUE)&(winter_travel$Trailhead=="Kebler"),])
summary(glm2kebler)

glm2slate <- glm.nb(user.count ~ rating_near, 
                  data=winter_travel[(winter_travel$has_sled==TRUE)&(winter_travel$Trailhead=="Slate River Rd"),])
summary(glm2slate)

glm2snodgrass <- glm.nb(user.count ~ rating_near, 
                  data=winter_travel[(winter_travel$has_sled==TRUE)&(winter_travel$Trailhead=="Snodgrass"),])
summary(glm2snodgrass)

glm2washington <- glm.nb(user.count ~ rating_near, 
                  data=winter_travel[(winter_travel$has_sled==TRUE)&(winter_travel$Trailhead=="Washington Gulch"),])
summary(glm2washington)

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

glm3 <- glm.nb(user.count~air_temp+change_depth+snow_depth, data=winter_travel)
summary(glm3)

# Looks like with new snow there are fewer users. About 2% per cm of new snow. 
# Warmer air temps bring people out, about 1% per degree. 
# More snow on the ground also correlates with fewer people.


#  4. What sample size is needed to represent the entire season ?

# This depends on the question, and the effect size that we care about.

#  5. Which days of the week have the highest number of visitors? Weekdays vs weekends?

glm5 <- glm.nb(user.count~weekend, data=winter_travel)
summary(glm5)

glm5a <- glm.nb(user.count~week, data=winter_travel)
summary(glm5a)

# Saturdays are about 12.5% busier than Sundays, weekdays are about half as busy as weekends.

#  6. How do visitation rates for different user groups vary seasonally?

ggplot(winter_travel, aes(year, user.count))+
  geom_jitter(height = 0, width = 0.2, alpha = 0.7)+
  facet_grid(modality~Trailhead)+
  scale_y_log10()



#  7. Which days of the year are busiest?

day_totals <- all_users %>% 
  group_by(date, week, weekend) %>%
  summarise(users = sum(user.count, na.rm = TRUE)) %>%
  arrange(desc(users))

head(day_totals, 10)

ggplot(day_totals, aes((yday(date)+30)%%365, users))+
  geom_smooth()+
  geom_point(aes(color=weekend))+
  scale_x_continuous(breaks = c(0,31,62,90,120), 
                     labels = c("Dec 1", "Jan 1", "Feb 1", "Mar 1", "Apr 1"))+
  labs(x="")

# Jump in usage in the last week of the calendar year, otherwise gradual increase until sometime mid March.

#  8. Do different groups of users utilize the trail more during different times of the season?

day_totals_modes <- all_locations %>% 
  group_by(date, week, weekend, modality, has_sled) %>%
  summarise(users = sum(user.count, na.rm = TRUE)) %>%
  arrange(desc(users))

head(day_totals_modes, 10)

ggplot(day_totals_modes, aes((yday(date)+ifelse(date>ymd("2020-2-28"),31,30))%%ifelse(date>ymd("2020-2-28"),31,30)), users))+
  geom_smooth()+
  geom_point(aes(color=weekend))+
  scale_x_continuous(breaks = c(0,31,62,90,120), 
                     labels = c("Dec 1", "Jan 1", "Feb 1", "Mar 1", "Apr 1"))+
  scale_y_log10()+
  labs(x="")+
  facet_grid(modality~1)

day_totals_modes2 <- all_locations %>% 
  group_by(date, week, weekend, has_sled) %>%
  summarise(users = sum(user.count, na.rm = TRUE)) %>%
  arrange(desc(users))

ggplot(day_totals_modes2, aes((yday(date)+30)%%365, users))+
  geom_smooth()+
  geom_point(aes(color=weekend))+
  scale_x_continuous(breaks = c(0,31,62,90,120), 
                     labels = c("Dec 1", "Jan 1", "Feb 1", "Mar 1", "Apr 1"))+
  scale_y_log10()+
  labs(x="")+
  facet_grid(has_sled~1)

# Variance is greater in the motorized groups, and the end of year and mid March peaks are more pronounced.

gam6 <- gam(user.count ~ modality+s((yday(date)+30)%%365), family = nb(), data=all_locations)
summary(gam6)

#  9. How did covid affect visitation rates and use patterns?
march_on <- all_users[month(all_users$date)%in%3:4,]
ggplot(march_on, aes((yday(march_on$date)+30%%365), user.count, color=year))+
  geom_point()+geom_smooth()+
  scale_x_continuous(breaks = c(90,122), 
                     labels = c("Mar 1", "Apr 1"))+
  scale_y_log10()+
  labs(x="")+
  facet_grid(has_sled~1)

# I don't see anything.

#  10. What are major year-to-year trends in visitation?

ggplot(winter_travel, aes(year, user.count))+
  geom_boxplot()+
  geom_jitter(height = 0.2, width = 0.2, alpha = 0.2, aes( color = rating_near))+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()+
  facet_grid(modality~Trailhead)

ggplot(all_modes, aes(year, user.count))+
  geom_boxplot()+
  geom_jitter(height = 0.2, width = 0.2, alpha = 0.2, aes( color = rating_near))+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()+
  facet_grid(~Trailhead)

ggplot(all_locations, aes(year, user.count))+
  geom_boxplot()+
  geom_jitter(height = 0.2, width = 0.2, alpha = 0.2, aes( color = rating_near))+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()+
  facet_grid(~modality)

ggplot(all_users, aes(year, user.count))+
  geom_boxplot()+
  geom_jitter(height = 0.2, width = 0.2, alpha = 0.2, aes( color = rating_near))+
  scale_color_manual(values = c("green", "yellow", "orange", "red"))+
  scale_y_log10()+
  facet_grid(~has_sled)

glm10 <- glm.nb(user.count~year*(Trailhead+modality+rating_near), data = winter_travel)
summary(glm10)

#  11. How do the days with the highest avalanche risk affect visitor rates?

high_risk <- as.numeric(winter_travel$rating_above)+
  as.numeric(winter_travel$rating_near)+
  as.numeric(winter_travel$rating_below) > 9
summary(glm.nb(user.count~high_risk, data=winter_travel))

# On high risk days the usage is about 30% of the average on days that are not high risk.
