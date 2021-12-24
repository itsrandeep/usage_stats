library(RSQLite)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)

# Read SQLite file and fetch all data from usageStats Table
con <- dbConnect(drv=RSQLite::SQLite(), dbname="data/usageDirect-history.sqlite3")
usage_stats <- dbGetQuery(conn=con, statement=paste("SELECT * FROM 'usageStats'", sep=""))
usage_stats <- as_tibble(usage_stats)
                         
# table contains days since epoch, converting it to date format for further use
usage_stats <- usage_stats %>%
               mutate(date= as_date(day),timeUsed_secs = timeUsed/1000) %>%
               select(applicationId, date, timeUsed_secs) 

# Reading app_name that contains map of ApplicationId to app names,
# If app_names doesn't contain application_id then use ApplicationId
app_names <- read_csv("data/app_names.csv")
usage_stats_with_names <- usage_stats %>% 
                          left_join(app_names, by=c('applicationId' = 'PackageName')) %>%
                          mutate(AppName = if_else(is.na(AppName),applicationId,AppName))

# Adding day of week in table for grouping
usage_stats_with_names <- usage_stats_with_names %>% 
                          mutate(w_day=wday(date,label = TRUE),
                                 is_weekend=ifelse(w_day %in% c('Sat', 'Sun'),'Yes','No'))

########################### PLOTS ##################################

# Daily Overall Usage of phone by hours
usage_stats_with_names %>% 
  group_by(date) %>% 
  summarise(total=sum(timeUsed_secs/60/60)) %>% 
  ggplot(aes(date, total)) + geom_line() + geom_point()

# Different color points for weekends
usage_stats_with_names %>% 
  group_by(date, is_weekend) %>% 
  summarise(total_hours=sum(timeUsed_secs/60/60)) %>% 
  ggplot(aes(date, total_hours)) + geom_point(aes(color=is_weekend))

# Total and Average hours plot of app Usage on different Weekdays
usage_stats_with_names %>% 
  group_by(w_day) %>%
  summarise(total_hours=sum(timeUsed_secs/60/60)) %>% 
  ggplot(aes(w_day, total_hours)) + geom_col()

usage_stats_with_names %>% 
  group_by(date, w_day) %>%
  summarise(total_hours=sum(timeUsed_secs/60/60)) %>%
  group_by(w_day) %>%
  summarise(mean_hours=mean(total_hours)) %>%
  ggplot(aes(w_day, mean_hours)) +
  geom_col()
