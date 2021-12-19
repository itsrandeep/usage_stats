library(RSQLite)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)

# Read SQLite file and fetch all data from usageStats Table
con <- dbConnect(drv=RSQLite::SQLite(), dbname="usageDirect-history.sqlite3")
usage_stats <- dbGetQuery(conn=con, statement=paste("SELECT * FROM 'usageStats'", sep=""))
usage_stats <- as_tibble(usage_stats)
                         
# table contains days since epoch, converting it to date format for further use
usage_stats <- usage_stats %>%
               mutate(date= as_date(day),timeUsed_secs = timeUsed/1000) %>%
               select(applicationId, date, timeUsed_secs) 

# Reading app_name that contains map of ApplicationId to app names,
# If app_names doesn't contain application_id then use ApplicationId
app_names <- read_csv("app_names.csv")
usage_stats_with_names <- usage_stats %>% 
                          left_join(app_names, by=c('applicationId' = 'PackageName')) %>%
                          mutate(AppName = if_else(is.na(AppName),applicationId,AppName))

# Adding day of week in table for grouping
usage_stats_with_names <- usage_stats_with_names %>% mutate(wday=wday(date,label = TRUE))


