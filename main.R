library(RSQLite)
library(dplyr)
library(ggplot2)

# Read SQLite file and fetch all data from usageStats Table
con <- dbConnect(drv=RSQLite::SQLite(), dbname="usageDirect-history.sqlite3")
usage_stats <- dbGetQuery(conn=con, statement=paste("SELECT * FROM 'usageStats'", sep=""))

# table contains days since epoch, converting it to date format for further use
usage_stats <- usage_stats %>% mutate(date= as.Date("1970-01-01") + day)
# Add timeUsed in secs column 
usage_stats <- usage_stats %>% mutate(timeUsed_secs = timeUsed/1000)


