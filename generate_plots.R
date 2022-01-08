library(readr)
library(dplyr)
library(ggplot2)

usage_stats_with_names = read_csv("data/usage_stats_with_names.csv")
########################### PLOTS ##################################

# Daily Overall Usage of phone by hours
usage_stats_with_names %>% 
  group_by(date) %>% 
  summarise(total_hours=sum(timeUsed_secs/60/60)) %>% 
  ggplot(aes(date, total_hours)) + geom_line() + geom_point()
ggsave('plots/daily_total_usage.jpg')

# Different color points for weekends
usage_stats_with_names %>% 
  group_by(date, is_weekend) %>% 
  summarise(total_hours=sum(timeUsed_secs/60/60)) %>% 
  ggplot(aes(date, total_hours)) + geom_point(aes(color=is_weekend))
ggsave('plots/daily_total_usage_scatter.jpg')

# Total hours plot of app Usage on different Weekdays
usage_stats_with_names %>% 
  group_by(w_day) %>%
  summarise(total_hours=sum(timeUsed_secs/60/60)) %>% 
  ggplot(aes(w_day, total_hours)) + geom_col()

# Average hours plot of app Usage on different Weekdays
usage_stats_with_names %>% 
  group_by(date, w_day) %>%
  summarise(total_hours=sum(timeUsed_secs/60/60)) %>%
  group_by(w_day) %>%
  summarise(mean_hours=mean(total_hours)) %>%
  ggplot(aes(w_day, mean_hours)) +
  geom_col()
ggsave('plots/avg_usage_by_w_day.jpg')

################### Analysis by App Names ####################

# Filter Top 5 Apps of everyday and count which apps are
# in top 5 for most of the days
usage_stats_with_names %>% 
  group_by(date) %>% 
  filter(rank(desc(timeUsed_secs)) < 5) %>% 
  ungroup() %>% 
  count(AppName) %>% 
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(reorder(AppName, n),n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "App Name",
    y = "No. of Times App is in Daily Top 5",
    title = "Most Consistently used Apps"
  ) 
ggsave('plots/consistently_used_daily_top5_apps.jpg')

## Filter out most used App of every day and plot them
usage_stats_with_names %>% 
  group_by(date) %>% 
  filter(timeUsed_secs == max(timeUsed_secs)) %>% 
  ungroup() %>% 
  count(AppName) %>% 
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(reorder(AppName, n),n)) +
  geom_col() +
  coord_flip()+
  labs(
    x = "App Name",
    y = "No. of Times App is Most Used in a Day"
  ) 
ggsave('plots/mostly_used_apps_in_a_day.jpg')

## Filter out most used App of every day and plot with timeUsed as size
usage_stats_with_names %>% 
  group_by(date) %>% 
  filter(timeUsed_secs == max(timeUsed_secs)) %>% 
  group_by(AppName) %>% 
  summarize(n=n(), total_app_used_hrs=sum(timeUsed_secs)/60/60) %>%
  arrange(desc(n)) %>% 
  head(10) %>%
  ggplot(aes(n,reorder(AppName, n))) + 
  geom_point(aes(size=total_app_used_hrs)) +
  labs(
    x = "App Name",
    y = "No. of Times App is Most Used in a Day"
  ) 
ggsave('plots/mostly_used_apps_in_a_day_2.jpg')

# Filter Top 5 Apps of everyday and count which apps are
# in top 5 for most of the days geom_point with timeUsed as size
usage_stats_with_names %>% 
  group_by(date) %>% 
  filter(rank(desc(timeUsed_secs)) < 5) %>% 
  group_by(AppName) %>% 
  summarize(n=n(), total_app_used_hrs=sum(timeUsed_secs)/60/60) %>%
  arrange(desc(n)) %>% 
  head(10) %>%
  ggplot(aes(n,reorder(AppName, n))) + 
  geom_point(aes(size=total_app_used_hrs)) +
  labs(
    x = "App Name",
    y = "No. of Times App is in Daily Top 5",
    title = "Most Consistently used Apps"
  ) 
ggsave('plots/consistently_used_daily_top5_apps_2.jpg')

