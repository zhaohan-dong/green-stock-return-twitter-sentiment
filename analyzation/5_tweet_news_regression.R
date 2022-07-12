## Regression between tweets and NYT/AP

require(tidyverse, quietly = TRUE)
require(readxl, quietly = TRUE)
require(lubridate, quietly = TRUE)

nyt_climate_change <- read_excel("data/nyt_climate_change_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))
nyt_global_warming <- read_excel("data/nyt_global_warming_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))
ap_climate_change <- read_excel("data/ap_climate_change_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))
ap_global_warming <- read_excel("data/ap_global_Warming_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))

nyt_climate_change <- nyt_climate_change %>%
  merge(count(nyt_climate_change, date, name = "nyt_climate_change_count")) %>%
  select(date, nyt_climate_change_count) %>%
  distinct() %>%
  filter(!is.na(date))
nyt_global_warming <- nyt_global_warming %>%
  merge(count(nyt_global_warming, date, name = "nyt_global_warming_count")) %>%
  select(date, nyt_global_warming_count) %>%
  distinct() %>%
  filter(!is.na(date))
ap_climate_change <- ap_climate_change %>%
  merge(count(ap_climate_change, date, name = "ap_climate_change_count")) %>%
  select(date, ap_climate_change_count) %>%
  distinct()
ap_global_warming <- ap_global_warming %>%
  merge(count(ap_global_warming, date, name = "ap_global_warming_count")) %>%
  select(date, ap_global_warming_count) %>%
  distinct()

news_summary_df <- nyt_climate_change %>%
  merge(nyt_global_warming) %>%
  merge(ap_climate_change) %>%
  merge(ap_global_warming) %>%
  group_by(date) %>%
  mutate(nyt_total = nyt_climate_change_count +
           nyt_global_warming_count,
         ap_total = ap_climate_change_count +
           ap_global_warming_count,
         news_total = nyt_total + ap_total) %>%
  ungroup()

summary_df <- read_csv("data/combined_monthly_data.csv")
green_etf_factor <- read_csv("data/green_etf_factor.csv")

df <- news_summary_df %>%
  merge(summary_df) %>%
  merge(green_etf_factor) %>%
  filter(date >= as.Date("2012-04-01"))

tweet_model <- arima(df$monthly_tweet_count, order = c(1, 0, 0))
nyt_model <- arima(df$nyt_total, order = c(1, 0, 0))
ap_model <- arima(df$ap_total, order = c(1, 0, 0))
news_model <- arima(df$news_total, order = c(1, 0, 0))

df$tweet_residual <- c(tweet_model$residuals)
df$nyt_residual <- c(nyt_model$residuals)
df$ap_residual <- c(ap_model$residuals)
df$news_residual <- c(news_model$residuals)

p <- ggplot(df, aes(x = date))

p + geom_line(aes(y = news_residual)) +
  geom_line(aes(y = tweet_residual / 1000, color = "Blue"))

cor.test(df$nyt_residual, lag(df$tweet_residual, 1), use = "complete.obs")

