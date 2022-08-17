## Regression between tweets and NYT/AP

require(tidyverse, quietly = TRUE)
require(readxl, quietly = TRUE)
require(lubridate, quietly = TRUE)

# Load news data
nyt_climate_change <- read_xlsx("data/nyt_climate_change_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))
nyt_global_warming <- read_xlsx("data/nyt_global_warming_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))
ap_climate_change <- read_xlsx("data/ap_climate_change_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))
ap_global_warming <- read_xlsx("data/ap_global_Warming_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))

# Bind different climate change term counts
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

# Load stock and twitter data
summary_df <- read_csv("data/combined_monthly_data.csv")
green_etf_factor <- read_csv("data/green_etf_factor.csv")

# Merge the dataframes
df <- news_summary_df %>%
  merge(summary_df) %>%
  merge(green_etf_factor) %>%
  filter(date >= as.Date("2012-04-01"))

# Calculate AR(1) for twitter and news to get residuals
tweet_model <- arima(df$monthly_tweet_count, order = c(1, 0, 0))
nyt_model <- arima(df$nyt_total, order = c(1, 0, 0))
ap_model <- arima(df$ap_total, order = c(1, 0, 0))
news_model <- arima(df$news_total, order = c(1, 0, 0))

df$tweet_residual <- c(tweet_model$residuals)
df$nyt_residual <- c(nyt_model$residuals)
df$ap_residual <- c(ap_model$residuals)
df$news_residual <- c(news_model$residuals)

# Plotting residuals
p <- ggplot(df, aes(x = date)) + theme_classic()
colors <- c("News Total" = "black", "NYT" = "orange", "AP" = "red", "Tweet / 1000" = "deepskyblue")
p + geom_line(aes(y = news_total, color = "News Total")) +
  geom_line(aes(y = nyt_total, color = "NYT")) +
  geom_line(aes(y = ap_total, color = "AP")) +
  labs(x = "Date",
       y = "Monthly Count",
       color = "Legend") +
  scale_color_manual(values = colors)

p + geom_line(aes(y = news_residual, color = "News Total")) +
  geom_line(aes(y = nyt_residual, color = "NYT")) +
  geom_line(aes(y = ap_residual, color = "AP")) +
  geom_line(aes(y = tweet_residual / 1000, color = "Tweet / 1000")) +
  labs(x = "Date",
       y = "Residual",
       color = "Legend") +
  scale_color_manual(values = colors)

# Test correlations
# Original correlation
cor.test(lag(df$ap_residual, 0), lag(df$tweet_residual, 0), use = "complete.obs")
cor.test(lag(df$nyt_residual, 0), lag(df$tweet_residual, 0), use = "complete.obs")
cor.test(lag(df$news_residual, 0), lag(df$tweet_residual, 0), use = "complete.obs")
# AR(1) residual correlation
cor.test(df$ap_total, lag(df$monthly_tweet_count, 0), use = "complete.obs")
cor.test(df$nyt_total, lag(df$monthly_tweet_count, 0), use = "complete.obs")
cor.test(df$news_total, lag(df$monthly_tweet_count, 0), use = "complete.obs")
