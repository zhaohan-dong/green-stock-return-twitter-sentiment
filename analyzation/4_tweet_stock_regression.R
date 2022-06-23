## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)

# Load data and convert date column from character to date type
twitter_df <- read_twitter_csv("data/tweet_sentiment.csv")
bmg_monthly_summary <- read_csv("data/bmg_monthly_summary.csv") %>%
  filter(date < as.Date("2018-05-01"))
clean_monthly_summary <- read_csv("data/clean_monthly_summary.csv") %>%
  filter(date < as.Date("2018-05-01"))
oil_gas_monthly_summary <- read_csv("data/oil_gas_monthly_summary.csv") %>%
  filter(date < as.Date("2018-05-01"))
wx_df <- read_csv("data/KNYC_monthly_summary_processed.csv") %>%
  mutate(date = as.Date(date))
oil_price_df <- read_csv("data/Cushing_OK_WTI_Spot_Price_FOB.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  rename(oil_price = `dollars per barrel`) %>%
  filter(date >= as.Date("2012-03-30") & date < as.Date("2018-05-01"))
ff_data <- read_csv("data/equity/F-F_Research_Data_Factors_daily.CSV") %>%
  rename(Mkt_RF = `Mkt-RF`) %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))
spx_data <- read_csv("data/SPX.csv") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  rename(spx_price = price)

# Removing tweet data not contingent for research
twitter_df <- twitter_df %>%
  mutate(date = as.Date(created_at), .after = created_at) %>%
  select(-created_at) %>%
  select(date, status_id, retweet_count, quote_count, reply_count, sentiment) %>%
  filter(date >= as.Date("2012-03-01"))

# Construct summary of twitter mean and standard dev, as well as tweet No. per month
twitter_summary <- twitter_df %>%
  mutate(yr_mon = format(date, "%Y-%m"), .after = date)
twitter_summary <- twitter_summary %>%
  merge(count(twitter_summary, yr_mon, name = "monthly_count")) %>%
  group_by(yr_mon) %>%
  summarize(across(sentiment, list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))), monthly_count) %>%
  ungroup() %>%
  distinct() %>%
  mutate(lag1_count = lag(monthly_count, n = 1),
         count_change_perc = monthly_count / lag1_count - 1) %>%
  filter(yr_mon != "2012-03")


# Summarize monthly oil_price
oil_price_monthly_summary <- oil_price_df %>%
  mutate(yr_mon = format(date, "%Y-%m"), yr = format(date, "%Y"), .after = date) %>%
  group_by(yr_mon) %>%
  summarise(date, across(oil_price, c(mean = mean, sd = sd))) %>%
  filter(date == min(date)) %>%
  ungroup()
oil_price_monthly_summary <- oil_price_monthly_summary %>%
  mutate(lag1_oil_price_mean = lag(oil_price_mean, n = 1),
         oil_price_change_perc = oil_price_mean / lag1_oil_price_mean - 1) %>%
  filter(yr_mon != "2012-03") %>%
  select(-date)

# Extract MKT-RF from ff data and summarize
ff_summary <- ff_data %>%
  mutate(yr_mon = format(date, "%Y-%m"), yr = format(date, "%Y"), .after = date) %>%
  group_by(yr_mon) %>%
  summarise(date, yr_mon, across(Mkt_RF, c(mean = mean, sd = sd))) %>%
  filter(date == min(date))

# Get only spx price monthly average
spx_monthly <- spx_data %>%
  select(date, spx_price) %>%
  mutate(yr_mon = format(date, "%Y-%m"), yr = format(date, "%Y"), .after = date) %>%
  group_by(yr_mon) %>%
  summarise(date, yr_mon, across(spx_price, c(mean = mean, sd = sd))) %>%
  filter(date == min(date)) %>%
  filter(yr_mon != "2012-03" & yr_mon != "2018-05") %>%
  select(-date)

# Join summaries with twitter and market data
twitter_clean_summary <- full_join(clean_monthly_summary, twitter_summary) %>%
  full_join(ff_summary) %>%
  full_join(oil_price_monthly_summary) %>%
  full_join(spx_monthly)
twitter_oil_gas_summary <- full_join(oil_gas_monthly_summary, twitter_summary) %>%
  full_join(ff_summary) %>%
  full_join(oil_price_monthly_summary)  %>%
  full_join(spx_monthly)
twitter_bmg_summary <- full_join(bmg_monthly_summary, twitter_summary) %>%
  full_join(ff_summary) %>%
  full_join(oil_price_monthly_summary)  %>%
  full_join(spx_monthly)

# Delete unused dataframes
rm(ff_data,
   ff_summary,
   bmg_monthly_summary,
   clean_monthly_summary,
   oil_gas_monthly_summary,
   oil_price_df,
   twitter_df)

###################################################################

# Create multiple linear regression model
clean_price_model <- lm(log(price_mean) ~  log(monthly_count) + log(lag1_count) + log(oil_price_mean) + log(spx_monthly$spx_price_mean), data = twitter_clean_summary)
oil_gas_price_model <- lm(log(price_mean) ~  log(monthly_count) + log(lag1_count) + log(oil_price_mean) + log(spx_monthly$spx_price_mean), data = twitter_oil_gas_summary)
bmg_price_model <- lm(log(price_mean) ~  log(monthly_count) + log(lag1_count) + log(oil_price_mean) + log(spx_monthly$spx_price_mean), data = twitter_bmg_summary)
summary(clean_price_model)
summary(oil_gas_price_model)
summary(bmg_price_model)

# Need to calculate oil price fluctuation and tweet count fluctuation!!!!!!!!!!!

# Return
clean_return_model <- lm(daily_raw_return_mean ~  count_change_perc + oil_price_change_perc + Mkt_RF_mean, data = twitter_clean_summary)
oil_gas_return_model <- lm(daily_raw_return_mean ~  count_change_perc + oil_price_change_perc + Mkt_RF_mean, data = twitter_oil_gas_summary)
bmg_return_model <- lm(daily_raw_return_mean ~  monthly_count + oil_price_change_perc + Mkt_RF_mean, data = twitter_bmg_summary)
summary(clean_return_model)
summary(oil_gas_return_model)
summary(bmg_return_model)

# Return standard deviation
clean_return_sd_model <- lm(daily_raw_return_sd ~  log(count_change_perc) + log(oil_price_mean), data = twitter_clean_summary)
oil_gas_return_sd_model <- lm(daily_raw_return_sd ~  log(count_change_perc) + log(oil_price_mean), data = twitter_oil_gas_summary)
bmg_return_sd_model <- lm(daily_raw_return_sd ~  log(monthly_count) + log(oil_price_mean), data = twitter_bmg_summary)
tidy(clean_return_sd_model)
tidy(oil_gas_return_sd_model)
tidy(bmg_return_sd_model)

ggplot(twitter_bmg_summary, aes(x = date, y = log(monthly_count))) +
  geom_line() +
  geom_point()

ggplot(twitter_bmg_summary, aes(x = date, y = log(oil_price_mean))) +
  geom_line() +
  geom_point()

ggplot(twitter_bmg_summary, aes(x = date, y = price_mean)) +
  geom_line() +
  geom_point()

ggplot(twitter_bmg_summary, aes(x = date, y = sentiment_mean)) +
  geom_line() +
  geom_point()

ggplot(oil_price_monthly_summary, aes(x = date, y = price_mean)) +
  geom_line() +
  geom_point()


