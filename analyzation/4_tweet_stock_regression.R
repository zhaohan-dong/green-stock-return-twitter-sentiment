## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)

# Load data and convert date column from character to date type
twitter_df <- read_twitter_csv("data/tweet_sentiment.csv")
gmb_monthly_summary <- read_csv("data/gmb_monthly_summary.csv") %>%
  filter(date < as.Date("2018-05-01"))
clean_monthly_summary <- read_csv("data/clean_monthly_summary.csv") %>%
  filter(date < as.Date("2018-05-01"))
oil_gas_monthly_summary <- read_csv("data/oil_gas_monthly_summary.csv") %>%
  filter(date < as.Date("2018-05-01"))
wx_df <- read_csv("data/KNYC_monthly_summary_processed.csv") %>%
  mutate(date = as.Date(date))
oil_price_df <- read_csv("data/Cushing_OK_WTI_Spot_Price_FOB.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  rename(price = `dollars per barrel`) %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))
ff_data <- read_csv("data/equity/F-F_Research_Data_Factors_daily.CSV") %>%
  rename(Mkt_RF = `Mkt-RF`) %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))

# Removing tweet data not contingent for research
twitter_df <- twitter_df %>%
  mutate(date = as.Date(created_at), .after = created_at) %>%
  select(-created_at) %>%
  select(date, status_id, retweet_count, quote_count, reply_count, sentiment) %>%
  filter(date >= as.Date("2012-04-01"))

# Construct summary of twitter mean and standard dev, as well as tweet No. per month
twitter_summary <- twitter_df %>%
  mutate(yr_mon = format(date, "%Y-%m"), .after = date)
twitter_summary <- twitter_summary %>%
  merge(count(twitter_summary, yr_mon, name = "monthly_count")) %>%
  group_by(yr_mon) %>%
  summarize(across(sentiment, list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))), monthly_count) %>%
  ungroup() %>%
  distinct()

# Summarize monthly oil_price
oil_price_monthly_summary <- oil_price_df %>%
  mutate(yr_mon = format(date, "%Y-%m"), yr = format(date, "%Y"), .after = date) %>%
  group_by(yr_mon) %>%
  summarise(date, across(price, c(mean = mean, sd = sd))) %>%
  filter(date == min(date))

# Extract MKT-RF from ff data and summarize
ff_summary <- ff_data %>%
  mutate(yr_mon = format(date, "%Y-%m"), yr = format(date, "%Y"), .after = date) %>%
  group_by(yr_mon) %>%
  summarise(date, yr_mon, across(Mkt_RF, c(mean = mean, sd = sd))) %>%
  filter(date == min(date))

# Join summaries with twitter and market data
twitter_clean_summary <- full_join(clean_monthly_summary, twitter_summary) %>%
  full_join(ff_summary)
twitter_oil_gas_summary <- full_join(oil_gas_monthly_summary, twitter_summary) %>%
  full_join(ff_summary)
twitter_gmb_summary <- full_join(gmb_monthly_summary, twitter_summary) %>%
  full_join(ff_summary)

# Delete unused dataframes
rm(ff_data,
   ff_summary,
   gmb_monthly_summary,
   clean_monthly_summary,
   oil_gas_monthly_summary,
   oil_price_df,
   twitter_df)

###################################################################

cor(twitter_clean_summary$daily_raw_return_sd, twitter_clean_summary$monthly_count)
cor(twitter_oil_gas_summary$daily_raw_return_sd, twitter_oil_gas_summary$monthly_count)
cor(twitter_gmb_summary$daily_raw_return_sd, twitter_gmb_summary$monthly_count)

# Significant even when we take into account price
clean_model <- lm(daily_raw_return_sd ~ monthly_count, data = twitter_clean_summary)
oil_gas_model <- lm(price_mean ~  monthly_count, data = twitter_oil_gas_summary)
gmb_model <- lm(daily_raw_return_sd ~ price_mean + monthly_count, data = twitter_gmb_summary)
tidy(clean_model)
tidy(oil_gas_model)
tidy(gmb_model)

ggplot(twitter_gmb_summary, aes(x = date, y = month_count)) +
  geom_line() +
  geom_point()

ggplot(twitter_gmb_summary, aes(x = date, y = sentiment_mean)) +
  geom_line() +
  geom_point()

ggplot(oil_price_monthly_summary, aes(x = date, y = price_mean)) +
  geom_line() +
  geom_point()


