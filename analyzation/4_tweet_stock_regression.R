## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)

# Load data and convert date column from character to date type
twitter_df <- read_twitter_csv("data/tweet_sentiment.csv")
gmb_monthly_summary <- read_csv("data/gmb_monthly_summary.csv") %>%
  filter(date < as.Date("2018-05-01"))
wx_df <- read_csv("data/KNYC_monthly_summary_processed.csv") %>%
  mutate(date = as.Date(date))
oil_df <- read_csv("data/Cushing_OK_WTI_Spot_Price_FOB.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

# Removing tweet data not contingent for research
twitter_df <- twitter_df %>%
  mutate(date = as.Date(created_at), .after = created_at) %>%
  select(-created_at) %>%
  select(date, status_id, retweet_count, quote_count, reply_count, sentiment) %>%
  filter(date >= as.Date("2011-04-01"))

# Construct summary of twitter mean and standard dev, as well as tweet No. per day
twitter_summary <- twitter_df %>%
  group_by(date) %>%
  summarize(across(sentiment, list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)))) %>%
  mutate(count(twitter_df, date, name = "daily_count"))


twitter_summary <- twitter_summary %>%
  mutate(yr_mon = format(date, "%Y-%m"), .after = date) %>%
  group_by(yr_mon) %>%
  mutate(month_count = sum(daily_count)) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  filter(date >= as.Date("2012-04-01")) %>%
  select(-daily_count, -date)

twitter_gmb_summary <- full_join(gmb_monthly_summary, twitter_summary)

cor(twitter_gmb_summary$daily_raw_return_sd, twitter_gmb_summary$month_count)

test_df <- lm(daily_raw_return_sd ~ month_count, data = twitter_gmb_summary)

tidy(test_df)
