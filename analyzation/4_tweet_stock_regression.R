## Regression between tweets and stock

require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)

# Load data
twitter_df <- read_twitter_csv("data/tweet_sentiment.csv")
stock_equal_weight_raw <- read_csv("data/stock_equal_weight_raw.csv")
stock_equal_weight_ff <- read_csv("data/stock_equal_weight_ff.csv")
wx_df <- read_csv("data/KNYC_monthly_summary_processed.csv") %>%
  mutate(DATE = as.Date(DATE))
oil_df <- read_csv("data/Cushing_OK_WTI_Spot_Price_FOB.csv")

twitter_df <- twitter_df %>%
  mutate(date = as.Date(created_at), .after = created_at) %>%
  select(-created_at) %>%
  select(date, status_id, retweet_count, quote_count, reply_count, sentiment) %>%
  filter(date >= as.Date("2011-04-01"))

twitter_df <- twitter_df %>%
  group_by(date) %>%
  summarize(across(sentiment, lst(mean, sd)))


ggplot(data=twitter_df, aes(x=date, y=sentiment_mean, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=wx_df, aes(x=DATE, y=ab_temp, group=1)) +
  geom_line()+
  geom_point()
