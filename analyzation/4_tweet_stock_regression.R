## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)

# Load data and convert date column from character to date type
twitter_df <- read_twitter_csv("data/tweet_sentiment.csv")
stock_equal_weight_raw <- read_csv("data/stock_equal_weight_raw.csv") %>%
  mutate(date = as.Date(date)) %>%
  filter(category == "clean",
         date < as.Date("2018-05-01")) %>%
  group_by(yr_mon) %>%
  filter(date == min(date)) %>%
  ungroup()
stock_equal_weight_ff <- read_csv("data/stock_equal_weight_ff.csv") #%>%
  filter(category == "oil-gas", term == "(Intercept)")
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
  filter(date >= as.Date("2012-04-01"))

ggplot(data=twitter_summary, aes(x=date, y=sentiment_mean, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=twitter_summary, aes(x=date, y=month_count, group=1)) +
  geom_line()+
  scale_x_date()+
  geom_point()

ggplot(data=wx_df, aes(x=date, y=ab_temp, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=stock_equal_weight_raw, aes(x=date)) +
  geom_line(aes(y = R_excess), color = "darkred") + 
  geom_point(aes(y = R_ex)) +
  geom_line(aes(y = MKT_RF), color="steelblue", linetype="twodash") +
  geom_point(aes(y = MKT_RF))

cor(stock_equal_weight_raw$month_count, stock_equal_weight_raw$MKT_RF)
cor(stock_equal_weight_raw$R_excess, twitter_summary$month_count)
cor(twitter_summary$sentiment_mean, twitter_summary$month_count)


analysis_df <- inner_join(stock_equal_weight_ff, twitter_summary, by = "yr_mon")

model <- cor(analysis_df$estimate, analysis_df$month_count)


