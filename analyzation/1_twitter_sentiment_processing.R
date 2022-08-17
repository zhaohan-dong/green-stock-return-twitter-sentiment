## Analyzing the twitter sentiment
# Note: set working directory as the parent directory of this file

# Load required packages
require("rtweet", quietly = TRUE)
require("tidyverse", quietly = TRUE)
require("vader", quietly = TRUE)
require("lubridate", quietly = TRUE)

# Load the dataset, tweet.csv created by
# merging all tweets csv from scrape_rtweet.Rmd using bind_tweet.R
tweet_df <- read_twitter_csv("data/tweet.csv")

# View(head(tweet_df))

# Select only columns that might be used and filter by the keywords in text/quoted_text
tweet_df <- tweet_df %>%
  select(c("user_id",
           "status_id",
           "created_at",
           "text",
           "retweet_count",
           "quote_count",
           "reply_count",
           "hashtags",
           "quoted_status_id",
           "quoted_text",
           "quoted_created_at",
           "quoted_user_id")) %>%
  filter((grepl("climate change|#climatechange|global warming|#globalwarming", text, ignore.case = TRUE) |
           grepl("climate change|#climatechange|global warming|#globalwarming", text, ignore.case = TRUE)))

# Substitute [at]username with anonymous
tweet_df$text <- gsub("@[a-zA-Z0-9]*", "@anonymous", tweet_df$text)
tweet_df$quoted_text <- gsub("@[a-zA-Z1-9 ]*", "@anonymous", tweet_df$quoted_text)

# Process through vader
tweet_df$sentiment <- vader_df(tweet_df$text, neu_set=T)$compound

# Save file
save_as_csv(tweet_df, "data/tweet_sentiment.csv")

# Load twitter data
twitter_df <- read_twitter_csv("data/tweet_sentiment.csv")
# Removing tweet data not contingent for research
twitter_df <- twitter_df %>%
  mutate(date = as.Date(created_at), .after = created_at) %>%
  select(-created_at) %>%
  select(date, status_id, retweet_count, quote_count,
         reply_count, sentiment)
# Construct summary of twitter mean and standard dev
# as well as tweet No. per month
twitter_summary <- twitter_df %>%
  mutate(date = floor_date(date, unit = "month"))
twitter_summary <- twitter_summary %>%
  merge(count(twitter_summary, date, name = "monthly_tweet_count")) %>%
  group_by(date) %>%
  summarize(date, across(sentiment,
                         list(mean = ~ mean(.x, na.rm = TRUE),
                              sd = ~ sd(.x, na.rm = TRUE))),
            monthly_tweet_count) %>%
  ungroup() %>%
  distinct()

save_as_csv(twitter_summary, "data/twitter_summary.csv")

# Clean up memory
rm(list = ls())
gc()
