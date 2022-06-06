## Analyzing the twitter sentiment
# Note: set working directory as the parent directory of this file

# Load required packages
require("rtweet", quietly = TRUE)
require("tidyverse", quietly = TRUE)
require("vader", quietly = TRUE)

# Load the dataset
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



