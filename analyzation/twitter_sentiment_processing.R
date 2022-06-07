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

# Substitute [at]username with anonymous
tweet_df$text <- gsub("@[a-zA-Z0-9]*", "@anonymous", tweet_df$text)
tweet_df$quoted_text <- gsub("@[a-zA-Z1-9 ]*", "@anonymous", tweet_df$quoted_text)

# Process through vader
tweet_df$sentiment <- vader_df(tweet_df$text, neu_set=T)$compound

# Save file
save_as_csv(tweet_df, "data/tweet_sentiment.csv")
