## Regression between tweets and stock volume

require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)
require(ggfortify, quietly = TRUE)

twitter_df <- read_twitter_csv("data/tweet_sentiment.csv")