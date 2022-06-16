## Binding tweets collected

require("tidyverse", quietly = TRUE)
require("rtweet", quietly = TRUE)

tweet_df <- tibble()
i <- 1

while(i < 12150002) {
  cur_tweet <- read_twitter_csv(paste("data/twitter_raw/tweet_", i, ".csv", sep = ""))
  tweet_df <- rbind(tweet_df, cur_tweet)
  i <- i + 90000
}

save_as_csv(tweet_df, "data/tweet_new.csv")
