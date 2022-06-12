## Regression between tweets and stock

require(tidyverse, quietly = TRUE)

# Load data
twitter_df <- read_csv("data/tweet_sentiment.csv")
stock_equal_weight_raw <- read_csv("data/stock_equal_weight_raw.csv")
stock_equal_weight_ff <- read_csv("data/stock_equal_weight_ff.csv")
wx_df <- read_csv("data/KNYC_monthly_summary_processed.csv")
oil_df <- read_csv("data/Cushing_OK_WTI_Spot_Price_FOB.csv")
