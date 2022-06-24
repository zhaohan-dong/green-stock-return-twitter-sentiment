## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)
require(ggfortify, quietly = TRUE)

analysis_df <- read_csv("data/combined_monthly_data.csv")

