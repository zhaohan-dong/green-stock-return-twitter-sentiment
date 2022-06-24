## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)
require(ggfortify, quietly = TRUE)
require(moments, quietly = TRUE)

analysis_df <- read_csv("data/combined_monthly_data.csv")

# Create multiple linear regression model
clean_price_model <- lm(log(price_mean) ~  monthly_count + oil_price_mean, data = analysis_df)
summary(clean_price_model)
autoplot(clean_price_model)
oil_gas_price_model <- lm(log(price_mean) ~  monthly_count + lag1_count + oil_price_mean, data = analysis_df)
summary(oil_gas_price_model)
autoplot(oil_gas_price_model)
bmg_price_model <- lm(log(price_mean) ~  monthly_count + lag1_count + oil_price_mean, data = analysis_df)
summary(bmg_price_model)
autoplot(bmg_price_model)

skewness(analysis_df$gmb_return)
skewness(log(1 + analysis_df$gmb_return))
