## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)
require(ggfortify, quietly = TRUE)
require(moments, quietly = TRUE)

analysis_df <- read_csv("data/combined_monthly_data.csv")

# Create multiple linear regression model
clean_return_model <- lm(clean_return - RF ~  Mkt_RF + SMB + HML, data = analysis_df)
summary(clean_return_model)
autoplot(clean_return_model)
cor(resid(clean_return_model), analysis_df$monthly_tweet_count)


## OG significantly explained by lag 1 monthly tweet count
oil_gas_return_model <- lm(oil_gas_return - RF ~  lag(monthly_tweet_count) + log(wti_spot_price) + Mkt_RF + SMB + HML, data = analysis_df)
summary(oil_gas_return_model)
autoplot(oil_gas_return_model)
cor(resid(oil_gas_return_model), analysis_df$lag1_tweet_count)

utility_return_model <- lm(utility_return ~ monthly_tweet_count + log(wti_spot_price) + Mkt_RF, data = analysis_df)
summary(utility_return_model)
autoplot(utility_return_model)
cor(resid(utility_return_model), analysis_df$monthly_tweet_count)

gmb_return_model <- lm(gmb_return ~ monthly_tweet_count + log(wti_spot_price) + Mkt_RF, data = analysis_df)
summary(gmb_return_model)
autoplot(gmb_return_model)

skewness(analysis_df$wti_spot_price)
skewness(log(analysis_df$wti_spot_price))
skewness(log(1 + analysis_df$gmb_return))

cor(analysis_df$wti_spot_price, analysis_df$lag1_tweet_count)

clean_volume_model <- lm(clean_volume ~ monthly_tweet_count + wti_spot_price, data = analysis_df)
summary(clean_volume_model)
autoplot(clean_volume_model)

oil_gas_volume_model <- lm(oil_gas_volume ~ monthly_tweet_count + wti_spot_price, data = analysis_df)
summary(oil_gas_volume_model)
autoplot(oil_gas_volume_model)

oil_tweet <- lm(log(wti_spot_price) ~ monthly_tweet_count, data = analysis_df)
summary(oil_tweet)
autoplot(oil_tweet)

ggplot(analysis_df, aes(x = date, y = monthly_tweet_count)) +
  geom_point()

test <- lm(lag1_tweet_count ~ lag(monthly_tweet_count), data = analysis_df)
summary(test)
