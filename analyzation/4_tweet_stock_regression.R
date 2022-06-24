## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(broom, quietly = TRUE)
require(ggfortify, quietly = TRUE)
require(car, quietly = TRUE)

analysis_df <- read_csv("data/combined_monthly_data.csv")

# Create multiple linear regression model for return
clean_return_model <- lm(clean_return - RF ~  Mkt_RF,
                         data = analysis_df)
summary(clean_return_model)
vif(clean_return_model)
autoplot(clean_return_model)
cor(resid(clean_return_model), analysis_df$monthly_tweet_count)

## OG significantly explained by lag 1 monthly tweet count
oil_gas_return_model <- lm(oil_gas_return - RF ~
                             lag(monthly_tweet_count) + log(wti_spot_price) +
                             Mkt_RF + SMB + HML,
                           data = analysis_df)
summary(oil_gas_return_model)
vif(oil_gas_return_model)
autoplot(oil_gas_return_model)

utility_return_model <- lm(utility_return - RF ~
                             lag(monthly_tweet_count) + log(wti_spot_price) + Mkt_RF,
                           data = analysis_df)
summary(utility_return_model)
vif(utility_return_model)
autoplot(utility_return_model)

gmb_return_model <- lm(gmb_return - RF ~
                         lag(monthly_tweet_count) + log(wti_spot_price) + Mkt_RF,
                       data = analysis_df)
summary(gmb_return_model)
vif(gmb_return_model)
autoplot(gmb_return_model)

# Skewness tests for log fitting
skewness(analysis_df$utility_return - analysis_df$RF)
skewness(log(analysis_df$utility_return))
skewness(log(1 + analysis_df$utility_return - analysis_df$RF))

# Create multiple linear regression model for volume
clean_volume_model <- lm(clean_volume ~
                           monthly_tweet_count + wti_spot_price,
                         data = analysis_df)
summary(clean_volume_model)
autoplot(clean_volume_model)

oil_gas_volume_model <- lm(oil_gas_volume ~
                             lag(monthly_tweet_count) + wti_spot_price,
                           data = analysis_df)
summary(oil_gas_volume_model)
autoplot(oil_gas_volume_model)

utility_volume_model <- lm(utility_volume ~
                             monthly_tweet_count + wti_spot_price,
                           data = analysis_df)
summary(utility_volume_model)
autoplot(utility_volume_model)
