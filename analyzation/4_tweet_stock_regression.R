## Regression between tweets and stock price

require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(rtweet, quietly = TRUE)
require(ggfortify, quietly = TRUE)

###################################################################

# Create multiple linear regression model
clean_price_model <- lm(log(price_mean) ~  monthly_count + oil_price_mean, data = twitter_clean_summary)
summary(clean_price_model)
autoplot(clean_price_model)
oil_gas_price_model <- lm(log(price_mean) ~  monthly_count + lag1_count + oil_price_mean, data = twitter_oil_gas_summary)
summary(oil_gas_price_model)
autoplot(oil_gas_price_model)
bmg_price_model <- lm(log(price_mean) ~  monthly_count + lag1_count + oil_price_mean, data = twitter_bmg_summary)
summary(bmg_price_model)
autoplot(bmg_price_model)

ggplot(twitter_clean_summary, aes(x = log(price_mean), y = log(monthly_count))) +
  geom_point()


# Return
clean_return_model <- lm(daily_raw_return_mean ~  monthly_count + oil_price_change_perc + Mkt_RF, data = twitter_clean_summary)
oil_gas_return_model <- lm(daily_raw_return_mean ~  monthly_count + oil_price_change_perc + Mkt_RF, data = twitter_oil_gas_summary)
bmg_return_model <- lm(daily_raw_return_mean ~  monthly_count + oil_price_change_perc + Mkt_RF, data = twitter_bmg_summary)
summary(clean_return_model)
autoplot(clean_return_model)
summary(oil_gas_return_model)
autoplot(clean_return_model)
summary(bmg_return_model)
autoplot(bmg_return_model)

# # Can't do these
# # Return standard deviation, the more people tweets, the more return sd it is
# clean_return_sd_model <- lm(sqrt(daily_raw_return_sd) ~  log(monthly_count) + sqrt(oil_price_sd) + sqrt(spx_price_sd), data = twitter_clean_summary)
# summary(clean_return_sd_model)
# autoplot(clean_return_sd_model)
# oil_gas_return_sd_model <- lm(sqrt(daily_raw_return_sd) ~ log(monthly_count) + sqrt(oil_price_sd) + sqrt(spx_price_return_sd), data = twitter_oil_gas_summary)
# summary(oil_gas_return_sd_model)
# autoplot(oil_gas_return_sd_model)
# bmg_return_sd_model <- lm(daily_raw_return_sd ~  log(monthly_count) + log(oil_price_sd), data = twitter_bmg_summary)
# summary(bmg_return_sd_model)
# autoplot(bmg_return_sd_model)

ggplot(twitter_bmg_summary, aes(x = date, y = log(monthly_count))) +
  geom_line() +
  geom_point()

ggplot(twitter_bmg_summary, aes(x = date, y = log(oil_price_mean))) +
  geom_line() +
  geom_point()

ggplot(twitter_bmg_summary, aes(x = date, y = price_mean)) +
  geom_line() +
  geom_point()

ggplot(twitter_bmg_summary, aes(x = date, y = sentiment_mean)) +
  geom_line() +
  geom_point()

ggplot(oil_price_monthly_summary, aes(x = date, y = price_mean)) +
  geom_line() +
  geom_point()


