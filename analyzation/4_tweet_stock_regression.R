## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(broom, quietly = TRUE)
require(ggfortify, quietly = TRUE)
require(car, quietly = TRUE)
require(DescTools, quietly = TRUE)

# Import and merge stock data and green etf data
analysis_df <- read_csv("data/combined_monthly_data.csv") %>%
  mutate(oil_gas_return = oil_gas_return * 100) %>%
  mutate(clean_return = clean_return * 100) %>%
  mutate(utilities_return = utilities_return * 100) %>%
  mutate(gmb_return = gmb_return * 100) %>%
  filter(date < "2018-05-01" & date >= "2012-02-01")
green_factor <- read_csv("data/green_etf_factor.csv")
analysis_df <- merge(analysis_df, green_factor)

# Create multiple linear regression model for return
# Change regression terms to test hypothesis

clean_return_model <- lm(clean_return - RF ~
                           lag(monthly_tweet_count, n = 0) +
                           Mkt_RF + SMB + HML + log(wti_oil_price),
                         data = analysis_df)
# Test linear regression assumptions
summary(clean_return_model)
durbinWatsonTest(clean_return_model)
vif(clean_return_model)
autoplot(clean_return_model)

oil_gas_return_model <- lm(oil_gas_return - RF ~ 
                             lag(monthly_tweet_count, n = 0) +
                             Mkt_RF + SMB + HML + log(wti_oil_price),
                           data = analysis_df)
# Test linear regression assumptions
summary(oil_gas_return_model)
durbinWatsonTest(oil_gas_return_model)
vif(oil_gas_return_model)
autoplot(oil_gas_return_model)

utilities_return_model <- lm(utilities_return - RF ~
                               lag(monthly_tweet_count, n = 0) +
                               Mkt_RF + SMB + HML + log(wti_oil_price),
                           data = analysis_df)
# Test linear regression assumptions
summary(utilities_return_model)
durbinWatsonTest(utilities_return_model)
vif(utilities_return_model)
autoplot(utilities_return_model)

gmb_return_model <- lm(gmb_return - RF ~
                         lag(monthly_tweet_count, n = 0) +
                         Mkt_RF + SMB + HML + log(wti_oil_price),
                       data = analysis_df)
# Test linear regression assumptions
summary(gmb_return_model)
durbinWatsonTest(gmb_return_model)
vif(gmb_return_model)
autoplot(gmb_return_model)

# # Create multiple linear regression model for volume, not implemented
# clean_ar <- arima(log(analysis_df$clean_volume), order = c(1, 0, 0))
# res <- as_tibble(clean_ar$residuals)
# analysis_df < analysis_df %>% cbind(res, factors.exclude = FALSE)
# clean_volume_model <- lm(log(clean_volume) ~
#                            lag(monthly_tweet_count, 0) + log(wti_spot_price),
#                          data = analysis_df)
# summary(clean_volume_model)
# durbinWatsonTest(clean_volume_model)
# autoplot(clean_volume_model)
# 
# oil_gas_volume_model <- lm(log(oil_gas_volume) ~
#                              lag(monthly_tweet_count, 0) + log(wti_spot_price),
#                            data = analysis_df)
# summary(oil_gas_volume_model)
# durbinWatsonTest(oil_gas_volume_model)
# plot(lag(analysis_df$monthly_tweet_count), oil_gas_volume_model$residuals)
# autoplot(oil_gas_volume_model)
# 
# utilities_volume_model <- lm(log(utilities_volume) ~
#                              lag(monthly_tweet_count, 0) + log(wti_spot_price),
#                            data = analysis_df)
# summary(utilities_volume_model)
# durbinWatsonTest(utilities_volume_model)
# vif(utilities_volume_model)
# autoplot(utilities_volume_model)
# 
# gmb_volume_model <- lm(log(clean_volume + oil_gas_volume) ~
#                                lag(monthly_tweet_count, 0) + log(wti_spot_price),
#                              data = analysis_df)
# summary(gmb_volume_model)
# durbinWatsonTest(gmb_volume_model)
# vif(gmb_volume_model)
# coeftest(gmb_volume_model, vcov=NeweyWest(gmb_volume_model))
# autoplot(gmb_volume_model)

# Plotting
p <- ggplot(analysis_df)

p + geom_line(aes(y = monthly_tweet_count, x = date), color="deepskyblue") +
  theme_classic() +
  ylab("Monthly Tweet Count") + 
  xlab("Date")

p + geom_line(aes(y = green_etf, x = date), color="darkgreen") +
  theme_classic() +
  ylab("Green ETF Index") + 
  xlab("Date")

p + geom_line(aes(y = utilities_stock_monthly_sharpe, x = date), color="darkgreen") +
  theme_classic() +
  ylab("sharpe") + 
  xlab("Date")
