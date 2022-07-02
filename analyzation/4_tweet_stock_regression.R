## Regression between tweets and stock

require(tidyquant, quietly = TRUE)
require(tidyverse, quietly = TRUE)
require(broom, quietly = TRUE)
require(ggfortify, quietly = TRUE)
require(car, quietly = TRUE)
require(DescTools, quietly = TRUE)

# Import and merge data
analysis_df <- read_csv("data/combined_monthly_data.csv") %>%
  mutate(oil_gas_return = oil_gas_return * 100) %>%
  mutate(clean_return = clean_return * 100) %>%
  mutate(utilities_return = utilities_return * 100) %>%
  mutate(gmb_return = gmb_return * 100) %>%
  filter(date < "2018-05-01")
green_factor <- read_csv("data/green_etf_factor.csv")
analysis_df <- merge(analysis_df, green_factor)

model <- arima(analysis_df$monthly_tweet_count, order = c(1, 0, 0))
analysis_df <- analysis_df %>% mutate(ar1_res = c(model$residuals))

# Plot playground
p <- ggplot(analysis_df)
p + 
  #geom_line(aes(y = lag((clean_return - RF) * 10, n = 0), x = date), color="red") +
  # geom_line(aes(y = lag(log10(total_flow), 0), x = date), color="blue") +
  geom_line(aes(y = lag(scale(green_etf), n = 0), x = date), color="blue") +
  geom_line(aes(y = lag(Winsorize(tweet_count_change_perc), n = 1), x = date), color="orange")

cor.test(Winsorize(analysis_df$green_etf), lag(Winsorize(analysis_df$tweet_count_change_perc, na.rm = TRUE), 5))

new_df <- analysis_df %>%
  select(date, green_etf, tweet_count_change_perc) %>%
  mutate(green_etf = case_when(green_etf > 0 ~ 1,
                               green_etf == 0 ~ 0,
                               green_etf < 0 ~ -1),
         tweet_count_change_perc = case_when(tweet_count_change_perc > 0 ~ 1,
                                             tweet_count_change_perc == 0 ~ 0,
                                             tweet_count_change_perc < 0 ~ -1))

q <- ggplot(new_df)
q +   geom_line(aes(y = lag(green_etf, n = 0), x = date), color="blue") +
  geom_line(aes(y = lag(tweet_count_change_perc, n = 0), x = date), color="orange")
new_model <- lm(scale(green_etf) ~ lag(scale(ar1_res), n = 1), data = analysis_df)
summary(new_model)

# Create multiple linear regression model for return
clean_return_model <- lm(clean_return - RF ~  lag(monthly_tweet_count, n = 3) +
                           Mkt_RF + SMB + HML,
                         data = analysis_df)
summary(clean_return_model)
durbinWatsonTest(clean_return_model)
vif(clean_return_model)
autoplot(clean_return_model)
cor(resid(clean_return_model), analysis_df$monthly_tweet_count)

## OG significantly explained by lag 1 monthly tweet count
oil_gas_return_model <- lm(oil_gas_return - RF ~ 
                             Mkt_RF + SMB + HML,
                           data = analysis_df)
summary(oil_gas_return_model)
oil_gas_return_model$residuals
durbinWatsonTest(oil_gas_return_model)
vif(oil_gas_return_model)
autoplot(oil_gas_return_model)

hist(analysis_df$sentiment_mean)

utilities_return_model <- lm(utilities_return - RF ~
                             monthly_tweet_count + sentiment_mean + log(wti_spot_price) +
                             Mkt_RF + SMB + HML,
                           data = analysis_df)
summary(utilities_return_model)
vif(utilities_return_model)
autoplot(utilities_return_model)

gmb_return_model <- lm(gmb_return - RF ~
                         monthly_tweet_count + sentiment_mean + log(wti_spot_price) + Mkt_RF + SMB + HML + ab_temp,
                       data = analysis_df)
summary(gmb_return_model)
vif(gmb_return_model)
autoplot(gmb_return_model)

# Skewness tests for log fitting
skewness(analysis_df$oil_gas_return - analysis_df$RF)
skewness(log(analysis_df$ab_temp))
skewness(log(1 + analysis_df$oil_gas_return - analysis_df$RF))

# Create multiple linear regression model for volume
clean_volume_model <- lm(log(clean_volume) ~
                           lag(monthly_tweet_count) + log(wti_spot_price),
                         data = analysis_df)
summary(clean_volume_model)
autoplot(clean_volume_model)

oil_gas_volume_model <- lm(log(oil_gas_volume) ~
                             lag(monthly_tweet_count) / lag(monthly_tweet_count, 2) + log(wti_spot_price),
                           data = analysis_df)
summary(oil_gas_volume_model)
hist(oil_gas_volume_model$residuals)
plot(lag(analysis_df$monthly_tweet_count), oil_gas_volume_model$residuals)
autoplot(oil_gas_volume_model)

utilities_volume_model <- lm(log(utilities_volume) ~
                             lag(monthly_tweet_count),
                           data = analysis_df)
summary(utilities_volume_model)
vif(utilities_volume_model)
autoplot(utilities_volume_model)

cor(log(analysis_df$wti_spot_price), lag(analysis_df$monthly_tweet_count, 2), use = "complete.obs")


analysis_df$res <- c(NA, NA, residuals(oil_gas_return_model))


test <- lm(lag(log(analysis_df$total_flow), n = 8) ~ lag(analysis_df$monthly_tweet_count / 1000000, n = 0), data = analysis_df, na.action = na.omit)
summary(test)
cor.test(lag(analysis_df$total_flow, n = 8), lag(analysis_df$monthly_tweet_count, n = 0), use = "complete.obs")
summary()
