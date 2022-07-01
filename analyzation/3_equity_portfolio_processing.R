## Combining the equity portfolio price and volume data
## Fama French 3 factors
## S&P 500 and oil price
# NOTE: Output date floored to first day of the month


# Load libraries
require(tidyverse, quietly = TRUE)
require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(rtweet, quietly = TRUE)

## Define functions
# Cleanse data input
cleanse_stock_data <- function(df) {
  result_df <- df %>%
    pivot_longer(cols = !c(ticker, data_field),
                 names_to ="date",
                 values_to = "value",
                 values_ptypes = numeric()) %>%
    mutate(date = as.Date(date, "%m/%d/%Y"),
           ticker = sub(" US Equity", "", ticker))
  return(result_df)
}

# Create dataframes for equal weighted portfolio from cleansed csv
get_price <- function(df) {
  # Select and tidy data
  result_df <- df %>%
    filter(data_field == "PX_LAST") %>%
    select(!data_field) %>%
    pivot_wider(names_from = ticker,
                values_from = value) %>%
    # Replace NA values with previous value from column (same ticker)
    na.locf()
  return(result_df)
}

# NOTE: NOT IMPLEMENTED
# Create dataframes for value weighted portfolio from cleansed csv
# value_weight_portfolio <- function(df) {
#   # Select and tidy data
#   result_df <- df %>%
#     filter(data_field == c("PX_LAST", "CUR_MKT_CAP")) %>%
#     pivot_wider(names_from = c(ticker, data_field),
#                 values_from = value) %>%
#     # Replace NA values with previous value from column (same ticker)
#     na.locf() %>%
#     mutate(mkt_cap_sum = rowSums(select(., ends_with("_CUR_MKT_CAP"))))
#   
#   # Add month helper column
#   result_df$yr_mon <- format(result_df$date, "%Y-%m")
#   
#   return(result_df)
# }

# # Model fitting for multiple groups with ff model
# # Solution on: https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr
# fit_ff_model <- function(portfolio_df) {
#   result_df <- portfolio_df %>%
#     group_by(yr_mon, category) %>%
#     group_modify(
#       # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
#       ~ tidy(lm(R_excess ~ MKT_RF + SMB + HML, data = .))
#     )
# }
# 
# # Fit CAPM model because of unsatisfactory ff-model
# fit_capm_model <- function(portfolio_df) {
#   result_df <- portfolio_df %>%
#     group_by(yr_mon, category) %>%
#     group_modify(
#       # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
#       ~ tidy(lm(R_excess ~ MKT_RF, data = .))
#     )
# }

# Calculate daily volume
get_volume <- function(df) {
  # Select and tidy data
  result_df <- df %>%
    filter(data_field == "PX_VOLUME") %>%
    select(!data_field) %>%
    pivot_wider(names_from = ticker,
                values_from = value) %>%
    # Replace NA values with previous value from column (same ticker)
    na.locf()
  return(result_df)
}

############################################################################
## Beginning of task
# Read stock data
clean_stock <- read_csv("data/equity/clean_selected.csv") %>%
  cleanse_stock_data() %>%
  filter(data_field == "PX_LAST")

oil_gas_stock <- read_csv("data/equity/oil_gas_coal_selected.csv") %>%
  cleanse_stock_data() %>%
  filter(data_field == "PX_LAST")

utilities_stock <- read_csv("data/equity/utilities_selected.csv") %>%
  cleanse_stock_data() %>%
  filter(data_field == "PX_LAST")

spx <- read_csv("data/equity/spx.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  select(-volume) %>%
  tq_transmute(price, mutate_fun = periodReturn, period = "monthly",
               col_rename = "spx_return") %>%
  mutate(date = floor_date(date, unit = "month"))

# Create equal weight portfolio and calculate monthly portfolio raw return
clean_stock_monthly_return <- clean_stock %>%
  group_by(ticker) %>%
  tq_transmute(value, mutate_fun = periodReturn, period = "monthly") %>%
  # Calculate portfolio return, change weight here for other weighting
  tq_portfolio(assets_col = ticker, returns_col = monthly.returns,
               col_rename = "clean_return") %>%
  mutate(date = floor_date(date, unit = "month"))
  
oil_gas_stock_monthly_return <- oil_gas_stock %>%
  group_by(ticker) %>%
  tq_transmute(value, mutate_fun = periodReturn, period = "monthly") %>%
  # Calculate portfolio return, change weight here for other weighting
  tq_portfolio(assets_col = ticker, returns_col = monthly.returns,
               col_rename = "oil_gas_return") %>%
  mutate(date = floor_date(date, unit = "month"))

utilities_stock_monthly_return <- utilities_stock %>%
  group_by(ticker) %>%
  tq_transmute(value, mutate_fun = periodReturn, period = "monthly") %>%
  # Calculate portfolio return, change weight here for other weighting
  tq_portfolio(assets_col = ticker, returns_col = monthly.returns,
               col_rename = "utilities_return") %>%
  mutate(date = floor_date(date, unit = "month"))

# Create Green minus brown long-short equal weight portfolio
gmb_monthly_return <- clean_stock_monthly_return %>%
  full_join(oil_gas_stock_monthly_return) %>%
  mutate(oil_gas_return = -oil_gas_return) %>%
  pivot_longer(cols = c(clean_return, oil_gas_return)) %>%
  # Calculate return, has 23 green stocks and 52 oil/gas stocks
  tq_portfolio(assets_col = name, returns_col = value,
               weights = c(23/75, 52/75), col_rename = "gmb_return")

# Read volumes for portfolios
clean_stock_vol <- read_csv("data/equity/clean_selected.csv") %>%
  cleanse_stock_data() %>%
  get_volume() %>%
  transmute(date = floor_date(date, unit = "month"),
            total_volume = rowSums(select(., -date))) %>%
  group_by(date) %>%
  transmute(clean_volume = sum(total_volume)) %>%
  ungroup() %>%
  unique() %>% # Delete redundant entries per month
  # Calculate volume percentage change
  tq_mutate(clean_volume, mutate_fun = periodReturn, period = "monthly",
               col_rename = "clean_stock_vol_change_perc")

oil_gas_stock_vol <- read_csv("data/equity/oil_gas_coal_selected.csv") %>%
  cleanse_stock_data() %>%
  get_volume() %>%
  transmute(date = floor_date(date, unit = "month"),
            total_volume = rowSums(select(., -date))) %>%
  group_by(date) %>%
  transmute(oil_gas_volume = sum(total_volume)) %>%
  ungroup() %>%
  unique() %>% # Delete redundant entries per month
  # Calculate volume percentage change
  tq_mutate(oil_gas_volume, mutate_fun = periodReturn, period = "monthly",
               col_rename = "oil_gas_stock_vol_change_perc")

utilities_stock_vol <- read_csv("data/equity/utilities_selected.csv") %>%
  cleanse_stock_data() %>%
  get_volume() %>%
  transmute(date = floor_date(date, unit = "month"),
            total_volume = rowSums(select(., -date))) %>%
  group_by(date) %>%
  transmute(utilities_volume = sum(total_volume)) %>%
  ungroup() %>%
  unique() %>% # Delete redundant entries per month
  # Calculate volume percentage change
  tq_mutate(utilities_volume, mutate_fun = periodReturn, period = "monthly",
               col_rename = "utilities_stock_vol_change_perc")

# Initialize output dataframe
output_df <- spx %>%
  merge(clean_stock_monthly_return) %>%
  merge(oil_gas_stock_monthly_return) %>%
  merge(utilities_stock_monthly_return) %>%
  merge(gmb_monthly_return) %>%
  merge(clean_stock_vol) %>%
  merge(oil_gas_stock_vol) %>%
  merge(utilities_stock_vol) %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))

# Clean up memory
rm(spx, clean_stock_monthly_return, oil_gas_stock_monthly_return,
             utilities_stock_monthly_return, gmb_monthly_return,
             clean_stock_vol, oil_gas_stock_vol, utilities_stock_vol)
gc()

################################################################################
# Stock data merge completed
# Load monthly FF factors
ff_monthly <- read_csv("data/equity/F-F_Research_Data_Factors_monthly.csv") %>%
  rename(Mkt_RF = `Mkt-RF`) %>%
  mutate(date = as.Date(paste0(as.character(date), "01"), format = "%Y%m%d"))

# Load WTI oil price
# oil_df <- read_csv("data/Cushing_OK_WTI_Spot_Price_FOB.csv") %>%
#   mutate(date = as.Date(date, "%m/%d/%Y")) %>%
#   rename(wti_spot_price = `dollars per barrel`) %>%
#   tq_transmute(wti_spot_price, mutate_fun = to.period, period = "month",
#                col_rename = "wti_spot_price") %>%
#   tq_mutate(wti_spot_price, mutate_fun = periodReturn,
#             col_rename = "wti_return") %>%
#   # Floor the date monthly to merge with other dataframes
#   mutate(date = floor_date(date, unit = "month"))
oil_df <- read_csv("data/Cushing_OK_WTI_Spot_Price_FOB_Monthly.csv") %>%
  mutate(date = as.Date(paste0("01-", date), "%d-%b-%y")) %>%
  rename(wti_spot_price = `dollars per barrel`) 


# Load twitter data
twitter_summary <- read_twitter_csv("data/twitter_summary.csv") %>%
  # Calculate monthly tweet count change
  mutate(date = as.Date(date),
         tweet_count_change_perc =
           monthly_tweet_count / lag(monthly_tweet_count) - 1)

# Twitter mau
twitter_mau <- read_csv("data/twitter_mau.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))
months <- lapply(X = twitter_mau$date, FUN = seq.Date, by = "month", length.out = 3)
months <- data.frame(date = do.call(what = c, months))
twitter_mau <- left_join(x = months, y = twitter_mau, by = "date")
twitter_mau$mau <- na.spline(object = twitter_mau$mau) # Extrapolate mau per month from per quarter data
twitter_summary <- twitter_summary %>%
  left_join(y = twitter_mau) %>%
  mutate(count_per_mau = monthly_tweet_count / mau)

# Load processed NYC weather data
knyc_wx <- read_csv("data/KNYC_monthly_summary_processed.csv") %>%
  select(date, ab_temp)

# Merge output dataframe with FF factors, WTI spot price and twitter summary
output_df <- output_df %>%
  merge(ff_monthly) %>%
  merge(oil_df) %>%
  right_join(twitter_summary) %>%
  merge(knyc_wx) %>%
  filter(date > as.Date("2010-01-01") & date < as.Date("2018-05-01"))

# Save output dataframe to csv
write_csv(output_df, "data/combined_monthly_data.csv")

# Clean up memory
rm(list = ls())
gc()
