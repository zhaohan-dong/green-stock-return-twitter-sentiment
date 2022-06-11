## We will cleanse the equity daily closing data
## Then we will calculate raw daily & monthly return and
## risk-adjusted return using fama-french three factor model

#library(tidyquant)
#library(timetk)
#library(broom)
#library(glue)

# Load libraries
require(tidyverse, quietly = TRUE)
require(broom, quietly = TRUE)



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
equal_weight_portfolio <- function(df) {
  # Select and tidy data
  result_df <- df %>%
    filter(data_field == "PX_LAST") %>%
    select(!data_field) %>%
    pivot_wider(names_from = ticker,
                values_from = value) %>%
    # Replace NA values with previous value from column (same ticker)
    na.locf()
  
  # Calculate return
  result_df <- result_df %>%
    mutate(price = rowSums(result_df %>% select(!c(date, category))) /
             (ncol(result_df) - 2))
  
  return(result_df)
}

# Calculate daily raw return of portfolio from portfolio df
daily_raw_return <- function(portfolio_df) {
  result_df <- portfolio_df %>%
    mutate(lag1 = lag(price, n = 1L, order_by = date)) %>%
    mutate(daily_raw_return = (price - lag1) / lag1) %>%
    select(!lag1) %>%
    ungroup()
  return(result_df)
}

# Combine fama-french three-factor and portfolio df with daily return
# and calculate daily excess return + add year-month group column
combine_ff_model <- function(portfolio_df) {
  ff_model_data <- read_csv("data/equity/F-F_Research_Data_Factors_daily.csv") %>%
    mutate(date = as.Date(as.character(date), "%Y%m%d"))
  
  result_df <- portfolio_df %>%
    left_join(ff_model_data) %>%
    rename(MKT_RF = `Mkt-RF`) %>%
    mutate(Mkt-RF = Mkt-RF / 100,
           SMB = SMB / 100,
           HML = HML / 100,
           RF = RF / 100,
           R_excess = daily_raw_return - RF)
  
  result_df$yr_mon <- format(result_df$date, "%Y-%m")
  return(result_df)
}





## Beginning of task
# Read stock data
clean_stock <- read_csv("data/equity/clean_selected.csv") %>%
  cleanse_stock_data() %>%
  mutate(category = "clean")

oil_gas_stock <- read_csv("data/equity/oil_gas_coal_selected.csv") %>%
  cleanse_stock_data() %>%
  mutate(category = "oil-gas")

utility_stock <- read_csv("data/equity/utility_selected.csv") %>%
  cleanse_stock_data() %>%
  mutate(category = "utility")

# Create equal weight portfolio and calculate daily raw return
clean_stock_equal_weight <- equal_weight_portfolio(clean_stock) %>%
  daily_raw_return() %>%
  select(date, category, price, daily_raw_return) %>%
  filter(date != as.Date("2012-03-30")) # Delete head of data with no return info
oil_gas_stock_equal_weight <- equal_weight_portfolio(oil_gas_stock) %>%
  daily_raw_return() %>%
  select(date, category, price, daily_raw_return) %>%
  filter(date != as.Date("2012-03-30")) # Delete head of data with no return info
utility_stock_equal_weight <- equal_weight_portfolio(utility_stock) %>%
  daily_raw_return() %>%
  select(date, category, price, daily_raw_return) %>%
  filter(date != as.Date("2012-03-30")) # Delete head of data with no return info

clean_stock_equal_weight <- combine_ff_model(clean_stock_equal_weight)




# Calculate daily raw return
price <- rbind(clean_stock, oil_gas_stock, utility_stock) %>%
  filter(data_field == "PX_LAST") %>%
  group_by(ticker) %>%
  mutate(lag1 = lag(value, n = 1L, order_by = date)) %>%
  mutate(daily_raw_return = (value - lag1) / lag1) %>%
  select(!c(data_field, lag1)) %>%
  ungroup() %>%
  rename(price = value)

# Load Fama-French three-factor model data
ff_model_data <- read_csv("data/equity/F-F_Research_Data_Factors_daily.csv") %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d"))


fitted_model <- price %>%
  filter(!is.na(monthly_raw_return)) %>%
  group_by(ticker, yr_mon) %>%
  do(model = lm(R_excess ~ MKT_RF + SMB + HML, data = ., na.action = na.omit))



## Calculate monthly raw return
# Create groups by month using yr_mon column
price$yr_mon <- format(price$date, "%Y-%m")

price <- price %>%
  group_by(ticker, yr_mon) %>%
  
  # Select last day of month in the data
  filter(date == max(date)) %>%
  ungroup() %>%
  
  # Regroup by ticker and calculate monthly raw return
  group_by(ticker) %>%
  mutate(lag1 = lag(price)) %>%
  mutate(monthly_raw_return = (price - lag1) / lag1) %>%
  select(c(ticker, yr_mon, monthly_raw_return)) %>%
  ungroup() %>%
  
  # Merge resulting data with original dataframe
  right_join(y = price)




# Combine ff model data from percentage, left join to include only market open days
price <- left_join(price, ff_model_data) %>%
  rename(MKT_RF = `Mkt-RF`) %>%
  mutate(monthly_raw_return = monthly_raw_return * 100,
         daily_raw_return = daily_raw_return * 100,
         R_excess = daily_raw_return - RF) %>%
  
  # Delete the March 30 entry after calculating return for April 02
  filter(date != as.Date("2012-03-30"))



# test <- lm_table(price %>% filter(!is.na(monthly_raw_return)), R_excess ~ MKT_RF + SMB + HML, .groups = c("ticker", "yr_mon"))

fitted_model <- fitted_model %>%
  left_join(y = price[!duplicated(price[ ,c("yr_mon", "ticker", "monthly_raw_return")]), ] %>%
              select(yr_mon, ticker, monthly_raw_return, category))




