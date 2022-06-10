## We will cleanse the equity daily closing data
## Then we will calculate raw daily & monthly return and
## risk-adjusted return using fama-french three factor model

require("tidyverse", quietly = TRUE)

# Define function to cleanse data
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

clean_stock <- read_csv("data/equity/clean_selected.csv") %>%
  cleanse_stock_data() %>%
  mutate(category = "clean")

oil_gas_stock <- read_csv("data/equity/oil_gas_coal_selected.csv") %>%
  cleanse_stock_data() %>%
  mutate(category = "oil-gas")

utility_stock <- read_csv("data/equity/utility_selected.csv") %>%
  cleanse_stock_data() %>%
  mutate(category = "utility")

# Calculate daily raw return
price <- rbind(clean_stock, oil_gas_stock, utility_stock) %>%
  filter(data_field == "PX_LAST") %>%
  group_by(ticker) %>%
  mutate(lag1 = lag(value, n = 1L, order_by = date)) %>%
  mutate(daily_raw_return = (value - lag1) / lag1) %>%
  select(!c(data_field, lag1)) %>%
  ungroup() %>%
  rename(price = value)

# Calculate monthly raw return

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
  right_join(y = price) %>%
  # Delete yr_mon column
  select(!yr_mon)


# Load Fama-French three-factor model data
ff_model_data <- read_csv("data/equity/F-F_Research_Data_Factors_daily.csv") %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d"))
