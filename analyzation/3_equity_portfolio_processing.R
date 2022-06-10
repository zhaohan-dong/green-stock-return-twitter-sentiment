## We will cleanse the equity daily closing data
## Then we will calculate raw daily & monthly return and
## risk-adjusted return using fama-french three factor model

require("tidyverse", quietly = TRUE)
require("xts", quietly = TRUE)

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
  ungroup()

# Calculate monthly raw return
price$mon_yr <- format(price$date, "%Y-%m")

price_temp <- price %>%
  group_by(ticker, mon_yr) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  group_by(ticker) %>%
  mutate(lag1 = lag(value)) %>%
  mutate(monthly_raw_return = (value - lag1) / lag1) %>%
  select(c(mon_yr, monthly_raw_return)) %>%
  ungroup()
