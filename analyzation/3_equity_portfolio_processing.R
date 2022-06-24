## Combining the equity portfolio price and volume data
## Fama French 3 factors
## S&P 500 and oil price
# NOTE: Output date floored to first day of the month


# Load libraries
require(tidyverse, quietly = TRUE)
require(tidyquant, quietly = TRUE)
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

utility_stock <- read_csv("data/equity/utility_selected.csv") %>%
  cleanse_stock_data() %>%
  filter(data_field == "PX_LAST")

spx <- read_csv("data/equity/spx.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  select(-volume) %>%
  tq_transmute(price, mutate_fun = periodReturn, period = "monthly", col_rename = "spx_return")

# Create equal weight portfolio and calculate daily raw return
# Deleting individual stock ticker data
clean_stock_monthly_return <- clean_stock %>%
  group_by(ticker) %>%
  tq_transmute(value, mutate_fun = periodReturn, period = "monthly") %>%
  tq_portfolio(assets_col = ticker, returns_col = monthly.returns, col_rename = "clean_return")
oil_gas_stock_monthly_return <- oil_gas_stock %>%
  group_by(ticker) %>%
  tq_transmute(value, mutate_fun = periodReturn, period = "monthly") %>%
  tq_portfolio(assets_col = ticker, returns_col = monthly.returns, col_rename = "oil_gas_return")
utility_stock_monthly_return <- utility_stock %>%
  group_by(ticker) %>%
  tq_transmute(value, mutate_fun = periodReturn, period = "monthly") %>%
  tq_portfolio(assets_col = ticker, returns_col = monthly.returns, col_rename = "utility_return")

# Create Green minus brown long-short equal weight portfolio
gmb_monthly_return <- full_join(clean_stock_monthly_return, oil_gas_stock_monthly_return) %>%
  mutate(oil_gas_return = -oil_gas_return) %>%
  pivot_longer(cols = c(clean_return, oil_gas_return)) %>%
  tq_portfolio(assets_col = name, returns_col = value, weights = c(23 / 75, 52 / 75), col_rename = "gmb_return")

# Read volumes for portfolios
clean_stock_vol <- read_csv("data/equity/clean_selected.csv") %>%
  cleanse_stock_data() %>%
  get_volume() %>%
  transmute(date = date,
            total_volume = rowSums(select(., -date)),
            yr_mon = format(date, "%Y-%m")) %>%
  group_by(yr_mon) %>%
  transmute(date = max(date),
            clean_volume = sum(total_volume)) %>%
  ungroup() %>%
  select(-yr_mon) %>%
  unique()

oil_gas_stock_vol <- read_csv("data/equity/oil_gas_coal_selected.csv") %>%
  cleanse_stock_data() %>%
  get_volume() %>%
  transmute(date = date,
            total_volume = rowSums(select(., -date)),
            yr_mon = format(date, "%Y-%m")) %>%
  group_by(yr_mon) %>%
  transmute(date = max(date),
            oil_gas_volume = sum(total_volume)) %>%
  ungroup() %>%
  select(-yr_mon) %>%
  unique()

utility_stock_vol <- read_csv("data/equity/utility_selected.csv") %>%
  cleanse_stock_data() %>%
  get_volume() %>%
  transmute(date = date,
            total_volume = rowSums(select(., -date)),
            yr_mon = format(date, "%Y-%m")) %>%
  group_by(yr_mon) %>%
  transmute(date = max(date),
            utility_volume = sum(total_volume)) %>%
  ungroup() %>%
  select(-yr_mon) %>%
  unique()

# Initialize output dataframe
output_df <- spx %>%
  merge(clean_stock_monthly_return) %>%
  merge(oil_gas_stock_monthly_return) %>%
  merge(utility_stock_monthly_return) %>%
  merge(gmb_monthly_return) %>%
  merge(clean_stock_vol) %>%
  merge(oil_gas_stock_vol) %>%
  merge(utility_stock_vol) %>%
  mutate(date = floor_date(date, unit = "month"))

# Load monthly FF factors
ff_monthly <- read_csv("data/equity/F-F_Research_Data_Factors_monthly.csv") %>%
  rename(Mkt_RF = `Mkt-RF`) %>%
  mutate(date = as.Date(paste0(as.character(date), "01"), format = "%Y%m%d"))

# Load WTI oil price
oil_df <- read_csv("data/Cushing_OK_WTI_Spot_Price_FOB.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  rename(wti_spot_price = `dollars per barrel`) %>%
  tq_transmute(wti_spot_price, mutate_fun = to.period, period = "month", col_rename = "wti_spot_price") %>%
  tq_mutate(wti_spot_price, mutate_fun = periodReturn, col_rename = "wti_return") %>%
  # Floor the date monthly to merge with other dataframe
  mutate(date = floor_date(date, unit = "month"))

# Load twitter data



output_df <- output_df %>%
  merge(ff_monthly) %>%
  merge(oil_df) %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))



# Merge output dataframe with FF factors and oil price


write_csv(output_df, "data/combined_monthly_data.csv")

## raw return std dev is significantly related to date!
test_model <- lm(daily_raw_return_sd ~ date, bmg_monthly_summary)
tidy(test_model)