## We will cleanse the equity daily closing data
## Then we will calculate raw daily & monthly return and
## risk-adjusted return using fama-french three factor model

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

# Combine fama-french three-factor and portfolio df with daily return
# and calculate daily excess return + add year-month grouping helper column
combine_ff_model <- function(portfolio_df) {
  ff_model_data <- read_csv("data/equity/F-F_Research_Data_Factors_daily.csv") %>%
    mutate(date = as.Date(as.character(date), "%Y%m%d"))
  
  result_df <- portfolio_df %>%
    left_join(ff_model_data) %>%
    rename(MKT_RF = `Mkt-RF`) %>%
    mutate(daily_raw_return = daily_raw_return * 100,
           R_excess = daily_raw_return - RF)
  return(result_df)
}

# Model fitting for multiple groups with ff model
# Solution on: https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr
fit_ff_model <- function(portfolio_df) {
  result_df <- portfolio_df %>%
    group_by(yr_mon, category) %>%
    group_modify(
      # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
      ~ tidy(lm(R_excess ~ MKT_RF + SMB + HML, data = .))
    )
}

# Fit CAPM model because of unsatisfactory ff-model
fit_capm_model <- function(portfolio_df) {
  result_df <- portfolio_df %>%
    group_by(yr_mon, category) %>%
    group_modify(
      # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
      ~ tidy(lm(R_excess ~ MKT_RF, data = .))
    )
}

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

spx <- read_csv("data/spx.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  select(-volume) %>%
  tq_transmute(price, mutate_fun = periodReturn, period = "monthly", col_rename = "mkt_return") %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))

# Create equal weight portfolio and calculate daily raw return
# Deleting individual stock ticker data
clean_stock_monthly_return <- clean_stock %>%
  group_by(ticker) %>%
  tq_transmute(value, mutate_fun = periodReturn, period = "monthly") %>%
  tq_portfolio(assets_col = ticker, returns_col = monthly.returns, col_rename = "clean_return") %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))
oil_gas_stock_monthly_return <- oil_gas_stock %>%
  group_by(ticker) %>%
  tq_transmute(value, mutate_fun = periodReturn, period = "monthly") %>%
  tq_portfolio(assets_col = ticker, returns_col = monthly.returns, col_rename = "oil_gas_return") %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))
utility_stock_monthly_return <- utility_stock %>%
  group_by(ticker) %>%
  tq_transmute(value, mutate_fun = periodReturn, period = "monthly") %>%
  tq_portfolio(assets_col = ticker, returns_col = monthly.returns, col_rename = "utility_return") %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))

# Green minus brown long-short
gmb_monthly_return <- full_join(clean_stock_monthly_return, oil_gas_stock_monthly_return) %>%
  mutate(oil_gas_return = -oil_gas_return) %>%
  pivot_longer(cols = c(clean_return, oil_gas_return)) %>%
  tq_portfolio(assets_col = name, returns_col = value, weights = c(23 / 75, 52 / 75), col_rename = "gmb_return")

# Volume
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
  unique() %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))

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
  unique() %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))

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
  unique() %>%
  filter(date > as.Date("2012-03-31") & date < as.Date("2018-05-01"))

output_df <- merge(clean_stock_monthly_return, oil_gas_stock_monthly_return) %>%
  merge(utility_stock_monthly_return) %>%
  merge(gmb_monthly_return) %>%
  merge(clean_stock_vol) %>%
  merge(oil_gas_stock_vol) %>%
  merge(utility_stock_vol) %>%
  merge(spx)

write_csv(output_df, "data/stock_monthly_data.csv")

## raw return std dev is significantly related to date!
test_model <- lm(daily_raw_return_sd ~ date, bmg_monthly_summary)
tidy(test_model)

# # Combine fama-french factors and calculate risk-free excess return
# clean_stock_equal_weight <- combine_ff_model(clean_stock_equal_weight)
# oil_gas_stock_equal_weight <- combine_ff_model(oil_gas_stock_equal_weight)
# utility_stock_equal_weight <- combine_ff_model(utility_stock_equal_weight)
# 
# 
# # Fit fama-french model to the portfolios
# clean_stock_equal_weight_ff <- fit_ff_model(clean_stock_equal_weight)
# oil_gas_stock_equal_weight_ff <- fit_ff_model(oil_gas_stock_equal_weight)
# utility_stock_equal_weight_ff <- fit_ff_model(utility_stock_equal_weight)
# 
# # Fit capm model
# clean_stock_equal_weight_capm <- fit_capm_model(clean_stock_equal_weight)
# oil_gas_stock_equal_weight_capm <- fit_capm_model(oil_gas_stock_equal_weight)
# utility_stock_equal_weight_capm <- fit_capm_model(utility_stock_equal_weight)
# 
# # Save as csv
# rbind(clean_stock_equal_weight,
#       oil_gas_stock_equal_weight,
#       utility_stock_equal_weight) %>%
#   write_csv("data/stock_equal_weight_raw.csv")
# 
# rbind(clean_stock_equal_weight_ff,
#       oil_gas_stock_equal_weight_ff,
#       utility_stock_equal_weight_ff) %>%
#   write_csv("data/stock_equal_weight_ff.csv")
# 
# rbind(clean_stock_equal_weight_capm,
#       oil_gas_stock_equal_weight_capm,
#       utility_stock_equal_weight_capm) %>%
#   write_csv("data/stock_equal_weight_capm.csv")


###############################################################################





# Combine ff model data from percentage, left join to include only market open days
# price <- left_join(price, ff_model_data) %>%
#   rename(MKT_RF = `Mkt-RF`) %>%
#   mutate(monthly_raw_return = monthly_raw_return * 100,
#          daily_raw_return = daily_raw_return * 100,
#          R_excess = daily_raw_return - RF) %>%
#   
#   # Delete the March 30 entry after calculating return for April 02
#   filter(date != as.Date("2012-03-30"))
# 
# # test <- lm_table(price %>% filter(!is.na(monthly_raw_return)), R_excess ~ MKT_RF + SMB + HML, .groups = c("ticker", "yr_mon"))
# 
# fitted_model <- fitted_model %>%
#   left_join(y = price[!duplicated(price[ ,c("yr_mon", "ticker", "monthly_raw_return")]), ] %>%
#               select(yr_mon, ticker, monthly_raw_return, category))

