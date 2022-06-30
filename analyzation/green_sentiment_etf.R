## test on etf

# Load libraries
require(tidyverse, quietly = TRUE)
require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(rtweet, quietly = TRUE)
require(readxl, quietly = TRUE)

etf_df <- read_xlsx("data/equity/all_etf.xlsx") %>%
  rename(date = `Data Date - Security Monthly`,
         ticker = `Ticker Symbol`,
         volume = `Trading Volume - Monthly`,
         price = `Price - Close - Monthly`,
         return = `Monthly Total Return`,
         shares_out = `Shares Outstanding Monthly - Issue`) %>%
  mutate(date = as.Date(date)) %>%
  select(date, ticker, price, volume, return, shares_outstanding)

green_etf_df <- read_csv("data/equity/fund_flow.csv") %>%
  pivot_longer(cols = !c(ticker, data_field),
               names_to ="date",
               values_to = "value",
               values_ptypes = numeric()) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         ticker = sub(" US Equity", "", ticker)) %>%
  pivot_wider(names_from = c(data_field), values_from = value) %>%
  rename(volume = PX_VOLUME,
         price = PX_LAST,
         fund_flow = FUND_FLOW,
         shares_out = EQY_SH_OUT) %>%
  select(-FUND_TOTAL_ASSETS)
