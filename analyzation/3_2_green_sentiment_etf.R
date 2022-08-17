## Create green ETF index according to Briere and Ramelli(2021)
# http://dx.doi.org/10.2139/ssrn.3850923

# Load libraries
require(tidyverse, quietly = TRUE)
require(tidyquant, quietly = TRUE)
require(broom, quietly = TRUE)
require(rtweet, quietly = TRUE)
require(readxl, quietly = TRUE)

# All ETF data from Wharton WRDS
etf_df <- read_xlsx("data/equity/all_etf.xlsx") %>%
  rename(date = `Data Date - Security Monthly`,
         ticker = `Ticker Symbol`,
         volume = `Trading Volume - Monthly`,
         price = `Price - Close - Monthly`,
         return = `Monthly Total Return`,
         shares_out = `Shares Outstanding Monthly - Issue`) %>%
  mutate(date = floor_date(as.Date(date), "monthly"),
         shares_out = shares_out / 1000000) %>% # shares outstanding in millions (same as bloomberg data)
  group_by(ticker) %>%
  mutate(return = price / lag(price) - 1) %>%
  ungroup() %>%
  select(date, ticker, price, volume, return, shares_out) %>%
  mutate(flows = shares_out / lag(shares_out) - 1,
         flows = gsub("Inf", NA, flows))

# ETF NAV data from Bloomberg BQL
etf_nav_df <- read_xlsx("data/equity/all_etf_nav.xlsx") %>%
  pivot_longer(cols = !c(ticker, data_field),
               names_to ="date",
               values_to = "value") %>%
  mutate(date = floor_date(as.Date(date, "%m/%d/%Y"), "monthly"),
         ticker = sub(" US Equity", "", ticker)) %>%
  pivot_wider(names_from = c(data_field), values_from = value) %>%
  rename(nav = FUND_TOTAL_ASSETS) # Caution total asset != NAV

etf_df <- etf_df %>%
  merge(etf_nav_df) %>%
  mutate(nav = nav / shares_out)

# Green etf data
green_etf_df <- read_xlsx("data/equity/green_etf_flow.xlsx") %>%
  pivot_longer(cols = !c(ticker, data_field),
               names_to ="date",
               values_to = "value",
               values_ptypes = numeric()) %>%
  mutate(date = floor_date(as.Date(date, "%m/%d/%Y"), "monthly"),
         ticker = sub(" US Equity", "", ticker)) %>%
  pivot_wider(names_from = c(data_field), values_from = value) %>%
  rename(volume = PX_VOLUME,
         price = PX_LAST,
         fund_flow = FUND_FLOW,
         shares_out = EQY_SH_OUT,
         nav = FUND_TOTAL_ASSETS) %>%
  mutate(flows = shares_out / lag(shares_out) - 1,
         green_etf = 1,
         nav = nav / shares_out,
         return = price / lag(price) - 1) %>%
  select(-fund_flow)

# Bind ETF data and green ETF data
etf_df <- etf_df %>% 
  filter(!ticker %in% unique(green_etf_df$ticker)) %>%
  mutate(green_etf = 0) %>%
  rbind(green_etf_df)

rm(etf_nav_df, green_etf_df)

# Perform cross sectional regression
result_df <- data.frame()

# Finally, Calculate green ETF factor according to Briere and Ramelli(2021)
for (d in unique(etf_df$date)) {
  monthly_df <- etf_df %>% filter(date == d)
  monthly_model <- lm(flows ~ log(nav) + green_etf + return, data = monthly_df, na.action = na.omit)
  result_df <- result_df %>%
    rbind(cbind(d, t(monthly_model$coefficients)))
}
result_df <- result_df %>%
  rename(date = d) %>%
  mutate(date = as.Date(date, origin = "1970-01-01"),
         green_etf = c(scale(green_etf))) %>%
  arrange(date) %>%
  select(date, green_etf)

# Save to CSV
write_csv(result_df, "data/green_etf_factor.csv")

rm(list = ls())
gc()
