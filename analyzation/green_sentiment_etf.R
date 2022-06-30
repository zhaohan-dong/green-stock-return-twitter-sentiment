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
         shares_out = `Shares Outstanding Monthly - Issue`,
         nav = `Net Asset Value - Monthly`) %>%
  mutate(date = floor_date(as.Date(date), "monthly")) %>%
  select(date, ticker, price, volume, return, shares_out, nav) %>%
  mutate(flows = shares_out / lag(shares_out) - 1,
         flows = gsub("Inf", NA, flows),
         green = 0)

green_etf_df <- read_csv("data/equity/fund_flow.csv") %>%
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
  mutate(flows = shares_out / lag(shares_out) - 1)

result_df <- data.frame()

for (d in unique(etf_df$date)) {
  monthly_df <- etf_df %>% filter(date == d)
  monthly_model <- lm(flows ~ green + return, data = monthly_df, na.action = na.omit)
  result_df <- result_df %>%
    rbind(cbind(d, t(monthly_model$coefficients)))
}
result_df <- result_df %>%
  rename(date = d) %>%
  mutate(date = as.Date(date, origin = "1970-01-01")) %>%
  arrange(date)


as_tibble(monthly_model$coefficients)
summary(etf_model)

