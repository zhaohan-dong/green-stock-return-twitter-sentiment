## Calculate the Abnormal Temperature in NYC
# Using method defined by Darwin Choi et al. (2020)
# Note: Not used in paper, tested the hypothesis of temperature influencing stock

require("tidyverse", quietly = TRUE)
require("zoo", quietly = TRUE)
require("lubridate", quietly = TRUE)

# Load original data from NOAA
# Select fields DATE, NAME, TAVG (monthly average temperature) (TMAX and TMIN also available)
nyc_wx <- read.csv("data/KNYC_monthly_summary.csv") %>%
  select(DATE, TAVG)

# Convert DATE field from string to date class with zoo package
nyc_wx$DATE <- as.Date(as.yearmon(nyc_wx$DATE))

# Filter data to between 1997 (120 months before 2007) and 2018 (incl)
nyc_wx <- nyc_wx %>%
  filter(DATE >= "1997-01-01" & DATE < "2019-01-01")

nyc_wx <- nyc_wx %>%
  # Calculate temperature average of previous 120 months
  mutate(prev_yrs_tavg = rollmeanr(TAVG, 120, na.pad = TRUE, align = "right"),
         # List lagged monthly average temperatures
         lag12 = lag(TAVG, n = 12L, order_by = DATE),
         lag24 = lag(TAVG, n = 24L, order_by = DATE),
         lag36 = lag(TAVG, n = 36L, order_by = DATE),
         lag48 = lag(TAVG, n = 48L, order_by = DATE),
         lag60 = lag(TAVG, n = 60L, order_by = DATE),
         lag72 = lag(TAVG, n = 72L, order_by = DATE),
         lag84 = lag(TAVG, n = 84L, order_by = DATE),
         lag96 = lag(TAVG, n = 96L, order_by = DATE),
         lag108 = lag(TAVG, n = 108L, order_by = DATE),
         lag120 = lag(TAVG, n = 120L, order_by = DATE)
         ) %>%
  # Calculate temperature average of the same month in past 10 years
  mutate(monthly_lagged_tavg_deviation = (lag12 + lag24 + lag36 + lag48 + lag60 +
                                lag72 + lag84 + lag96 + lag108 + lag120) / 12 - prev_yrs_tavg) %>%
  select(DATE, TAVG, prev_yrs_tavg, monthly_lagged_tavg_deviation) %>%
  # Calculate abnormal temperature
  mutate(ab_temp = TAVG - prev_yrs_tavg - monthly_lagged_tavg_deviation)

nyc_wx <- rename_with(nyc_wx, tolower) %>%
  filter(!is.na(ab_temp))

write.csv(nyc_wx, "data/KNYC_monthly_summary_processed.csv")

# Clean up memory
rm(list = ls())
gc()
