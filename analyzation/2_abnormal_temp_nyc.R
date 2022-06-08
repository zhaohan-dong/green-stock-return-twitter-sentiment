## Calculate the Abnormal Temperature in NYC
# Using method defined by Darwin Choi et al. (2020)

require("tidyverse", quietly = TRUE)
require("zoo", quietly = TRUE)

# Load original data from NOAA
# Select fields DATE, NAME, TAVG (monthly average temperature), TMAX, TMIN
nyc_wx <- read.csv("data/KNYC_monthly_summary.csv") %>%
  select(DATE, NAME, TAVG, TMAX, TMIN)

# Convert DATE field from string to date class
nyc_wx$DATE <- as.Date(as.yearmon(nyc_wx$DATE))

# Filter data to between 1997 (120 months before 2007) and 2018 (incl)
nyc_wx <- nyc_wx %>%
  filter(DATE >= "1997-01-01" & DATE < "2019-01-01")

