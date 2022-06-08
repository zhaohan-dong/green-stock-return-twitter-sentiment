## Calculate the Abnormal Temperature in NYC
# Using method defined by Darwin Choi et al. (2020)

require("tidyverse", quietly = TRUE)
require("zoo", quietly = TRUE)
require("lubridate", quietly = TRUE)

# Load original data from NOAA
# Select fields DATE, NAME, TAVG (monthly average temperature), TMAX, TMIN
nyc_wx <- read.csv("data/KNYC_monthly_summary.csv") %>%
  select(DATE, NAME, TAVG, TMAX, TMIN)

# Convert DATE field from string to date class with zoo package
nyc_wx$DATE <- as.Date(as.yearmon(nyc_wx$DATE))

# Filter data to between 1997 (120 months before 2007) and 2018 (incl)
nyc_wx <- nyc_wx %>%
  filter(DATE >= "1997-01-01" & DATE < "2019-01-01")

prev_year_average <- function(current_month) {
  # Handle months before 2007-01, return NA_Date_
  if (current_month < "2007-01-01") {
    return(NA_Date_)
  } else {
    query_wx <- nyc_wx %>%
      filter(DATE >= current_month - month(120) &
               DATE < current_month)
    return(avg(query_wx$TAVG))
  }
}

prev_year_average("2018-01")

nyc_wx$PREV_YRS_TAVG <- nyc_wx
