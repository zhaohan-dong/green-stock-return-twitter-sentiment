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

# Define query filter for the prev_year_average function
query_date_filter <- function(date, prior_month = 120) {
  filtered_df <- nyc_wx %>%
    filter(DATE >= date - months(prior_month) &
             DATE < current_month)
  return(filtered_df)
}


prev_year_avg <- function(data_col) {
  # Handle months before 2007-01, return NA_Date_
  ifelse(data_col < "2007-01-01",
         # return NA_Date_ if month is before 2007-01
         return(NA),
         
         # else
         return(avg(query_date_filter(data_col, 120)$data_col))
         )
}

nyc_wx$PREV_YRS_TAVG <- prev_year_avg(nyc_wx$DATE)

nyc_wx$DATE < "2007-01-01"
