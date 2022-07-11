## Regression between tweets and NYT/AP

require(tidyverse, quietly = TRUE)
require(readxl, quietly = TRUE)
require(lubridate, quietly = TRUE)

nyt_climate_change <- read_excel("data/nyt_climate_change_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))
nyt_global_warming <- read_excel("data/nyt_global_warming_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))
ap_climate_change <- read_excel("data/ap_climate_change_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))
ap_global_warming <- read_excel("data/ap_global_Warming_2008_2018.xlsx") %>%
  rename_all(tolower) %>%
  rename(date = `published date`) %>%
  mutate(date = as.Date(date, format = "%B %e, %Y %A")) %>%
  mutate(date = floor_date(date, unit = "month"))

nyt_climate_change <- nyt_climate_change %>%
  merge(count(nyt_climate_change, date, name = "monthly_nyt_climate_change_count")) %>%
  select(date, monthly_nyt_climate_change_count) %>%
  distinct() %>%
  filter(!is.na(date))
nyt_global_warming <- nyt_global_warming %>%
  merge(count(nyt_global_warming, date, name = "monthly_nyt_global_warming_count")) %>%
  select(date, monthly_nyt_global_warming_count) %>%
  distinct() %>%
  filter(!is.na(date))
ap_climate_change <- ap_climate_change %>%
  merge(count(ap_climate_change, date, name = "monthly_ap_climate_change_count")) %>%
  select(date, monthly_ap_climate_change_count) %>%
  distinct()
ap_global_warming <- ap_global_warming %>%
  merge(count(ap_global_warming, date, name = "monthly_ap_global_warming_count")) %>%
  select(date, monthly_ap_global_warming_count) %>%
  distinct()

news_summary_df <- nyt_climate_change %>%
  merge(nyt_global_warming) %>%
  merge(ap_climate_change) %>%
  merge(ap_global_warming) %>%
  group_by(date) %>%
  mutate(news_total = sum(monthly_nyt_climate_change_count,
                          monthly_nyt_global_warming_count,
                          monthly_ap_climate_change_count,
                          monthly_ap_global_warming_count)) %>%
  ungroup()


