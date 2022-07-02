## Regression between tweets and NYT/AP

require(tidyverse, quietly = TRUE)
require(readxl, quietly = TRUE)

nyt_climate_change <- read_excel("data/nyt_climate_change_2008_2018.xlsx")
nyt_global_warming <- read_excel("data/nyt_global_warming_2008_2018.xlsx")