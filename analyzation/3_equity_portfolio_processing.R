## We will cleanse the equity daily closing data

require("tidyverse", quietly = TRUE)

clean_stock <- read.csv("data/equity/clean_selected.csv")

clean_stock <- clean_stock %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = !Ticker,
               names_to = "value")
