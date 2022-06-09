## We will cleanse the equity daily closing data

require("tidyverse", quietly = TRUE)

# Define function to cleanse data
cleanse_stock_data <- function(df) {
  result_df <- df %>%
    pivot_longer(cols = !c(ticker, data_field),
                 names_to ="date",
                 values_to = "value",
                 values_ptypes = numeric()) %>%
    mutate(date = as.Date(date, "%d/%m/%Y"),
           ticker = sub(" US Equity", "", ticker))
  return(result_df)
}

clean_stock <- read_csv("data/equity/clean_selected.csv") %>%
  cleanse_stock_data() %>%
  write_csv("data/equity/clean_cleansed.csv")
rm(clean_stock)

oil_gas_stock <- read_csv("data/equity/oil_gas_selected.csv") %>%
  cleanse_stock_data() %>%
  write_csv("data/equity/oil_gas_cleansed.csv")
rm(oil_gas_stock)

utility_stock <- read_csv("data/equity/utility_selected.csv") %>%
  cleanse_stock_data() %>%
  write_csv("data/equity/utility_cleansed.csv")
rm(utility_stock)
