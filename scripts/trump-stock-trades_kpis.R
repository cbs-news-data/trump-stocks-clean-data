library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(jsonlite)

trades_enriched <- read.csv("data/trades_enriched.csv") %>% 
  mutate(date = as.Date(date)) %>%
  mutate(amount_range = amount) %>% 
  separate(amount, into = c("amount_low", "amount_high"), sep = "-") %>% 
  mutate(amount_low = as.numeric(str_remove_all(amount_low, "[$,]")),
         amount_high = as.numeric(str_remove_all(amount_high, "[$,]"))) %>% 
  select(pdf_row, date, type, amount_range, amount_low, amount_high, amount_mid, raw_description, canonical_company, ticker, asset_type, gics_sector)


total_trades <- nrow(trades_enriched)
total_trades_pretty <- prettyNum(total_trades, big.mark = ",")

purchases_sales <- trades_enriched %>% 
  group_by(type) %>% 
  summarise(count = n())

purchases <- purchases_sales %>% filter(type == "purchase") %>% pull(count)
sales    <- purchases_sales %>% filter(type == "sale")     %>% pull(count)

purchases_pretty <- prettyNum(purchases, big.mark = ",")
sales_pretty <- prettyNum(sales, big.mark = ",")

buys_range <- trades_enriched %>% 
  filter(type == "purchase") %>% 
  summarise(sum_low = sum(amount_low),
            sum_high = sum(amount_high))

buys_low <- buys_range %>% pull(sum_low)
buys_high <- buys_range %>% pull(sum_high)

buys_low_pretty <- scales::dollar(buys_low, scale = 1e-6, suffix = "M", accuracy = 1)
buys_high_pretty <- scales::dollar(buys_high, scale = 1e-6, suffix = "M", accuracy = 1)
buys_range_pretty <- paste0(buys_low_pretty, "-", buys_high_pretty)

sells_range <- trades_enriched %>% 
  filter(type == "sale") %>% 
  summarise(sum_low = sum(amount_low),
            sum_high = sum(amount_high))

sells_low <- sells_range %>% pull(sum_low)
sells_high <- sells_range %>% pull(sum_high)

sells_low_pretty <- scales::dollar(sells_low, scale = 1e-6, suffix = "M", accuracy = 1)
sells_high_pretty <- scales::dollar(sells_high, scale = 1e-6, suffix = "M", accuracy = 1)
sells_range_pretty <- paste0(sells_low_pretty, "-", sells_high_pretty)

by_company <- trades_enriched %>%
  group_by(canonical_company) %>% 
  summarize(count = n())

number_of_companies <- nrow(by_company)
number_of_companies_pretty <- prettyNum(number_of_companies, big.mark = ",")


kpis <- list(
  total_trades         = total_trades_pretty,
  purchases            = purchases_pretty,
  sales                = sales_pretty,
  buys_range           = buys_range_pretty,
  sells_range          = sells_range_pretty,
  number_of_companies  = number_of_companies_pretty
)

write_json(kpis, "output/kpis.json", auto_unbox = TRUE)
