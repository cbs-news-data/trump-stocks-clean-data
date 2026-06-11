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

top_companies_dollar_range <- trades_enriched %>% 
  group_by(canonical_company) %>% 
  summarise(sum_low = sum(amount_low),
            sum_high = sum(amount_high)) %>% 
  arrange(desc(sum_high)) %>% 
  head(20)

write.csv(top_companies_dollar_range, "output/top_companies_dollar_range.csv", row.names = FALSE)

top_companies_number_trades <- trades_enriched %>% 
  group_by(canonical_company) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(15)

write.csv(top_companies_number_trades, "output/top_companies_number_trades.csv", row.names = FALSE)


weekly_activity <- trades_enriched %>%
  mutate(week = floor_date(as.Date(date), "week", week_start = 1)) %>%
  group_by(week, type) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(week) %>% 
  pivot_wider(names_from = type, values_from = count)

write.csv(weekly_activity, "output/weekly_activity.csv", row.names = FALSE)


dollars_by_sector_sales <- trades_enriched %>%
  filter(type=="sale") %>% 
  group_by(gics_sector) %>% 
  summarise(sum_low = sum(amount_low),
            sum_high = sum(amount_high)) %>% 
  arrange(desc(sum_high))

write.csv(dollars_by_sector_sales, "output/dollars_by_sector_sales.csv", row.names = FALSE)

dollars_by_sector_buys <- trades_enriched %>%
  filter(type=="purchase") %>% 
  group_by(gics_sector) %>% 
  summarise(sum_low = sum(amount_low),
            sum_high = sum(amount_high)) %>% 
  arrange(desc(sum_high))

write.csv(dollars_by_sector_buys, "output/dollars_by_sector_buys.csv", row.names = FALSE)


number_buys_sells_by_sector <- trades_enriched %>%
  group_by(gics_sector, type) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = type, values_from = count) %>% 
  arrange(desc(purchase)) %>% 
  rename(Buys = purchase,
         Sells = sale)

write.csv(number_buys_sells_by_sector, "output/number_buys_sells_by_sector.csv", row.names = FALSE)


stock_daily_close <- read.csv("data/daily_close_all.csv") %>% mutate(date = as.Date(date, format = "%Y-%m-%d"))

NVDA_daily_close <- stock_daily_close %>% 
  select(date, NVDA) %>% 
  filter(date >= "2025-12-01",
         date <= "2026-05-01")

PLTR_daily_close <- stock_daily_close %>% 
  select(date, PLTR) %>% 
  filter(date >= "2025-12-01",
         date <= "2026-05-01")

LLY_daily_close <- stock_daily_close %>% 
  select(date, LLY) %>% 
  filter(date >= "2025-12-01",
         date <= "2026-05-01")

write.csv(NVDA_daily_close, "output/NVDA_daily_close.csv", row.names = FALSE)
write.csv(PLTR_daily_close, "output/PLTR_daily_close.csv", row.names = FALSE)
write.csv(LLY_daily_close, "output/LLY_daily_close.csv", row.names = FALSE)
