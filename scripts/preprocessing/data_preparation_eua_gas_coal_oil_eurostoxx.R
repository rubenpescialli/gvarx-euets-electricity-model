library(dplyr)
library(tidyr)
library(zoo)
library(openxlsx)
library(xml2)



#### For weekend days I consider the first previous available datum (except for Italy, for that I have downloaded the weekend days electricity prices as well) ####

initial_data = read.csv("data/raw/non_specific_time_series.csv") 
str(initial_data)

price_cols <- setdiff(
  names(initial_data),
  c("Date","GME.Italy.Baseload.E.Mwh")
)

initial_data <- initial_data %>%
  mutate(
    Date = as.Date(Date, format = "%d-%m-%y")
  ) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
  arrange(Date) %>%
  fill(all_of(price_cols), .direction = "down")

# I need to stop at 2025-03-19 and start at 2017-12-31. -> This is because the downloaded data for electricity
# price refers to the delivery date of the day ahead price. So if we want to match that we need to take as 
# price date one day earlier.
initial_data <- initial_data[
    initial_data$Date >= as.Date("2017-12-31") &
    initial_data$Date <= as.Date("2025-03-19"),
]

# write.xlsx(initial_data,
#            file      = "non_specific_time_series_adjusted_for_missing_weekend_days.xlsx",
#            rowNames  = FALSE)



#### Adjusting the brent series: exchange rate and adjusting for weekends and similar ####

# This is not needed anymore, it was in case you needed to consider spot prices for oil.

# data_brent = read.csv("DCOILBRENTEU.csv")
# str(data_brent)
# 
# data_brent <- data_brent %>% 
#   mutate(observation_date = as.Date(observation_date))
# 
# # Exchange rate
# doc <- read_xml("usd.xml")
# 
# ns <- xml_ns(doc)
# print(ns)
# 
# obs_nodes <- xml_find_all(doc, ".//d2:Obs", ns = ns)
# 
# data_usd_eur <- tibble(
#   date  = xml_attr(obs_nodes, "TIME_PERIOD"),
#   value = xml_attr(obs_nodes, "OBS_VALUE") %>% as.numeric()
# )
# 
# rm(doc, obs_nodes)
# 
# str(data_usd_eur)
# data_usd_eur$date = as.Date(data_usd_eur$date)
# 
# data_usd_eur <- data_usd_eur %>%
#   filter(date >= as.Date("2017-12-29") & date <= as.Date("2025-03-19"))
# 
# data_usd_eur <- data_usd_eur %>%
#   rename(observation_date = date)
# 
# data_brent <- left_join(data_brent, data_usd_eur, by = "observation_date")
# 
# data_brent <- data_brent %>%
#   mutate(DCOILBRENTEU = DCOILBRENTEU * (1/value))
# 
# # Weekend and similar days adjustments
# 
# data_brent <- data_brent %>% 
#   complete(observation_date = seq.Date(
#     from = as.Date("2017-12-31"),
#     to   = as.Date("2025-03-19"),
#     by   = "day"
#   )) %>%
#   arrange(observation_date) %>%
#   fill(DCOILBRENTEU, .direction = "down")
# 
# data_brent[1,2] = 66.73*(1/1.1993)
# data_brent[2,2] = 66.73*(1/1.1993)
# 
# write.xlsx(data_brent,
#            file      = "Brent_EU_in_EUR_per_barrel_adjusted_for_missing_weekend_days.xlsx",
#            rowNames  = FALSE)