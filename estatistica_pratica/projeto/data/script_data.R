
# ======================================================================================= #
# Script Name :                                                                                            
# Purpose     :                                                                      
# Args        : 
# Date        : Fri Dec 31 17:12:32 2021   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mosaic.pt                                           
# ======================================================================================= #

# Import prices split adjusted
# calculate annual variance
# import fundamentals
# select variables
# merge with securities

library(readr)
library(tidyverse)
library(lubridate)

# import files
fundamentals <- read_csv("estatistica_pratica/projeto/data/fundamentals.csv")
prices_split_adjusted <- read_csv("estatistica_pratica/projeto/data/prices-split-adjusted.csv")
securities <- read_csv("estatistica_pratica/projeto/data/securities.csv")

sd_stocks <- prices_split_adjusted %>% 
  mutate(year = year(date)) %>% 
  filter(year == 2016) %>% 
  group_by(symbol) %>% 
  summarise(
    sd_closingPrice = sd(close, na.rm = TRUE),
    average_closingPrice = mean(close, na.rm = TRUE)
  )

# Quick Ratio,Current Ratio, Pre-Tax ROE, Net Income/Total Assets, Short-Term Debt / Current Portion of Long-Term Debt - Long-Term Debt - Cash and Cash Equivalents/Earnings Before Interest and Tax
fundamental_info <- fundamentals %>% 
  mutate(year = year(`Period Ending`)) %>% 
  filter(year == 2016) %>% 
  select( symbol =`Ticker Symbol`, year, quickRatio = `Quick Ratio`, 
          currentRatio = `Current Ratio` , roe = `Pre-Tax ROE`, net_income = `Net Income`, 
          totalAssets = `Total Assets`, shor_debt = `Short-Term Debt / Current Portion of Long-Term Debt`,
          longDebt = `Long-Term Debt`, cash = `Cash and Cash Equivalents` , 
          EBIT = `Earnings Before Interest and Tax`, outstandig_shares = `Estimated Shares Outstanding`, 
          eps = `Earnings Per Share`
          ) %>% 
  mutate(
    roa = net_income / totalAssets,
    netDebt_ebit = (shor_debt + longDebt - cash) / EBIT
  ) %>% 
  select( -shor_debt, -longDebt,  -cash, -net_income)

data <- fundamental_info %>% 
  left_join(sd_stocks, by = "symbol" ) %>% 
  left_join(select(securities, symbol = `Ticker symbol`, industry = `GICS Sector`), by = "symbol") %>% 
  mutate(
    market_cap = outstandig_shares * average_closingPrice,
    size_cap = 
      case_when(
        market_cap / 100000000 >= 200 ~ "mega cap",
        market_cap / 100000000 <= 0.3 ~ "micro cap",
        market_cap / 100000000 >= 10 ~ "large cap",
        market_cap / 100000000 >= 2 ~ "medium cap",
        TRUE ~ "small cap"
      ),
    pe_ratio = average_closingPrice / eps
    ) %>% 
  select(-market_cap, -outstandig_shares) %>% 
  filter(
    !is.na(sd_closingPrice)
    )

write_csv(data, "estatistica_pratica/projeto/data/securities_data_2016.csv")
















