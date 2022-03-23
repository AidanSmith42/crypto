library(tidyverse)
library(tidyquant)
library(fredr)
library(treasuryTR)
library(pwiser)
library(patchwork)
library(cowplot)
library(gt)
library(riingo)
library(gtsummary)
library(gtExtras)
library(english)
library(coinmarketcapr)
library(geckor)
library(ggridges)


#install.packages("devtools")
#devtools::install_github("brshallo/pwiser")
#remotes::install_github("jthomasmock/gtExtras")

bea_api_key <- Sys.getenv("BEA_API_KEY")
fred_api_key <- Sys.getenv("FRED_API_KEY")
quandl_api_key <- Sys.getenv("QUANDL_API_KEY")
coinmarketcap_api_key <-  "4c009bf9-8da6-4870-8434-fcbafb014efb"
coinmarketcapr::setup(api_key = coinmarketcap_api_key, sandbox = FALSE)





sol_from_coingecko <- 
  coin_history(
    coin_id = "solana",
    vs_currency = "usd",
    days = "1000"
  )
sol_from_coingecko_cleaned <- 
  sol_from_coingecko %>% 
  mutate(date = as.Date(timestamp)) %>% 
  select(coin = coin_id, date, price) %>% 
  distinct(date, .keep_all = T)



# knitr::opts_chunk$set(message=FALSE, 
#                       warning=FALSE,
#                       comment = NA, 
#                       echo = FALSE,
#                       out.width = '85%', 
#                       fig.align = 'left',
#                       fig.pos = 'h', 
#                       out.extra = '')



top_crypto_tibble <- 
  get_crypto_listings('USD', 
                      latest = T, 
                      # limit  = 50, 
                      sort  = "USD_market_cap")  %>%
  arrange(desc(USD_market_cap)) 

top_20_cryptos <- 
  top_crypto_tibble %>%  
  filter(!str_detect(slug, "usd"),
         slug != "wrapped-bitcoin") %>% 
  select(symbol, coin = slug,  USD_market_cap, USD_price, USD_volume_24h, date_added ) %>% 
  filter(!coin %in% c( "tether", "bitcoin-cash", "ethereum-classic")) %>% 
  mutate(
    `Volume to Mkt Cap` = USD_volume_24h/USD_market_cap,
    across(.cols = contains("market"),
           ~round(.x, 0))
  ) %>%
  arrange(desc(USD_market_cap)) %>% 
  rename_with(~str_remove(.x, "USD_") %>% str_replace_all("_", " ")) %>% 
  slice(1:20)

top_20_logos <- 
  top_20_cryptos %>% 
  pull(symbol) %>% 
  get_crypto_meta() %>% 
  select(logo, symbol)

top_20_descriptions<- 
  top_20_cryptos %>% 
  pull(symbol) %>% 
  get_crypto_meta() %>% 
  select(description, symbol)

top_20_crypto_tickers <- 
  top_20_cryptos %>%
  mutate(symbol = str_glue("{str_to_lower(symbol)}usd")) %>% 
  pull(symbol)

top_20_crypto_prices_for_sparkline <- 
  top_20_crypto_tickers   %>% 
  riingo_crypto_prices(start_date = "2021-01-01") %>% 
  group_by(ticker) %>% 
  select(ticker, date, close) %>% 
  filter(ticker != "solusd") %>% 
  mutate(date = ymd(date)) %>% 
  bind_rows(
    sol_from_coingecko_cleaned %>% 
      mutate(ticker = "solusd") %>% 
      select(ticker, date, close = price)
  ) %>% 
  arrange(date) %>% 
  summarise(price = list(close))

table <- top_20_cryptos %>% 
  left_join(top_20_logos) %>% 
  # select(-symbol) %>% 
  left_join(top_20_crypto_prices_for_sparkline %>% 
              mutate(symbol = str_remove(ticker, "usd") %>% toupper()) %>% 
              select(-ticker, `2021 Price` = price)
  ) %>% 
  select(-symbol) %>% 
  relocate(logo, everything(), `date added`) %>% 
  mutate(`date added` = str_glue("{month(`date added`, label = T)} {year(`date added`)}")) %>% 
  gt() %>% 
  fmt_currency(
    columns = `market cap`:`volume 24h`,
    currency = "USD",
    decimals = 2,
    suffixing = TRUE 
  ) %>% 
  fmt_percent(columns = `Volume to Mkt Cap`) %>% 
  gt_sparkline(`2021 Price`, same_limit = FALSE, line_color = "#40c5ff") %>%
  gt_theme_guardian() %>% 
  gt_img_rows(logo) %>%
  tab_header(title = str_glue("Coin Snapshots of {Sys.Date()}")) %>% 
  tab_source_note(
    md(
      paste0("**Data:** Coinmarketcap and Coingecko")
    )
  ) 
