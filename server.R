
library(coinmarketcapr)
library(quantmod)
library(qmao)

stocks <- read.csv("stocks.csv")
coinmarketcapr::setup("4c009bf9-8da6-4870-8434-fcbafb014efb")  



server <- function(input, output, session){
  
  btc <- get_crypto_quotes(symbol="BTC")
  eth <- get_crypto_quotes(symbol="ETH")
  xmr <- get_crypto_quotes(symbol="XMR")
  iota <- get_crypto_quotes(symbol="MIOTA")
  
  crypto <- btc$price * .026838041 + eth$price * 3.4 + xmr$price * 2 + iota$price *305
  crypto_percent <- data.frame(SevenDay = btc$percent_change_7d + eth$percent_change_7d + xmr$percent_change_7d + iota$percent_change_7d  ,
                               ThirtyDay = btc$percent_change_30d + eth$percent_change_30d + xmr$percent_change_30d + iota$percent_change_30d ,
                               NinetyDay = btc$percent_change_90d + eth$percent_change_90d + xmr$percent_change_90d + iota$percent_change_90d)

  

  
  
  tickers <- stocks$Ticker
  getSymbols(tickers, from ="2020/05/05")
  prices <- PF(tickers, silent=TRUE)
  
  DF <- prices %>% as.data.frame() %>% mutate(Date = as.Date(row.names(DF)))
  
  DF <- DF %>% mutate(AMD = AMD * 75,
                DKNG = DKNG * 30,
                DAL = DAL * 23,
                PENN = PENN * 8,
                CRSR = CRSR * 16,
                FCEL = FCEL * 20,
                ICLN = ICLN * 33,
                THCX = THCX * 40,
                ARKK = ARKK * 6,
                SMH = SMH * 2,
                EWY = EWY * 5,
                MOAT = MOAT * 3)
  
  DF <- DF %>% rowwise() %>% mutate(NET = sum(c_across(AMD:MOAT)))
  
  DF <- DF %>% mutate(PL = NET- sum(stocks$Price))
  
  }


