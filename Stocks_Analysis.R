library(quantmod) ## is a third party tool to get a variety data of financial programs 
## Also the quantmod  helps in introducing the SMA simple moving average Time series functions 
library(BatchGetSymbols)
library(plotly)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(TSstudio)
library(tidyverse)
library(TSstudio)
library(tidymodels)
library(tidyquant) ## Needed for tq_transmute
library(quantmod) 
library(modeltime)
library(readr)
library(readxl)
library(lubridate)
library(DataExplorer)
library(zoo)
library(janitor)
library(timetk)
library(forecast)
library(patchwork)
library(graphics)
library(plotly)
library(devtools)
#devtools::install_github("thomasp85/patchwork", force = TRUE)
library(rlang)
library(earth)
library(modeltime.ensemble)
library(dygraphs)
library(xts)
library(gganimate)
library(zgtools) ## To install the dark mode of the plotly 
library(DataEditR)
library(ggthemes) ## to get the wall street theme
library(AnomalyDetection) ## Also the anomly detection library is installed via twitter/AnomalyDetection
library(purrr)
library(dplyr)
library(cowplot)
library(PerformanceAnalytics)
library(FinancialInstrument)
library(blotter) ## The blotter package is installed via github not the R cran 
library(quantmod)

### Market making to match the buyers and sellers, seeking an effective market 
## Market Order immediately 
## Limit order based on a limit 
## Large orders 
## Day orders only valid to execute in the same day, unless that did not occur,cancel the order 
## Good To canceled --> either to be executed immediately or left un executed until it is canceled by the initiator 
## Fill or Kill --> To be immediately executed or will be canceled otherwise 

out <- getSymbols("AAPL", env = NULL)
barChart(out, name = "AAPL")
barChart(out, name = "AAPL", subset = "last 28 days")

MSFT <- getSymbols("MSFT", auto.assign = FALSE)
chartSeries(MSFT, name = "Microsoft Stock Performance")
addMACD()
addBBands()
chartSeries(MSFT, name = "MSFT", subset = "last 90 days")
addMACD()
addBBands()

library(quantmod)

# Import QQQ data from Yahoo! Finance
getSymbols(Symbols = "QQQ", auto.assign = TRUE)


Stocks <-tq_get("AAPL") %>%
  select(symbol, date,  open, high, low, close,volume) %>% 
  arrange(date)  %>% 
  mutate(Year = year(date)) 



##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##


## Importing Amazon Stock Price 

AMZN <- tq_get("AMZN", from = "2015-01-01")


AMZN %>% 
  ggplot(aes(x = date, y = adjusted)) + 
  geom_line(aes(color = "Daily")) + 
  scale_y_continuous(
    labels = scales::label_dollar(), 
    position = "right"
  ) + 
  labs(title = "Amazon Stock Price", 
       subtitle = "Daily Adjusted Simple Moving Avergae",
       x = "") + 
  geom_ma(ma_fun = SMA, n= 30, size = 1.2, 
          aes(color = "30 days SMA")) + ## The SMA stands for the simple moving average 
  geom_ma(ma_fun = SMA, n =  90, size = 1.2, 
          aes(color = "90 days SMA")) + 
  scale_color_manual(name = "", 
                     values = c("Daily" = "black", 
                                "30 days SMA" = "blue", 
                                "90 days SMA" = "green")) +
  theme_wsj()



plotly_elegant <- function(plot, ..., margin = 3, background = "#1f1f1e", color = "#ffffff") {
  plot %>%
    plotly::layout(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      font = list(color = color,family = "Times New Roman"),
      plot_bgcolor = background,
      paper_bgcolor = background,
      margin = margin
    ) %>%
    plotly::layout(...)
}


## This part for the buttons 

rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           bgcolor = "#FF6633",
           font = list(size = 8),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=1,
                  label='1 YR',
                  step='year',
                  stepmode='backward'),
             list(count =6, 
                  label = '6 MO', 
                  step = 'month', 
                  stepmode = 'backward'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward')
           ))


rs2 <-list(visible = TRUE, x = 0.5, y = -0.055,
  xanchor = 'center', yref = 'paper',
   bgcolor = "#0066CC",
  font = list(size = 8),
  buttons = list(
  list(count=1,
       label='RESET',
       step='all'),
  list(count=1,
       label='1 YR',
       step='year',
       stepmode='backward'),
  list(count =6, 
       label = '6 MO', 
       step = 'month', 
       stepmode = 'backward'),
  list(count=3,
       label='3 MO',
       step='month',
       stepmode='backward'),
  list(count=1,
       label='1 MO',
       step='month',
       stepmode='backward')
))



 Stocks %>% 
  plot_ly(x=~date, y=~volume, type='bar', name = "AAPL Volume")  %>%
   plotly_elegant(
     xaxis = list(title = "Date"),
     yaxis = list(title = "APPL Volume")
   ) %>% 
  layout(title = paste("Apple: 2011-01-03 -",Sys.Date()),
         xaxis = list(rangeselector = rs2),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent')) 


Stocks %>% 
  mutate(Thirty_MA = TTR::SMA(close,n =30), 
         Ninety_MA = TTR::SMA(close, n=90)) %>% 
  plot_ly(x = ~ date, type="candlestick",
          open = ~open, close = ~close,
          high = ~high, low = ~low) %>% 
  add_lines(x = ~date, y = ~Thirty_MA, name = "30 Days Mv Avg",
            line = list(color = '#3333FF', width = 0.5),
            hoverinfo = "none", inherit = F)  %>%
  add_lines(x = ~date, y = ~Ninety_MA, name = "90 Days Mv Avg",
            line = list(color = '#CCFF00', width = 0.5),
            hoverinfo = "none", inherit = F)  %>%
  plotly_elegant(
    xaxis = list(title = "Date"),
    yaxis = list(title = "APPL Price")
  ) %>% 
  layout(title = paste("Apple: 2011-01-03 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))






Stocks %>% 
  plot_ly( 
       x = ~ date, 
       y = ~ close, 
       #frame = ~ Year,
       type = "scatter",
       mode = "line",
       name = "Apple Stock prices") %>% 
  layout(title = "Total Apple Stock Prices",
         yaxis = list(title = "Prices in Dollars")) %>% 
  plotly_elegant(
    xaxis = list(title = "APPLE DATE"),
    yaxis = list(title = "APPL Close Price")  ## To create the dark mode
  ) %>%
  rangeslider() ## To add the slider 


## This graphs meant for checing the Anomalies

APPL_ANOMALIES <- AnomalyDetectionVec(x = Stocks$close , period = 365, direction = 'both', plot = T)

APPL_ANOMALIES$anoms

Stocks_xts <- xts(x = Stocks$close, order.by = Stocks$date)
dygraph(Stocks_xts,main = "Total Apple Stock close prices") %>% 
  dyOptions(labelsUTC = TRUE, fillGraph=FALSE, fillAlpha=0.1, drawGrid = FALSE, colors="#0066CC") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)  %>% 
  dyAxis("y", "Apple Close price in $") %>% 
  dyAnnotation("2021-07-09", text = "APPL")








##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##

## Analyzing CIB stock price 


COMI_MONTHLY <- read.csv("data/COMI_Historical_Data.csv")  %>%
  mutate(Date = paste(substr(Date,1,4),substr(Date,5,6),"01",sep = "-")) %>% 
  rename(Volume = Vol., Change = Change..) %>% 
  mutate(Volume = as.character(Volume),
         Volume = as.numeric(case_when(str_detect(Volume,"M") ~ gsub("M","",Volume),
                                       str_detect(Volume,"B") ~gsub("B","",Volume), 
                                       TRUE ~ Volume)), 
         Volume = Volume * 1000000,
         Price = as.numeric(gsub(",", "",Price)),
         Open= as.numeric(gsub(",", "",Open)), 
         High = as.numeric(gsub(",", "",High)) , 
         Low = as.numeric(gsub(",", "",Low)), 
         Change = as.numeric(gsub("%", "", Change)), 
         Year = year(Date))



COMI_MONTHLY %>% 
  plot_ly(x = ~ Date, type="candlestick",
          open = ~Open, close = ~Price,
          high = ~High, low = ~Low) %>% 
  plotly_elegant(
    xaxis = list(title = "Date"),
    yaxis = list(title = "CIB Price")
  ) %>% 
  layout(title = "CIB Price Chandle Stick from 2020 : 2021")


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


## ---------------------------------------------------------------------------------------------------------## 
##----------------------------------------------------------------------------------------------------------## 


## Initiallizing the stocks strategies 


options("getSymbols.warning4.0"=FALSE)
from ="2008-01-01"
to ="2012-12-31"
symbols = c("AAPL", "IBM")
currency("USD")
getSymbols(symbols, from=from, to=to, 
           adjust=TRUE)
stock(symbols, currency="USD", multiplier=1)
initEq=10^6



rm("account.buyHold",pos=.blotter)
rm("portfolio.buyHold",pos=.blotter)

initPortf("buyHold", symbol=symbols)
initAcct("buyHold", portfolios = "buyHold",
         initEq = initEq)


Apple.Buy.Date <- first(time(AAPL))
Apple.Buy.Price <- as.numeric(Cl(AAPL[Apple.Buy.Date,]))
Apple.Sell.Date <- last(time(AAPL))
Apple.Sell.Price <- as.numeric(Cl(AAPL[Apple.Sell.Date,]))
Apple.Qty <- trunc(initEq/(2*Apple.Buy.Price))




IBM.Buy.Date <- first(time(IBM))
IBM.Buy.Price <- as.numeric(Cl(IBM[IBM.Buy.Date,]))
IBM.Sell.Date <- last(time(IBM))
IBM.Sell.Price <- as.numeric(Cl(IBM[IBM.Sell.Date,]))
IBM.Qty <- trunc(initEq/(2*IBM.Buy.Price))


addTxn(Portfolio = "buyHold", 
       Symbol = "AAPL", 
       TxnDate = Apple.Buy.Date, 
       TxnQty = Apple.Qty,
       TxnPrice = Apple.Buy.Price,
       TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Buy.Date, 
       TxnQty = IBM.Qty,
       TxnPrice = IBM.Buy.Price,
       TxnFees = 0)


addTxn(Portfolio = "buyHold", 
       Symbol = "AAPL", 
       TxnDate = Apple.Sell.Date, 
       TxnQty = -Apple.Qty,
       TxnPrice = Apple.Sell.Price,
       TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Sell.Date, 
       TxnQty = -IBM.Qty,
       TxnPrice = IBM.Sell.Price,
       TxnFees = 0)


updatePortf(Portfolio = "buyHold")
updateAcct(name = "buyHold")
updateEndEq(Account = "buyHold")

chart.Posn("buyHold", Symbol = "AAPL")

chart.Posn("buyHold", Symbol = "IBM")


out <- perTradeStats("buyHold", "IBM")
t(out)



##---------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------## 


## Buy and filter Rule 

from ="2009-01-01"
to ="2012-12-31"
symbols = c("MSFT")
currency("USD")
getSymbols(symbols, from=from, to=to, 
           adjust=TRUE)
stock(symbols, currency="USD", multiplier=1)
initEq=10^6





rm("account.filter",pos=.blotter)
rm("portfolio.filter",pos=.blotter)

initPortf("filter", symbol=symbols)
initAcct("filter", portfolios = "filter",
         initEq = initEq)

price <- Cl(MSFT)         
r <- price/Lag(price) - 1    
delta<-0.03
signal <-c(NA)
for (i in 2: length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(MSFT))

trade <- Lag(signal)
trade <- na.fill(trade,0)


for (i in 1:length(price)){
  if (as.numeric(trade[i]) == 1){
    addTxn(Portfolio = "filter",
           Symbol = "MSFT", 
           TxnDate = time(price[i]), 
           TxnQty = 1000,
           TxnPrice = price[i],
           TxnFees = 0)    
  }
  if (as.numeric(trade[i]) == -1){
    addTxn(Portfolio = "filter",
           Symbol = "MSFT", 
           TxnDate = time(price[i]), 
           TxnQty = -1000,
           TxnPrice = price[i],
           TxnFees = 0)    
  }
}


updatePortf(Portfolio = "filter")
updateAcct(name = "filter")
updateEndEq(Account = "filter")
chart.Posn("filter", Symbol = "MSFT")
