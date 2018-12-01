
library(readr)

#getwd() #"D:/NYCDSA/Project 4 - Capstone Project"
setwd("D:/NYCDSA/Project 4 - Capstone Project")

eur= read_csv("EURUSD1.csv")

#4448014 obs
#edit date time, set bull/bear bar column


library(anytime)
eur$Date <- anydate(eur$Date) # anytime library
#eur$Date <- as.Date(eur$Date) # {base}
class(eur$Date) #"Date"

class(eur$Timestamp)

library(lubridate) #part of tidyverse?

minute(eur$Timestamp[1:3]) # 0 1 2
?lubridate

library(dplyr)
eur %>% group_by(Date) %>% summarise(max(High))
eur %>% group_by(year(Date)) %>% summarise(max(High)) #can use lubridate year() to group
look = eur %>% group_by(Date, hour(Timestamp)) %>% summarise(max(High))
look = eur %>% group_by(Date) %>% summarise(max(High))


########################################################################################### plotting w/ plotly
#http://tabvizexplorer.com/interactive-candlestick-chart-using-r/
#https://plot.ly/r/candlestick-charts/

#install.packages("plotly")
library(plotly)
library(quantmod)

#install.packages("rlang")

look = eur %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                            Low = min(Low), Close = last(Close))

p <- look %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low) %>%
  layout(title="EURUSD Daily Chart")

p

#saved as eurdaily_plotly.png
########################################################################################


######################################################################################## plotting with dygraph
#https://rstudio.github.io/dygraphs/gallery-candlestick.html
library(xts)
install.packages("dygraphs")
library(dygraphs)

data(sample_matrix) #column as index

eurusd = eur %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))

eurusd2007 = eurusd[year(eurusd$Date) == 2007,]
eurusd2007a = eurusd2007[-1]
rownames(eurusd2007a) <- eurusd2007$Date

dygraph(eurusd2007a) %>%
  dyCandlestick()


########################################################################################


######################################################################################## DEBUG tidyquant geom_barchart
#Warning: Ignoring unknown parameters: colour_up, colour_down
#Warning: Ignoring unknown parameters: colour_up, colour_down
#Warning: Ignoring unknown parameters: colour_up, colour_down
#Warning messages:
#1: Computation failed in `stat_linerange_bc()`:
#  Evaluation error: argument "color_up" is missing, with no default. 
#2: Computation failed in `stat_segment_left_bc123()`:
#  Evaluation error: argument "color_down" is missing, with no default. 
#3: Computation failed in `stat_segment_right_bc()`:
#  Evaluation error: argument "color_up" is missing, with no default. 
#solution: FIRST, RUN geom_chart raw code. 
StatSegmentLeftBC<- ggplot2::ggproto("stat_segment_left_bc123", Stat,
                                     required_aes = c("x", "open", "high", "low", "close"),
                                     
                                     compute_group = function(data, scales, params,
                                                              fill_up, fill_down,
                                                              color_up = "darkgreen", color_down = "darkred") {
                                       
                                       data <-  data %>%
                                         dplyr::mutate(color = ifelse(open < close, color_up, color_down))
                                       
                                       tibble::tibble(x    = data$x,
                                                      xend = data$x - 0.5,
                                                      y    = data$open,
                                                      yend = data$open,
                                                      colour = data$color)
                                     }
)    
#
StatSegmentRightBC <- ggplot2::ggproto("StatSegmentRightBC", Stat,
                                       required_aes = c("x", "open", "high", "low", "close"),
                                       
                                       compute_group = function(data, scales, params,
                                                                fill_up, fill_down,
                                                                color_up = "darkgreen", color_down = "darkred") {
                                         
                                         data <-  data %>%
                                           dplyr::mutate(color = ifelse(open < close, color_up, color_down))
                                         
                                         tibble::tibble(x    = data$x,
                                                        xend = data$x + 0.5,
                                                        y    = data$close,
                                                        yend = data$close,
                                                        colour = data$color)
                                       }
)
#

StatLinerangeBC <- ggplot2::ggproto("StatLinerangeBC", Stat,
                                    required_aes = c("x", "open", "high", "low", "close"),
                                    
                                    compute_group = function(data, scales, params,
                                                             fill_up, fill_down,
                                                             color_up = "darkgreen", color_down = "darkred") {
                                      
                                      data <-  data %>%
                                        dplyr::mutate(color = ifelse(open < close, color_up, color_down))
                                      
                                      tibble::tibble(x = data$x,
                                                     ymin = data$low,
                                                     ymax = data$high,
                                                     colour = data$color)
                                    }
)
########################################################################################


######################################################################################## plotting with tidyquant
#https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ04-charting-with-tidyquant.html
#charting financial data using ggplot2
#install.packages("tidyquant")
# Loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR 
library(readr)
setwd("D:/NYCDSA/Project 4 - Capstone Project")
eur= read_csv("EURUSD1.csv")

library(anytime)
eur$Date <- anydate(eur$Date)
library(tidyquant)

eurusd = eur %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                            Low = min(Low), Close = last(Close))
#####Line Chart
eurusd %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_line() +
  labs(title = "EURUSD Line Chart", y = "Closing Price", x = "") #+ 
  theme_tq()

eurusd %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_barchart(aes(open = Open, high = High, low = Low, close = Close)) +
  labs(title = "Bar Chart", y = "Closing Price", x = "") + 
  theme_tq()
#library(dplyr)
#library(ggplot2)
#library(lubridate)

#####CandleStick Chart
#get one year 
eurusd2007 = eurusd[year(eurusd$Date) == 2007,]

end <- as_date("2008-12-31")
#eurusd2007[1:50,] 
eurusd2007 %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close),
                   fill_up  = "darkgreen", fill_down  = "darkred")+
  labs(title = "EURUSD Candlestick Chart", y = "Price", x = "") + 
  coord_x_date(xlim = c("2007-1-10", "2007-5-31"), ylim = c(1.28, 1.375)) + #c(end - weeks(6), end)
  theme_tq()

??geom_candlestick
#data <-  data %>%
#  dplyr::mutate(color = ifelse(open < close, color_up, color_down))

########################################################################################
#repeat for JPY, CHF, and Gold
jpy= read_csv("USDJPY1.csv")
chf= read_csv("USDCHF1.csv")
xau= read_csv("XAUUSD1.csv")

#format date column 
jpy$Date <- anydate(jpy$Date)
chf$Date <- anydate(chf$Date)
xau$Date <- anydate(xau$Date)

#turn minute data to daily
usdjpy = jpy %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
usdchf = chf %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
xauusd = xau %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))

#get one year worth of daily
usdjpy2007 = usdjpy[year(usdjpy$Date) == 2007,]
usdchf2007 = usdchf[year(usdchf$Date) == 2007,]
xauusd2007 = xauusd[year(xauusd$Date) == 2007,]

#need symbol for groupby 
usdjpy2007$Symbol = "USDJPY"
usdchf2007$Symbol = "USDCHF"
xauusd2007$Symbol = "XAUUSD"
eurusd2007$Symbol = "EURUSD"

#append all four instruments
total2007 <- rbind(usdjpy2007, usdchf2007, xauusd2007, eurusd2007) 
#write.csv(total2007, "total2007.csv")

total2007 %>% 
ggplot(aes(x = Date, y = Close, group = Symbol)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
  labs(title = "Four Currencies Candlestick Chart", 
       subtitle = "Experimenting with Multiple Instruments",
       y = "Price", x = "") + 
  #coord_x_date(xlim = c(start, end)) +
  facet_wrap(~Symbol, ncol = 2, scale = "free_y") + 
  theme_tq()

#next, visualizing trends. or start with tinyquant documentation