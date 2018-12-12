library(readr) ; library(plotly); library(dplyr) ; library(anytime)
library(lubridate)

#getwd() #"D:/NYCDSA/Project 4 - Capstone Project"
setwd("D:/NYCDSA/Project 4 - Capstone Project v2")


#read reuters data, USD daily data
eur= read_csv("eurdaily_reuters.csv")
eur = eur[c(1,5)]
#write.csv(eur, "eurdaily_reuters.csv", row.names = F)

#reuters= read_csv("reuters_excerpt_post_NLP_full.csv")
reuters= read_csv("reuters_plotly.csv")
#reuters = reuters[-c(2,5,8,9,11:14)]
#write.csv(reuters, "reuters_plotly.csv", row.names = F)

#MIXED NEGATIVE  NEUTRAL POSITIVE 
#8      704     9436       52 
remove exerpt LDA
remove page number
remove sentiment numbers

table(reuters$post.topic)
#0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   26   27 
#1231 1163   53   35 1402 2048   46   24    8    8    1    6  849  595  456   11  528  425   12  650  241  408 
c = reuters[reuters$post.topic == 10,]


unique(reuters$term)
unique(reuters$post.term)
unique(reuters$sentiment) #[1] "NEUTRAL"  "NEGATIVE" "POSITIVE" "MIXED"
table(reuters$sentiment)
reuters[reuters$sentiment == "MIXED",]
a = reuters %>% group_by(Date) %>% count()
mean(a[[2]])
summary(a)
#Date                  n         
#Min.   :2010-05-17   Min.   : 1.000  
#1st Qu.:2012-09-26   1st Qu.: 2.000  
#Median :2014-11-09   Median : 4.000  
#Mean   :2014-10-10   Mean   : 4.376  
#3rd Qu.:2016-10-22   3rd Qu.: 6.000  
#Max.   :2018-11-30   Max.   :36.000  

b = reuters[reuters$Date == '2015-01-15',]
b1 = reuters[reuters$Date == '2013-07-13',]
b2 = reuters[reuters$Date == '2014-12-18',]


#d = reuters %>% group_by(hour(Timestamp)) %>% count()
#summary(d)



d = reuters %>% group_by(month(Date)) %>% count()
d2 = reuters %>% group_by(year(Date)) %>% count()

summary(d2)
############################################################# plotly
a <- list(text = "Stock Split",
          x = '2014-06-06',
          y = 1.6,
          xref = 'x',
          yref = 'paper',
          xanchor = 'left',
          showarrow = FALSE
)

l <- list(type = line,
          x0 = '2014-06-06',
          x1 = '2014-06-06',
          y0 = 0,
          y1 = 1,
          xref = 'x',
          yref = 'paper',
          line = list(color = 'black',
                      width = 0.5)
)


p <- eur %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low) %>%
  layout(title = "Basic Candlestick Chart") %>% 
  layout( shapes = l,
          annotations = a)

p
pp <- df %>%
  plot_ly(x=~Date, y=~AAPL.Volume, type='bar', name = "AAPL Volume",
          color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
  layout(yaxis = list(title = "Volume"))

p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("Apple: 2015-02-14 -",Sys.Date()),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


################################# Time series chart
eurreuters = read_csv("all_currencies_plotly.csv")
eurreuters = eurreuters[eurreuters$Date > "2010-05-15",]
eurreuters[eurreuters$Date == "2015-01-15",]['post.y'] = "Hedge funds, speculators face big losses on Swiss franc rally<br>Swiss central bank stuns market with policy U-turn<br>Euro floored as SNB decision puts spotlight on ECB, franc surges<br>Weak oil dictates mutual fund winners and losers in 2014<br>Lagarde said Swiss central bank did not warn IMF<br>Swiss shares post biggest one-day fall since 1989 on franc furore<br>Commodities traders' costs soar with Swiss franc's leap<br>Oil price plunge could leave helicopters sputtering<br>Oil settles down despite promising mid-day rally<br>Swiss franc soars after SNB drops cap on currency<br>Franc, European shares soar after Swiss currency cap lifted<br>Who wants to buy a hotel? Swiss Alps in shock over franc's rise<br>Capital flows to emerging markets to fall in 2015 for second straight year<br>Swiss banks hammered as earnings face hit from currency surge<br>Soaraway Swiss franc threatens downturn and deflation<br>ABB sees limited impact of strong Swiss franc against the euro<br>Pending deals seen unaffected by Swiss franc leap<br>IG forecasts up to 30 million sterling hit from soaring Swiss franc<br>Factbox - Why scrapping the Swiss currency cap matters<br>From watchmakers to ski resorts, Swiss firms hammered by franc surge<br>Sterling surges to 7-year high against euro after shock SNB move<br>FTSE shrugs off Swiss jitters as gold miners rally<br>Three-month Swiss Libor falls to record low - 0.372 percent<br>Roche says spread of currencies mitigates impact of franc surge<br>SNB's Jordan says franc cap was not sustainable<br>Swiss franc surge hits emerging Europe bank sector<br>SNB looking at overall FX situation, not currency basket - Jordan"

#write.csv(eurreuters, "final_reuters_chart_data.csv", row.names = F)
hovertext1 <- paste0("Date:<b>", eurreuters$Date, "</b><br>",
                     "News:<b>", eurreuters$post.y, "</b><br>")

p = plot_ly(data= eurreuters,
        x = ~Date,
        yaxis = "y") %>%
      
  add_lines(data = eurreuters, x = ~Date, y = ~Close, line = list(color = "#00526d", width = 1),
         inherit = F, name = "EURUSD") %>% 
  
  add_lines(data = eurreuters, x = ~Date, y = ~USDJPY, line = list(color = "#F95959", width = 1),
         inherit = F, name = "USDJPY", visible= 'legendonly') %>% 
  
  add_lines(data = eurreuters, x = ~Date, y = ~USDCHF, line = list(color = "green", width = 1),
         inherit = F, name = "USDCHF", visible= 'legendonly') %>% 
  
  add_lines(data = eurreuters, x = ~Date, y = ~GBPUSD, line = list(color = "red", width = 1),
            inherit = F, name = "GBPUSD", visible= 'legendonly') %>% 
  
  add_lines(data = eurreuters, x = ~Date, y = ~XAUUSD, line = list(color = "gold", width = 1),
            inherit = F, name = "XAUUSD", visible= 'legendonly') %>% 
  
  add_lines(data = eurreuters, x = ~Date, y = ~AUDUSD, line = list(color = "blue", width = 1),
            inherit = F, name = "AUDUSD", visible= 'legendonly') %>% 
  
  add_lines(data = eurreuters, x = ~Date, y = ~NZDUSD, line = list(color = "black", width = 1),
            inherit = F, name = "NZDUSD", visible= 'legendonly') %>% 
  
  add_lines(data = eurreuters, x = ~Date, y = ~USDSEK, line = list(color = "orange", width = 1),
            inherit = F, name = "USDSEK", visible= 'legendonly') %>% 
  
  add_lines(data = eurreuters, x = ~Date, y = ~USDCAD, line = list(color = "violet", width = 1),
            inherit = F, name = "USDCAD", visible= 'legendonly') %>% 
  
  add_lines(data = eurreuters, x = ~Date, y = 1, line = list(color = "#00526d", width = 0),
            hoverinfo = text, text = hovertext1, inherit = F, name = "newsline") %>% 
  
  add_bars(data = eurreuters, x = ~Date, y = ~n,
           yaxis = "y2", inherit = F, name = "News", marker = list(color = "#33bbff")) %>% 
  
  layout(plot_bgcolor = "rgb(250,250,250)",
    xaxis = list(title = "", domain = c(0,0.95),
                 rangeslider = list(visible = F)),
    yaxis = list(domain = c(0.22, 0.9)),
    yaxis2 = list(domain = c(0, 0.18), side = "left"),
    
    hovermode = 'compare',
    
    annotations = list(x = 0, y = 1, xanchor = "left", yanchor = "top",
           xref = "paper", yref = "paper",
           text = paste0("<b>Reuters Scrapy Shiny FX Chart</b>"),
           font = list(size = 30, family = "serif"),
           showarrow = FALSE)
    )

p
install.packages("slickR")



eurreuters[eurreuters$Date == "2015-01-15",]['post.y'] = "Hedge funds, speculators face big losses on Swiss franc rally<br>Swiss central bank stuns market with policy U-turn<br>Euro floored as SNB decision puts spotlight on ECB, franc surges<br>Weak oil dictates mutual fund winners and losers in 2014<br>Lagarde said Swiss central bank did not warn IMF<br>Swiss shares post biggest one-day fall since 1989 on franc furore<br>Commodities traders' costs soar with Swiss franc's leap<br>Oil price plunge could leave helicopters sputtering<br>Oil settles down despite promising mid-day rally<br>Swiss franc soars after SNB drops cap on currency<br>Franc, European shares soar after Swiss currency cap lifted<br>Who wants to buy a hotel? Swiss Alps in shock over franc's rise<br>Capital flows to emerging markets to fall in 2015 for second straight year<br>Swiss banks hammered as earnings face hit from currency surge<br>Soaraway Swiss franc threatens downturn and deflation<br>ABB sees limited impact of strong Swiss franc against the euro<br>Pending deals seen unaffected by Swiss franc leap<br>IG forecasts up to 30 million sterling hit from soaring Swiss franc<br>Factbox - Why scrapping the Swiss currency cap matters<br>From watchmakers to ski resorts, Swiss firms hammered by franc surge<br>Sterling surges to 7-year high against euro after shock SNB move<br>FTSE shrugs off Swiss jitters as gold miners rally<br>Three-month Swiss Libor falls to record low - 0.372 percent<br>Roche says spread of currencies mitigates impact of franc surge<br>SNB's Jordan says franc cap was not sustainable<br>Swiss franc surge hits emerging Europe bank sector<br>SNB looking at overall FX situation, not currency basket - Jordan"
#<br>SNB's Jordan expects Swiss franc to ease from current levels
#<br>Lonza says Swiss franc surge will hurt competitiveness, visibility
#<br>Denmark may cut rates after Swiss drop franc cap - analysts
#<br>Swiss finance minister says confident SNB will ensure monetary stability
#<br>SNB says negative interest rates are already showing effects
#<br>Dollar claws back ground lost on retail sales disappointment
#<br>Downgrades for oil producers likely if crude doesn't recover - Fitch



eurreuters[eurreuters$Date == "2015-01-15",]['post.y']
eurreuters = eurreuters[-3]

eurreuters = eurreuters[eurreuters$Date > "2010-05-15",]
##############################################################################
library(tidyr)
#eurreuters %>% spread()

reut = reuters[c(1,5)]
reut = reut[order(reut$Date),]
reut$UpCount = sequence(rle(as.character(reut$Date))$lengths)
a = reut %>% spread(UpCount, title)


b = a %>% unite('post', 2:37, sep = "<br>", remove = T)

b$post = gsub("(<br>NA)", "", b$post)

eurreuters = merge(eurreuters3,b, by = 'Date', all.x = T)
eurreuters = merge(eurreuters,a, by = 'Date', all.x = T)

eurreuters = merge(eurreuters,aud, by = 'Date', all.x = T)
eurreuters = merge(eurreuters,gbp, by = 'Date', all.x = T)
eurreuters = merge(eurreuters,sek, by = 'Date', all.x = T)
eurreuters = merge(eurreuters,cad, by = 'Date', all.x = T)

#write.csv(eurreuters, "all_currencies_plotly.csv", row.names = F)
eurreuters = read_csv("all_currencies_plotly.csv")
##############################################################################
currencies = read_csv("currencies_9_plotly.csv")
currencies2 = merge(currencies ,b, by = 'Date', all.x = T) %>% order_by(Symbol)

currencies2 = currencies2 %>% order_by(Symbol)
currencies3 = currencies2[order(currencies2$Symbol),]
row.names(currencies3) = NULL
#write.csv(currencies3, "currencies_news_plotly.csv", row.names = F)




data_table = read_csv("reuters_plotly.csv")
data_table = data_table[-4]

data_table = data_table[,c(1,4,2,5,6,7,3)]
write.csv(data_table, "reuters_plotly_no_post.csv", row.names = )


