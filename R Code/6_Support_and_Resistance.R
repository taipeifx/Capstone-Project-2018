
#colnames(eur)

#eurdate , 6 var
#[1] "Date"          "Timestamp"     "Open"          "High"          "Low"           "Close"

#14 var
#[1] "Date"          "Timestamp"     "Open"          "High"          "Low"           "Close"         "Symbol"        "Up"           
#[9] "Down"          "Neutral"       "Bar"           "Range"         "UpCount"       "DownCount"

#16 var
#[1] "Date"          "Timestamp"     "Open"          "High"          "Low"           "Close"         "Symbol"        "Up"           
#[9] "Down"          "Neutral"       "Bar"           "Range"         "UpCount"       "DownCount"     "Real.Range"    "Neutral.Range"

library(readr)
library(anytime)
library(tidyquant)
library(dplyr)

library(Hmisc)
library(psych)
setwd("D:/NYCDSA/Project 4 - Capstone Project")

#read csv
eur= read_csv("eurdate.csv") #4448014 obs
jpy= read_csv("jpydate.csv") #4410893
chf= read_csv("chfdate.csv") #4442023
xau= read_csv("xaudate.csv") #4235219

eurvol= read_csv("EURUSDvol.csv")
jpyvol= read_csv("USDJPYvol.csv")
chfvol= read_csv("USDCHFvol.csv")
xauvol= read_csv("XAUUSDvol.csv")

eurvol$Date <- anydate(eurvol$Date) #library(anytime)
jpyvol$Date <- anydate(jpyvol$Date) 
chfvol$Date <- anydate(chfvol$Date) 
xauvol$Date <- anydate(xauvol$Date) 

#add volume (+1 var) to eurdate , 6 var
#[1] "Date"          "Timestamp"     "Open"          "High"          "Low"           "Close"
eur = merge(eur, eurvol, by = c("Date", "Timestamp"), all = TRUE)
jpy = merge(jpy, jpyvol, by = c("Date", "Timestamp"), all = TRUE)
chf = merge(chf, chfvol, by = c("Date", "Timestamp"), all = TRUE)
xau = merge(xau, xauvol, by = c("Date", "Timestamp"), all = TRUE)

eur = eur[-3]
chf = chf[-3]
jpy = jpy[-3]
xau = xau[-3]

############################################################################## eur vol
summary(eurvol$Volume)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.00     61.55    134.06    247.81    273.30 133032.37

which.max(eurvol$Volume) #4016472
eurvol[4016470:4016482,]

hist(eurvol$Volume,xlab="Volume", main="Histogram of EURUSD Volume Range", xlim=c(0, 1000), breaks = 5000) 

ggplot(data=eurvol, aes(eurvol$Volume)) + 
  geom_histogram(breaks=seq(0, 1000, by = 10), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of EURUSD Minute Volume Range") +
  labs(x="Volume", y="Count") + 
  xlim(c(0, 1000)) #Histogram of EURUSD Minute Volume Range

#####mean, sd, etc
Hmisc::describe(eurvol$Volume) #library(Hmisc)
#       n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 4448014        0  1574111        1    247.8    283.7    19.33    30.74    61.55   134.06   273.30   616.90   903.60 
# lowest : 1.350000e-04 1.740000e-04 1.790000e-04 1.890000e-04 1.950000e-04, 
# highest: 4.194464e+04 4.896792e+04 6.070194e+04 9.059270e+04 1.330324e+05

ggplot(data=eurvol, aes(log(eurvol$Volume))) + 
  geom_histogram(breaks=seq(0, 10, by = 1), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of EURUSD Minute Volume Range") +
  labs(x="Volume", y="Count") + 
  xlim(c(0, 10)) #Histogram of EURUSD Minute Volume Range

############################################################################## vol

############################################################################## jpy vol
summary(jpyvol$Volume)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-21.40     50.78    105.14    323.03    229.80 136919.04

which.max(jpyvol$Volume) 

hist(jpyvol$Volume,xlab="Volume", main="Histogram of JPYUSD Volume Range", xlim=c(0, 800), breaks = 5000) 

ggplot(data=jpyvol, aes(jpyvol$Volume)) + 
  geom_histogram(breaks=seq(0, 800, by = 10), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of JPYUSD Minute Volume Range") +
  labs(x="Volume", y="Count") + 
  xlim(c(0, 800)) #Histogram of jpyUSD Minute Volume Range

#####mean, sd, etc
Hmisc::describe(jpyvol$Volume) #library(Hmisc)
# n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 4410893        0  1460814        1      323    465.3    13.10    23.06    50.78   105.14   229.80   591.00  1071.80 
# lowest :    -21.400000      0.000000      0.000034      0.000056      0.000058
# highest:  78322.000336  91723.000145 119056.000061 122339.600464 136919.041668

ggplot(data=jpyvol, aes(log(jpyvol$Volume))) + 
  geom_histogram(breaks=seq(0, 10, by = 1), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of jpyUSD Minute Volume Range") +
  labs(x="Volume", y="Count") + 
  xlim(c(0, 10)) #Histogram of jpyUSD Minute Volume Range

############################################################################## vol

############################################################################## chf vol
summary(chfvol$Volume)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00    39.10    88.31   236.92   184.86 82639.90

hist(chfvol$Volume,xlab="Volume", main="Histogram of chfUSD Volume Range", xlim=c(0, 800), breaks = 5000) 

ggplot(data=chfvol, aes(chfvol$Volume)) + 
  geom_histogram(breaks=seq(0, 800, by = 10), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of CHFUSD Minute Volume Range") +
  labs(x="Volume", y="Count") + 
  xlim(c(0, 800)) #Histogram of chfUSD Minute Volume Range

#####mean, sd, etc
Hmisc::describe(chfvol$Volume) #library(Hmisc)
#       n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#4442023        0  1312116        1    236.9    330.8     9.00    16.24    39.10    88.31   184.86   445.40   797.40 
#lowest : 1.06000e-04 1.11000e-04 1.21000e-04 1.23000e-04 1.28000e-04, 
#highest: 6.66588e+04 6.97889e+04 7.19835e+04 7.38792e+04 8.26399e+04

############################################################################## vol

############################################################################## xau vol
summary(xauvol$Volume)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00529 0.01665 0.02754 0.03831 4.91263 

hist(xauvol$Volume,xlab="Volume", main="Histogram of xauUSD Volume Range", xlim=c(0, 0.1), breaks = 2000) 

ggplot(data=xauvol, aes(xauvol$Volume)) + 
  geom_histogram(breaks=seq(0, 0.1, by = 0.001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of XAUUSD Minute Volume Range") +
  labs(x="Volume", y="Count") + 
  xlim(c(0, 0.1)) #Histogram of xauUSD Minute Volume Range

#####mean, sd, etc
Hmisc::describe(xauvol$Volume) #library(Hmisc)
#       n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 4235219        0   956631        1  0.02754  0.02999  0.00281  0.00354  0.00529  0.01665  0.03831  0.06698  0.08950 
# lowest : 3.60000e-08 4.40000e-08 4.50000e-08 4.68000e-08 4.98000e-08, 
# highest: 2.96180e+00 3.00020e+00 3.67700e+00 4.69200e+00 4.91263e+00

############################################################################## vol
#redo correlation with log values of price?
#about the same

write.csv(eur, "eurvol.csv", row.names = FALSE)
write.csv(jpy, "jpyvol.csv", row.names = FALSE)
write.csv(chf, "chfvol.csv", row.names = FALSE)
write.csv(xau, "xauvol.csv", row.names = FALSE)

#colnames(eurvol)
# 3 var
#[1] "Date" "Timestamp"   "Volume"


############################################################################## eur Support Resistance
#Support, resistance made into hist bins
#most common high low values
# eur, group by high/low, count
eurHigh = eur %>% group_by(High) %>% summarise(Count = n()) %>% arrange(desc(Count))

head(eurHigh)
# A tibble: 6 x 2
#High Count
#<dbl> <int>
#1  1.41  1281
#2  1.41  1027
#3  1.36   663
#4  1.36   643
#5  1.36   605
#6  1.36   595

# Minimal theme + blue fill color
ggplot(data=eurHigh, aes(x=High, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(title="A Count of Minute Bar Highs as EURUSD Resistance") +
  labs(x="Minute Bar High (Resistance)", y="Count")+
  ylim(c(0,500))

summary(eurHigh$Count) #56100 obs, resistance defined as more than mean count? or top 1 %
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   34.00   67.00   79.29  113.00 1281.00 

eurHigh %>% group_by(Count) %>% summarise(Count2 = n()) %>% arrange(desc(Count2))
# A tibble: 440 x 2
#Count Count2
#<int>  <int>
#1     1    484
#2    19    471
#3    15    465
#4    17    463
#5    47    463

eurHigh1 = eurHigh[1:561,] #1 percent, count 269
eurHigh1 = eurHigh[eurHigh$Count >= 269,]

ggplot(data=eurHigh1, aes(x=High, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(title="A Count of Minute Bar Highs as EURUSD Resistance") +
  labs(x="Minute Bar High (Resistance)", y="Count")

summary(eurHigh1$High)

hist(eurHigh$Count,xlab="Volume", xlim = c(0,400), main="Histogram of chfUSD Volume Range", breaks = 50) 


eurLow = eur %>% group_by(Low) %>% summarise(Count = n()) %>% arrange(desc(Count))
eurLow #56089
eurLow1 = eurLow[1:561,] #276
min(eurLow1$Count) #276
eurLow1 = eurLow[eurLow$Count >= 276,]

ggplot(data=eurLow, aes(x=Low, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(title="A Count of Minute Bar Lows as EURUSD Support") +
  labs(x="Minute Bar Low (Support)", y="Count")+ 
  ylim(c(0,500))

#save s/r 
write.csv(eurHigh1, "eurresistance.csv", row.names = FALSE)
write.csv(eurLow1, "eursupport.csv", row.names = FALSE)

#plot against chart of usd
#turn minute data to daily
eurusd = eur %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
#run debug before plotting
daily = eurusd %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close),
                   fill_up  = "darkgreen", fill_down  = "darkred")+
  labs(title = "EURUSD Resistance Lines Chart", y = "Price", x = "") + 
  coord_x_date(xlim = c("2007-1-1", "2018-11-30"), ylim = c(1.05, 1.59)) + #c(end - weeks(6), end)
  theme_tq()

daily+geom_hline(yintercept=eurHigh1$High[1:10], color = "red")

#might need support and resistance for every segment, 1.0 - 1.10, 1.1 - 1.2 
eurHighseg = eurHigh
eurHighseg$Seg = eurHighseg$High
#eurHighseg$Seg = signif(eurHighseg$Seg, digits = 2)
eurHighseg$Seg = findInterval(eurHighseg$Seg, seq(1, 1.6, by=0.05))

a = eurHighseg %>% group_by(Seg) %>% mutate(Max = max(Count))# %>% arrange(desc(Count))

# https://www.rforexcelusers.com/find-nth-largest-smallest-group/
a = eurHighseg %>% group_by(Seg) %>%
  mutate(rank = rank(desc(Count))) %>%
  arrange(rank)  # add segments, find highest rank for each segment

b = filter(a, rank<=1)
daily+geom_hline(yintercept=b$High, color = "red")


########## eur low
eurLowseg = eurLow
eurLowseg$Seg = eurLowseg$Low
eurLowseg$Seg = findInterval(eurLowseg$Seg, seq(1, 1.6, by=0.05))

c = eurLowseg %>% group_by(Seg) %>%
  mutate(rank = rank(desc(Count))) %>%
  arrange(rank)  # add segments, find highest rank for each segment

d = filter(c, rank<=1)
daily+geom_hline(yintercept=b$High, color = "red") + geom_hline(yintercept=d$Low, color = "blue")+
  labs(title = "EURUSD Support/Resistance Lines Chart", y = "Price", x = "") 

#save s/r 
write.csv(a, "eurresistance.csv", row.names = FALSE)
write.csv(c, "eursupport.csv", row.names = FALSE)


#distance from closest support / resistance
##############################################################################


############################################################################## jpy Support Resistance
jpyHigh = jpy %>% group_by(High) %>% summarise(Count = n()) %>% arrange(desc(Count))

# Minimal theme + blue fill color
ggplot(data=jpyHigh, aes(x=High, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(title = "Count of USDJPY M1 Bar Highs for Resistance", y = "Count", x = "USDJPY High")##

summary(jpyHigh$Count) #56100 obs, resistance defined as more than mean count? or top 1 %
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   24.00   66.00   88.92  122.00  910.00

#####
jpyLow = jpy %>% group_by(Low) %>% summarise(Count = n()) %>% arrange(desc(Count))

ggplot(data=jpyLow, aes(x=Low, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(title="Count of M1 Bar Lows as JPYUSD Support") +
  labs(x="Minute Bar Low (Support)", y="Count")

#plot against chart of jpy
#turn minute data to daily
jpyusd = jpy %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
#run debug before plotting
daily = jpyusd %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close),
                   fill_up  = "darkgreen", fill_down  = "darkred")+
  labs(title = "USDJPY Resistance Lines Chart", y = "Price", x = "") + 
  coord_x_date(xlim = c("2007-1-1", "2018-11-30"), ylim = c(75, 125)) +
  theme_tq()

#might need support and resistance for every segment, 1.0 - 1.10, 1.1 - 1.2 
jpyHighseg = jpyHigh
jpyHighseg$Seg = jpyHighseg$High
jpyHighseg$Seg = findInterval(jpyHighseg$Seg, seq(70, 125, by=5))
#seq(70, 120, by=10)

a1 = jpyHighseg %>% group_by(Seg) %>%
  mutate(rank = rank(desc(Count))) %>%
  arrange(rank)  # add segments, find highest rank for each segment

b1 = filter(a1, rank<=2)
daily+geom_hline(yintercept=b1$High, color = "red")


########## jpy low
jpyLowseg = jpyLow
jpyLowseg$Seg = jpyLowseg$Low
jpyLowseg$Seg = findInterval(jpyLowseg$Seg, seq(70, 125, by=5))

c1 = jpyLowseg %>% group_by(Seg) %>%
  mutate(rank = rank(desc(Count))) %>%
  arrange(rank)  # add segments, find highest rank for each segment

d1 = filter(c1, rank<=1)
daily+geom_hline(yintercept=b1$High, color = "red") + geom_hline(yintercept=d1$Low, color = "blue")+
  labs(title = "JPYUSD Support/Resistance Lines Chart", y = "Price", x = "") 

#save s/r 
write.csv(a1, "jpyresistance.csv", row.names = FALSE)
write.csv(c1, "jpysupport.csv", row.names = FALSE)


#distance from closest support / resistance
##############################################################################

############################################################################## chf Support Resistance
chfHigh = chf %>% group_by(High) %>% summarise(Count = n()) %>% arrange(desc(Count))

# Minimal theme + blue fill color
ggplot(data=chfHigh, aes(x=High, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(title = "Count of USDCHF M1 Bar Highs for Resistance", y = "Count", x = "USDchf High")##
  #xlim(c(1.0,1.2))

summary(chfHigh$Count) #56100 obs, resistance defined as more than mean count? or top 1 %
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   24.00   66.00   88.92  122.00  910.00

#####
chfLow = chf %>% group_by(Low) %>% summarise(Count = n()) %>% arrange(desc(Count))

ggplot(data=chfLow, aes(x=Low, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(title="Count of M1 Bar Lows as USDCHF Support") +
  labs(x="Minute Bar Low (Support)", y="Count")

#plot against chart of chf
#turn minute data to daily
chfusd = chf %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
#run debug before plotting
daily = chfusd %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close),
                   fill_up  = "darkgreen", fill_down  = "darkred")+
  labs(title = "USDCHF Resistance Lines Chart", y = "Price", x = "") + 
  coord_x_date(xlim = c("2007-1-1", "2018-11-30"), ylim = c(0.7, 1.25)) +
  theme_tq()

#might need support and resistance for every segment, 1.0 - 1.10, 1.1 - 1.2 
chfHighseg = chfHigh
chfHighseg$Seg = chfHighseg$High
chfHighseg$Seg = findInterval(chfHighseg$Seg, seq(0.7, 1.3, by=.05))
#seq(70, 120, by=10)

a1 = chfHighseg %>% group_by(Seg) %>%
  mutate(rank = rank(desc(Count))) %>%
  arrange(rank)  # add segments, find highest rank for each segment

b1 = filter(a1, rank<=1.5)
daily+geom_hline(yintercept=b1$High, color = "red")


########## chf low
chfLowseg = chfLow
chfLowseg$Seg = chfLowseg$Low
chfLowseg$Seg = findInterval(chfLowseg$Seg, seq(0.7, 1.3, by=.05))

c1 = chfLowseg %>% group_by(Seg) %>%
  mutate(rank = rank(desc(Count))) %>%
  arrange(rank)  # add segments, find highest rank for each segment

d1 = filter(c1, rank<=1)
daily+geom_hline(yintercept=b1$High, color = "red") + geom_hline(yintercept=d1$Low, color = "blue")+
  labs(title = "CHFUSD Support/Resistance Lines Chart", y = "Price", x = "") 

#save s/r 
write.csv(a1, "chfresistance.csv", row.names = FALSE)
write.csv(c1, "chfsupport.csv", row.names = FALSE)


#distance from closest support / resistance
##############################################################################

############################################################################## xau Support Resistance
xauHigh = xau %>% group_by(High) %>% summarise(Count = n()) %>% arrange(desc(Count))

# Minimal theme + blue fill color
ggplot(data=xauHigh, aes(x=High, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(title = "Count of XAUUSD M1 Bar Highs for Resistance", y = "Count", x = "USDXAU High")+##
  ylim(c(0,50))

summary(xauHigh$Count) #56100 obs, resistance defined as more than mean count? or top 1 %
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   2.000   4.459   5.000 170.000 

#####
xauLow = xau %>% group_by(Low) %>% summarise(Count = n()) %>% arrange(desc(Count))

ggplot(data=xauLow, aes(x=Low, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+
  labs(title="Count of M1 Bar Lows as XAUUSD Support") +
  labs(x="Minute Bar Low (Support)", y="Count")+
  ylim(c(0,70))

#plot against chart of xau
#turn minute data to daily
xauusd = xau %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
#run debug before plotting
daily = xauusd %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close),
                   fill_up  = "darkgreen", fill_down  = "darkred")+
  labs(title = "XAUUSD Resistance Lines Chart", y = "Price", x = "") + 
  coord_x_date(xlim = c("2007-1-1", "2018-11-30"), ylim = c(600, 1900)) +
  theme_tq()

#might need support and resistance for every segment, 1.0 - 1.10, 1.1 - 1.2 
xauHighseg = xauHigh
xauHighseg$Seg = xauHighseg$High
xauHighseg$Seg = findInterval(xauHighseg$Seg, seq(600, 1900, by=100))
#seq(70, 120, by=10)

a1 = xauHighseg %>% group_by(Seg) %>%
  mutate(rank = rank(desc(Count))) %>%
  arrange(rank)  # add segments, find highest rank for each segment

b1 = filter(a1, rank<=1.5)
daily+geom_hline(yintercept=b1$High, color = "red")


########## xau low
xauLowseg = xauLow
xauLowseg$Seg = xauLowseg$Low
xauLowseg$Seg = findInterval(xauLowseg$Seg, seq(600, 1900, by=100))

c1 = xauLowseg %>% group_by(Seg) %>%
  mutate(rank = rank(desc(Count))) %>%
  arrange(rank)  # add segments, find highest rank for each segment

d1 = filter(c1, rank<=1.5)
daily+geom_hline(yintercept=b1$High, color = "red") + geom_hline(yintercept=d1$Low, color = "blue")+
  labs(title = "XAUUSD Support/Resistance Lines Chart", y = "Price", x = "") 

#save s/r 
write.csv(a1, "xauresistance.csv", row.names = FALSE)
write.csv(c1, "xausupport.csv", row.names = FALSE)


#distance from closest support / resistance
##############################################################################


################################################################################### 
################################################################################### 
################################################################################### eur distance
#distance from closest support / resistance
euvol

eurR = read_csv("eurresistance.csv")
eurS = read_csv("eursupport.csv")

eurRa = filter(eurR, rank<=5)
eurSa = filter(eurS, rank<=5)

library(data.table)
dt = data.table(eurRa[1], val = eurRa[1]) # you'll see why val is needed in a sec
sort(eurRa[1])

sort(eurRa$High)
dt
setattr(dt, "sorted", "w")  # let data.table know that w is sorted

#can't use S/R to predict since it takes time to build s/r
#use distance from high to close, distance from close to low
################################################################################### 


################################################################################### 
#if were to predict, range and direction

################################################################################### TO DO 
#do by hour V
#stats by year V

#install performanceAnalytics
#tidyquant

#Support, resistance made into hist bins? V
#candlestick formation 
#tech analysis, wilder stuff

#use distance from high to close, distance from close to low
#vwap
#MA
#bollinger band
#RSI
#etc.

### end goal
#vwap
#limit orders cause price range segmentation
#cluster analysis, heirchical , use python.
#aiko.liu@nycdatascience.com
#ACF
#arbitrage
#need to use tools 
###

#time series, autocorrelation? 

#https://www.quantinsti.com/blog/forecasting-stock-returns-using-arima-model
#https://www.quantinsti.com/blog/predictive-modeling-algorithmic-trading