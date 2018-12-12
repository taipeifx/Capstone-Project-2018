# predict price of EURUSD with CHF, JPY, XAU
#columns
#1. date
#2. timestamp
#3. Real.Range (as diff between High / Low) (predicting this)
#4. OC.Range (diff between Open/ Close)
#5. HO.Range (High / Open)
#6. CL.Range (Close / Low)
#7-11 JPY open, JPY 4 ranges
#12-16. CHF open, CHF 4 ranges
#17-21. XAU open, XAU 4 ranges
#10. volume 
#11. Bar
#12. UpCount
#13. DownCount
#14. tech analysis
#
#
#[1] "Date"          "Timestamp"     "Open"          "High"          "Low"           "Close"         "Symbol"        "Up"           
#[9] "Down"          "Neutral"       "Bar"           "Range"         "UpCount"       "DownCount"     "Real.Range"    "Neutral.Range"

#https://www.quantconnect.com/login


library(readr)
library(anytime)
library(tidyquant)
library(dplyr)

library(Hmisc)
library(psych)
setwd("D:/NYCDSA/Project 4 - Capstone Project")

#read csv
eur= read_csv("eur16var.csv") #4448014 obs
jpy= read_csv("jpy16var.csv") #4410893
chf= read_csv("chf16var.csv") #4442023
xau= read_csv("xau16var.csv") #4235219

eurvol= read_csv("EURUSDvol.csv")
jpyvol= read_csv("USDJPYvol.csv")
chfvol= read_csv("USDCHFvol.csv")
xauvol= read_csv("XAUUSDvol.csv")

eurvol$Date <- anydate(eurvol$Date) #library(anytime)
jpyvol$Date <- anydate(jpyvol$Date) 
chfvol$Date <- anydate(chfvol$Date) 
xauvol$Date <- anydate(xauvol$Date) 

################################################################################### 
#if were to predict, range and direction
#columns
#1. date
#2. timestamp

#eur = eur[-17]
#3. Real.Range (as diff between High / Low) (predicting this)
eur = eur %>% mutate(Real.Range = ifelse(Bar == "Up", (High-Low), ifelse(Bar == "Down", (Low - High), 
                                  ifelse(High == Low, 0, NA)))) # Neutral NA, 0 values counted

#4. Body(diff between Open/ Close), have -
eur = eur %>% mutate(Body = (Close - Open))# up bar if Close > Open

#5. TopWick (Open - High, High-Close), have -
eur = eur %>% mutate(TopWick = ifelse(Bar == "Down", (Open - High), (High - Close)))

#6. BottomWIck (Low - Close, Open - Low), have -
eur = eur %>% mutate(BottomWick = ifelse(Bar == "Down",(Low - Close), (Open - Low)))


#7,8 JPY open, JPY Real.Range, Volume
#9,10. CHF open, CHF Real.Range
#11,12. XAU open, XAU Real.Range
#13. volume
eurvol= read_csv("EURUSDvol.csv")
jpyvol= read_csv("USDJPYvol.csv")
chfvol= read_csv("USDCHFvol.csv")
xauvol= read_csv("XAUUSDvol.csv")

colnames(eurvol)[3] = "EURVolume"
eur = merge(eur, eurvol, by= c("Date","Timestamp"), all.x = TRUE)

jpy = jpy %>% mutate(JPYRange= ifelse(Bar == "Down", Range * -1, Range))
jpy = jpy[c(1,2,3,17)]
colnames(jpyvol)[3] = "JPYVolume"
colnames(jpy)[3] = "JPYOpen"
jpy = merge(jpy, jpyvol, by= c("Date","Timestamp"), all.x = TRUE)

chf = chf %>% mutate(CHFRange= ifelse(Bar == "Down", Range * -1, Range))
chf = chf[c(1,2,3,17)]
colnames(chfvol)[3] = "CHFVolume"
colnames(chf)[3] = "CHFOpen"
chf = merge(chf, chfvol, by= c("Date","Timestamp"), all.x = TRUE)

xau = xau %>% mutate(XAURange= ifelse(Bar == "Down", Range * -1, Range))
xau = xau[c(1,2,3,17)]
colnames(xauvol)[3] = "XAUVolume"
colnames(xau)[3] = "XAUOpen"
xau = merge(xau, xauvol, by= c("Date","Timestamp"), all.x = TRUE)

eur = merge(eur, jpy, by= c("Date","Timestamp"), all.x = TRUE)
eur = merge(eur, chf, by= c("Date","Timestamp"), all.x = TRUE)
eur = merge(eur, xau, by= c("Date","Timestamp"), all.x = TRUE)

#eur = eur[,-c(21:29)]
#use close prices
jpy = jpy %>% mutate(JPYRange= ifelse(Bar == "Down", Range * -1, Range))
jpy = jpy[c(1,2,6,17)]
colnames(jpy)[3] = "JPYClose"
jpy = merge(jpy, jpyvol, by= c("Date","Timestamp"), all.x = TRUE)

head(chf$Close)
chf = chf %>% mutate(CHFRange= ifelse(Bar == "Down", Range * -1, Range))
chf = chf[c(1,2,6,17)]
colnames(chf)[3] = "CHFClose"
chf = merge(chf, chfvol, by= c("Date","Timestamp"), all.x = TRUE)

xau = xau %>% mutate(XAURange= ifelse(Bar == "Down", Range * -1, Range))
xau = xau[c(1,2,6,17)]
colnames(xau)[3] = "XAUClose"
xau = merge(xau, xauvol, by= c("Date","Timestamp"), all.x = TRUE)

eur = merge(eur, jpy, by= c("Date","Timestamp"), all.x = TRUE)
eur = merge(eur, chf, by= c("Date","Timestamp"), all.x = TRUE)
eur = merge(eur, xau, by= c("Date","Timestamp"), all.x = TRUE)
#14. Bar
#15,16 UpCount / DownCount
eur$UpCount = sequence(rle(as.character(eur$Up))$lengths) #https://stackoverflow.com/questions/19998836/r-count-consecutive-occurrences-of-values-in-a-single-column
eur[which(is.na(eur$Up)),]["UpCount"] = NA
##--##
eur$DownCount = sequence(rle(as.character(eur$Down))$lengths) 
eur[which(is.na(eur$Down)),]["DownCount"] = NA

#17. tech analysis
#see 8_Technical_Analysis

##To know the current storage capacity
memory.limit()
#[1] 8103
## To increase the storage capacity
memory.limit(size=56000)
#[1] 56000    
## I did this to increase my storage capacity to 7GB

#save file
#write.csv(eur, "eur29var.csv", row.names = FALSE)
################################################################################### TO DO 

#hypothetical question, if SF of 1st floor was 1000, SF of basement was 1200, and pricing mattered if SF1 > SFBasement, would we need a new column 
#stating if basement was larger or smaller than 1st floor?

#do by hour V
#stats by year V

#install performanceAnalytics
#tidyquant

#Support, resistance made into hist bins? V
#candlestick formation V
#tech analysis, wilder stuff V

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