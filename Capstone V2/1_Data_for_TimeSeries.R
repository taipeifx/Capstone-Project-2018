#Currency Pairs:
#1. EURUSD
#2. XAUUSD
#3. GBPUSD
#4. NZDUSD
#5. AUDUSD
#6. USDJPY
#7. USDCHF
#8. USDCAD
#9. USDSEK

#Files Needed:
# for TimeSeries Analysis, Daily - Date, Open, High, Low, Close, Vol, Symbol  #adjusted to NYC time

# for Machine Learning Predictions, Minute - Date, Timestamp, Open, High, Low, Close, 
#for all pairs: Vol, TopWick, BottomWick, Range, Range*Vol, BBand.up, BBand.down, BBands distance, ATR10, ATR60, ATR240, ATR1440 (for all pairs?)
#ROC, MOM, 

######################################################################################## Time Series Analysis Data


library(readr)
library(dplyr)
library(anytime)
#getwd() #"D:/NYCDSA/Project 4 - Capstone Project v2"
setwd("D:/NYCDSA/Project 4 - Capstone Project v2")

#read csv : Date, Open, High, Low, Close, Vol
eur= read_csv("eurdaily.csv")
jpy= read_csv("jpydaily.csv")
chf= read_csv("chfdaily.csv")
xau= read_csv("xaudaily.csv")

aud= read_csv("auddaily.csv")
gbp= read_csv("gbpdaily.csv")
nzd= read_csv("nzddaily.csv")
cad= read_csv("caddaily.csv")
sek= read_csv("sekdaily.csv") #swedish krona

#format date column 
eur$Date <- anydate(eur$Date)
jpy$Date <- anydate(jpy$Date)
chf$Date <- anydate(chf$Date)
xau$Date <- anydate(xau$Date)

aud$Date <- anydate(aud$Date)
gbp$Date <- anydate(gbp$Date)
nzd$Date <- anydate(nzd$Date)
cad$Date <- anydate(cad$Date)
sek$Date <- anydate(sek$Date)

#turn minute data to daily ? if minute data. 
#auddaily = aud %>% group_by(Date) %>% summarise(Close = last(Close))
#gbpdaily = gbp %>% group_by(Date) %>% summarise(Close = last(Close))
#nzddaily = nzd %>% group_by(Date) %>% summarise(Close = last(Close))
#caddaily = cad %>% group_by(Date) %>% summarise(Close = last(Close))

#need symbol for groupby 
jpy$Symbol = "USDJPY"
chf$Symbol = "USDCHF"
xau$Symbol = "USDXAU"
eur$Symbol = "USDEUR"

aud$Symbol = "USDAUD"
gbp$Symbol = "USDGBP"
nzd$Symbol = "USDNZD"
cad$Symbol = "USDCAD"
sek$Symbol = "USDSEK"

#get usdxxx

eur[2:5] = 1/eur[2:5]
gbp[2:5] = 1/gbp[2:5]
nzd[2:5] = 1/nzd[2:5]
aud[2:5] = 1/aud[2:5]
xau[2:5] = 1/xau[2:5]
xau[2:5] = xau[2:5]/1000 #not divided by zero currencies_9_reg.csv

eur$Close = 1/eur$Close
xau$Close = 1/xau$Close 
gbp$Close = 1/gbp$Close
nzd$Close = 1/nzd$Close
aud$Close = 1/aud$Close


currencies_9 <- rbind(eur, jpy, chf, xau, aud, gbp, nzd, cad, sek)


#write.csv(currencies_9, "currencies_9_reg.csv", row.names = FALSE)
