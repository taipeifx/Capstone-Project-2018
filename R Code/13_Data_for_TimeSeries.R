
library(readr)
library(dplyr)
#getwd() #"D:/NYCDSA/Project 4 - Capstone Project"
setwd("D:/NYCDSA/Project 4 - Capstone Project")

eur= read_csv("eurdate.csv")
jpy= read_csv("jpydate.csv")
chf= read_csv("chfdate.csv")
xau= read_csv("xaudate.csv")


eurdaily = eur %>% group_by(Date) %>% summarise(Close = last(Close))
jpydaily = jpy %>% group_by(Date) %>% summarise(Close = last(Close))
chfdaily = chf %>% group_by(Date) %>% summarise(Close = last(Close))
xaudaily = xau %>% group_by(Date) %>% summarise(Close = last(Close))

jpydaily$Symbol = "USDJPY"
chfdaily$Symbol = "USDCHF"
xaudaily$Symbol = "USDXAU"
eurdaily$Symbol = "USDEUR"

1/xaudaily[2][1,]
#get usdeur
eurdaily$Close = 1/eurdaily$Close 
xaudaily$Close = 1/xaudaily$Close 

currencies = rbind(eurdaily,xaudaily)
currencies = rbind(currencies,jpydaily)
currencies = rbind(currencies,chfdaily)


#write.csv(currencies,"currencies.csv", row.names = F)








########################################################################################
library(readr)
library(dplyr)
library(lubridate) 

#repeat for other currencies
currencies= read_csv("currencies.csv")
aud= read_csv("AUDUSDdaily.csv")
gbp= read_csv("GBPUSDdaily.csv")
nzd= read_csv("NZDUSDdaily.csv")
cad= read_csv("USDCADdaily.csv")
sek= read_csv("") #swedish krona

#format date column 
aud$Date <- anydate(aud$Date)
gbp$Date <- anydate(gbp$Date)
nzd$Date <- anydate(nzd$Date)
cad$Date <- anydate(cad$Date)

#turn minute data to daily ? if minute data. 
auddaily = aud %>% group_by(Date) %>% summarise(Close = last(Close))
gbpdaily = gbp %>% group_by(Date) %>% summarise(Close = last(Close))
nzddaily = nzd %>% group_by(Date) %>% summarise(Close = last(Close))
caddaily = cad %>% group_by(Date) %>% summarise(Close = last(Close))

#need symbol for groupby 
auddaily$Symbol = "USDAUD"
gbpdaily$Symbol = "USDGBP"
nzddaily$Symbol = "USDNZD"
caddaily$Symbol = "USDCAD"

#get usdxxx
auddaily$Close = 1/auddaily$Close 
gbpdaily$Close = 1/gbpdaily$Close
nzddaily$Close = 1/nzddaily$Close

currencies_8 <- rbind(currencies, auddaily, gbpdaily, nzddaily, caddaily) 


#write.csv(currencies_8, "currencies_8", row.names = FALSE)
