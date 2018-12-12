
# for Machine Learning Predictions, Minute - Date, Timestamp, Open, High, Low, Close, Vol 
#for all pairs: TopWick, BottomWick, Range, Range*Vol, BBand.up, BBand.down, BBands range, ATR10, ATR60, ATR240, ROCx3, MOMx3

######################################################################################## Machine Learning Predictions Data

library(readr) ; library(dplyr) ; library(anytime) ; library(quantmod)
#getwd() #"D:/NYCDSA/Project 4 - Capstone Project"
setwd("D:/NYCDSA/Project 4 - Capstone Project v2")

#read csv minute data: Date, Timestamp, Open, High, Low, Close, Vol
eur= read_csv("eurmin.csv")
jpy= read_csv("jpymin.csv")
chf= read_csv("chfmin.csv")
xau= read_csv("xaumin.csv")

aud= read_csv("audmin.csv")
gbp= read_csv("gbpmin.csv")
nzd= read_csv("nzdmin.csv")
cad= read_csv("cadmin.csv")
sek= read_csv("sekmin.csv", col_types = cols( Date = col_integer(),
                                              Timestamp = col_time(format = ""),
                                              Open = col_double(),
                                              High = col_double(),
                                              Low = col_double(),
                                              Close = col_double(),
                                              Volume = col_double()
))

#cur = read_csv("currencies_9_reg.csv")

#remember to lag in the end

#search replace below:
############################
#define instrument as EUR for further feature construction
EUR = eur 

#turn date into datetime object
EUR$Date <- anydate(EUR$Date)
class(EUR$Timestamp)

#Range
EUR = EUR %>% mutate(EUR.Range = (High - Low))

#Body(diff between Open/ Close), has -
EUR = EUR %>% mutate(EUR.Body = (Close - Open))# up bar if Close > Open

#TopWick (High-Open for downbar, High-Close for upbar)
EUR = EUR %>% mutate(EUR.TopWick = ifelse(EUR.Body <= 0, (High - Open), (High - Close)))

#BottomWIck (Close-Low for downbar, Open-Low for upbar)
EUR = EUR %>% mutate(EUR.BotWick = ifelse(EUR.Body <= 0, (Close - Low), (Open - Low)))

EUR$EUR.Body = abs(EUR$EUR.Body)

###tech analysis

#library(quantmod); #library(TTR); library(caret);library(corrplot);library(pROC);library(FSelector);


#Force Index Indicator - force is an indicator that uses range * volume 
EUR = EUR %>% mutate(EUR.Force = (EUR.Range * Volume))


#Edit columns
colnames(EUR)[7] = "EUR.Vol"
#EUR <- EUR %>% select(EUR.Range, everything())
#colnames(EUR)[1] = "Range"
#EUR$Range = EUR$Range * 10000

# Price change Indicators (ROC and Momentum)
# The Rate of Change (ROC) indicator measures the percentage change of the current price as compared to the price a certain number of periods ago
EUR.ROC10 = ROC(EUR$Close, n = 10,type ="discrete")*100 #quantmod
EUR.ROC60 = ROC(EUR$Close, n = 60,type ="discrete")*100
EUR.ROC240 = ROC(EUR$Close, n = 240,type ="discrete")*100 

# MOM is a leading indicator measuring a security's rate-of-change
EUR.MOM10 = momentum(EUR$Close, n = 10, na.pad = TRUE)
EUR.MOM60 = momentum(EUR$Close, n = 60, na.pad = TRUE)
EUR.MOM240 = momentum(EUR$Close, n = 240, na.pad = TRUE)


# Volatility signal Indicator (ATR)
#True range (TR) is a measure of volatility of a High-Low-Close series; average true range (ATR) is a 
#Welles Wilder's style moving average of the TR. Developed by J. Welles Wilder in 1978.
EUR.TR = ATR(eur[,c("High","Low","Close")], n = 10, maType="EMA")[,1]
EUR.ATR10 = ATR(eur[,c("High","Low","Close")], n = 10, maType="EMA")[,2]
EUR.ATR60 = ATR(eur[,c("High","Low","Close")], n = 60, maType="EMA")[,2]
EUR.ATR240 = ATR(eur[,c("High","Low","Close")], n = 240, maType="EMA")[,2]

#Bollinger Bands
#%B quantifies a security's price relative to the upper and lower Bollinger Band. 
BB = BBands(EUR[,c("High","Low","Close")])
BB = data.frame(BB)

#colnames(BBands) #"dn"   "mavg" "up"   "pctB"
BB = BB[-c(2,4)]
colnames(BB) = c("EUR.BBdn","EUR.BBup")
BB = BB %>% mutate(EUR.BBrng = (EUR.BBup - EUR.BBdn))

EUR = EUR[-c(4:7)]

## Combining all the Indicators and the Class into one dataframe
datasetNA = data.frame(EUR,EUR.ROC10,EUR.ROC60,EUR.ROC240,EUR.MOM10,EUR.MOM60,EUR.MOM240,EUR.TR,EUR.ATR10,EUR.ATR60,EUR.ATR240,BB)
datasetNA[9:14] = abs(datasetNA[9:14])

#write.csv(datasetNA, "EUR21var.csv", row.names = F)

##################################################################################################################
##################################################################################################################
##################################################################################################################
#correlation plot below if needed
dataset = na.omit(datasetNA)

## Understanding the dataset using descriptive statistics
print(head(dataset),5)
dim(dataset)

summary(dataset)

##  Visualizing the dataset using a correlation matrix
correlations = cor(dataset[,c(1,4:21)])
print(head(correlations))
library(corrplot)
corrplot(correlations, method="circle", title = "EURUSD Jan 2007- Dec 2018 Correlations", mar=c(0,0,2,0))

