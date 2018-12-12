
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
#define instrument as SEK for further feature construction
SEK = sek

#turn date into datetime object
SEK$Date <- anydate(SEK$Date)
#class(SEK$Timestamp)

#Range
SEK = SEK %>% mutate(SEK.Range = (High - Low))

#Body(diff between Open/ Close), has -
SEK = SEK %>% mutate(SEK.Body = (Close - Open))# up bar if Close > Open

#TopWick (High-Open for downbar, High-Close for upbar)
SEK = SEK %>% mutate(SEK.TopWick = ifelse(SEK.Body <= 0, (High - Open), (High - Close)))

#BottomWIck (Close-Low for downbar, Open-Low for upbar)
SEK = SEK %>% mutate(SEK.BotWick = ifelse(SEK.Body <= 0, (Close - Low), (Open - Low)))

SEK$SEK.Body = abs(SEK$SEK.Body)

###tech analysis

#library(quantmod); #library(TTR); library(caret);library(corrplot);library(pROC);library(FSelector);


#Force Index Indicator - force is an indicator that uses range * volume 
SEK = SEK %>% mutate(SEK.Force = (SEK.Range * Volume))


#Edit columns
colnames(SEK)[7] = "SEK.Vol"
#SEK <- SEK %>% select(SEK.Range, everything())
#colnames(SEK)[1] = "Range"
#SEK$Range = SEK$Range * 10000

# Price change Indicators (ROC and Momentum)
# The Rate of Change (ROC) indicator measures the percentage change of the current price as compared to the price a certain number of periods ago
SEK.ROC10 = ROC(SEK$Close, n = 10,type ="discrete")*100 #quantmod
SEK.ROC60 = ROC(SEK$Close, n = 60,type ="discrete")*100
SEK.ROC240 = ROC(SEK$Close, n = 240,type ="discrete")*100 

# MOM is a leading indicator measuring a security's rate-of-change
SEK.MOM10 = momentum(SEK$Close, n = 10, na.pad = TRUE)
SEK.MOM60 = momentum(SEK$Close, n = 60, na.pad = TRUE)
SEK.MOM240 = momentum(SEK$Close, n = 240, na.pad = TRUE)


# Volatility signal Indicator (ATR)
#True range (TR) is a measure of volatility of a High-Low-Close series; average true range (ATR) is a 
#Welles Wilder's style moving average of the TR. Developed by J. Welles Wilder in 1978.
SEK.ATR10 = ATR(SEK[,c("High","Low","Close")], n = 10, maType="EMA")[,2]
SEK.ATR60 = ATR(SEK[,c("High","Low","Close")], n = 60, maType="EMA")[,2]
SEK.ATR240 = ATR(SEK[,c("High","Low","Close")], n = 240, maType="EMA")[,2]

#Bollinger Bands
#%B quantifies a security's price relative to the upper and lower Bollinger Band. 
BB = BBands(SEK[,c("High","Low","Close")])
BB = data.frame(BB)

#colnames(BBands) #"dn"   "mavg" "up"   "pctB"
BB = BB[-c(2,4)]
colnames(BB) = c("SEK.BBdn","SEK.BBup")
BB = BB %>% mutate(SEK.BBrng = (SEK.BBup - SEK.BBdn))

SEK = SEK[-c(3:6)]

## Combining all the Indicators and the Class into one dataframe
datasetNA = data.frame(SEK,SEK.ROC10,SEK.ROC60,SEK.ROC240,SEK.MOM10,SEK.MOM60,SEK.MOM240,SEK.ATR10,SEK.ATR60,SEK.ATR240,BB)
datasetNA[9:14] = abs(datasetNA[9:14])

#datasetNA = tibble::as_tibble(datasetNA)
write.csv(datasetNA, "SEK20var.csv", row.names = F)

#eur = read_csv("EUR21var.csv")

#sum(is.na(datasetNA$SEK.ROC240))
##################################################################################################################
##################################################################################################################
##################################################################################################################
#correlation plot below if needed
dataset = na.omit(datasetNA)

## Understanding the dataset using descriptive statistics
#print(head(dataset),5)
#dim(dataset)

#summary(dataset)

##  Visualizing the dataset using a correlation matrix
correlations = cor(dataset[,c(3:20)])
#print(head(correlations))
#library(corrplot)
corrplot(correlations, method="circle", title = "USDSEK Jan 2007- Dec 2018 Correlations", mar=c(0,0,2,0))
#858 x 631


########################################## timestamp update
xau= read_csv("XAU20var.csv")
xautime = read_csv("xautime.csv")
xau$Timestamp = xautime$Timestamp
xau = tibble::as_tibble(xau)
write.csv(xau, "XAU20var_time.csv", row.names = F)

aud= read_csv("AUD20var.csv")
audtime = read_csv("audtime.csv")
aud$Timestamp = audtime$Timestamp
aud = tibble::as_tibble(aud)
write.csv(aud, "AUD20var_time.csv", row.names = F)

nzd= read_csv("NZD20var.csv")
nzdtime = read_csv("nzdtime.csv")
nzd$Timestamp = nzdtime$Timestamp
nzd = tibble::as_tibble(nzd)
write.csv(nzd, "NZD20var_time.csv", row.names = F)

cad= read_csv("CAD20var.csv")
cadtime = read_csv("cadtime.csv")
cad$Timestamp = cadtime$Timestamp
cad = tibble::as_tibble(cad)
write.csv(cad, "CAD20var_time.csv", row.names = F)

#sek= read_csv("SEK20var.csv", col_types = cols(Volume = col_double()))
#sektime = read_csv("sektime.csv")
sek$Timestamp = sektime$Timestamp
sek = tibble::as_tibble(sek)
#write.csv(sek, "SEK20var_time.csv", row.names = F)

chf= read_csv("CHF20var.csv")
chftime = read_csv("chftime.csv")
chf$Timestamp = chftime$Timestamp
chf = tibble::as_tibble(chf)
write.csv(chf, "CHF20var_time.csv", row.names = F)

gbp= read_csv("GBP20var.csv")
gbptime = read_csv("gbptime.csv")
gbp$Timestamp = gbptime$Timestamp
gbp = tibble::as_tibble(gbp)
write.csv(gbp, "GBP20var_time.csv", row.names = F)

##
jpy= read_csv("JPY20var.csv")
jpytime = read_csv("jpytime.csv")
jpy$Timestamp = jpytime$Timestamp
write.csv(jpy, "JPY20var_time.csv", row.names = F)


eur = read_csv("EUR21var.csv")
eurtime = read_csv("eurtime.csv")
eur$Timestamp = eurtime$Timestamp
write.csv(eur, "EUR21var_time.csv", row.names = F)


class(eur$Timestamp)
class(eurtime$Timestamp)  

