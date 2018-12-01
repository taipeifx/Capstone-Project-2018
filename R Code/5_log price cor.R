#log correlation

##############################################################################
eur2007cor = eur[year(eur$Date) == 2007,][,c(1:3)]  #for year 2007
jpy2007cor = jpy[year(jpy$Date) == 2007,][,c(1:3)]
chf2007cor = chf[year(chf$Date) == 2007,][,c(1:3)]
xau2007cor = xau[year(xau$Date) == 2007,][,c(1:3)]

colnames(eur2007cor) = c("Date", "Timestamp","EUR")
colnames(jpy2007cor) = c("Date", "Timestamp","JPY")
colnames(chf2007cor) = c("Date", "Timestamp","CHF")
colnames(xau2007cor) = c("Date", "Timestamp","XAU")

eur2007cor$EUR = log(eur2007cor$EUR)
jpy2007cor$JPY = log(jpy2007cor$JPY)
chf2007cor$CHF = log(chf2007cor$CHF)
xau2007cor$XAU = log(xau2007cor$XAU)

###
hist(eur2007cor$EUR,xlab="Log Price", main="Histogram of EURUSD Log Price", xlim=c(0.25, 0.43), breaks = 5000) 
hist(eur$Open,xlab="Price", main="Histogram of EURUSD Price", breaks = 5000) 

ggplot(data=eur2007cor, aes(eur2007cor$EUR)) + 
  geom_histogram(breaks=seq(0.25, 0.43, by = 0.001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of EURUSD Log Price") +
  labs(x="Log Price", y="Count") + 
  xlim(c(0.25, 0.43)) #Histogram of EURUSD Log Price

ggplot(data=eur, aes(eur$Open)) + 
  geom_histogram(breaks=seq(1,1.6, by = 0.001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of EURUSD Price") +
  labs(x="Log Price", y="Count") + 
  xlim(c(1,1.6)) #Histogram of EURUSD Price
###

merge2007 = merge(chf2007cor, eur2007cor, by= c("Date","Timestamp"), all = TRUE)
merge2007 = merge(merge2007, jpy2007cor, by= c("Date","Timestamp"), all = TRUE)
merge2007 = merge(merge2007, xau2007cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2007[,c(3:6)], use = "complete.obs")
corr1
# 2007 Log Price Correlation
#CHF        EUR        JPY        XAU
#CHF  1.0000000 -0.9642598  0.8873526 -0.9291995
#EUR -0.9642598  1.0000000 -0.7763763  0.9402343
#JPY  0.8873526 -0.7763763  1.0000000 -0.8010201
#XAU -0.9291995  0.9402343 -0.8010201  1.0000000

# 2007 Correlation
#CHFOpen    EUROpen    JPYOpen    XAUOpen
#CHFOpen  1.0000000 -0.9687375  0.8827654 -0.9300167
#EUROpen -0.9687375  1.0000000 -0.7778656  0.9416268
#JPYOpen  0.8827654 -0.7778656  1.0000000 -0.8012310
#XAUOpen -0.9300167  0.9416268 -0.8012310  1.0000000

library(corrplot)
corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2007 Log Price Correlation", mar=c(0,0,2,0))
