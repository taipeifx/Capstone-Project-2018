
library(readr)
library(anytime)
library(tidyquant)
library(dplyr)

library(Hmisc)
library(psych)
setwd("D:/NYCDSA/Project 4 - Capstone Project")

#read csv
eur= read_csv("eur14var.csv")
jpy= read_csv("jpy14var.csv")
chf= read_csv("chf14var.csv")
xau= read_csv("xau14var.csv")

#do up/down bars by hour 
#turn minute data to hourly
eurusd = eur %>% group_by(Date, hour(Timestamp)) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
usdjpy = jpy %>% group_by(Date, hour(Timestamp)) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
usdchf = chf %>% group_by(Date, hour(Timestamp)) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
xauusd = xau %>% group_by(Date, hour(Timestamp)) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))


#need symbol for groupby 
usdjpy$Symbol = "USDJPY"
usdchf$Symbol = "USDCHF"
xauusd$Symbol = "XAUUSD"
eurusd$Symbol = "EURUSD"

#bar column
eurusd = eurusd %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down")))
usdjpy = usdjpy %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down")))
usdchf = usdchf %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down")))
xauusd = xauusd %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down")))


total <- rbind(usdjpy, usdchf, xauusd, eurusd) 


total %>% group_by(Symbol, Bar) %>% summarise(Upcount = n()) #hourly up/down by currency

#Symbol Bar     Upcount
#<chr>  <chr>     <int>
#1 EURUSD Down      36676
#2 EURUSD Neutral     452
#3 EURUSD Up        37096
#4 USDCHF Down      36535
#5 USDCHF Neutral     880
#6 USDCHF Up        36810
#7 USDJPY Down      35962
#8 USDJPY Neutral     792
#9 USDJPY Up        37136
#10 XAUUSD Down      35762
#11 XAUUSD Neutral     415
#12 XAUUSD Up        36324


###################################################################################eur
#1 EURUSD Down      36676
#2 EURUSD Neutral     452
#3 EURUSD Up        37096

#excluding neutral bars, total bars: 
37096+36676 #= 73772
#% of Up Bars: 
37096 / 73772 #= 0.5028466 (0.5005132 for minutes)

#neutral count
452/ (452+37096+36676) #0.006089674 (0.06435097 for minutes)

#dbinom
dbinom(0:5,5,0.5) #0.03125 0.15625 0.31250 0.31250 0.15625 0.03125 !!
dbinom(1:20, size= 1:20,0.5)
#[1] 5.000000e-01 2.500000e-01 1.250000e-01 6.250000e-02 3.125000e-02 1.562500e-02 7.812500e-03 3.906250e-03 1.953125e-03
#[10] 9.765625e-04 4.882812e-04 2.441406e-04 1.220703e-04 6.103516e-05 3.051758e-05 1.525879e-05 7.629395e-06 3.814697e-06
#[19] 1.907349e-06 9.536743e-07

#qbinom
n<- 73772
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#36620 37152

binom.test(37096, 73772, 1/2, alternative = "two.sided") #up
#Exact binomial test
#data:  37096 and 73772
#number of successes = 37096, number of trials = 73772, p-value = 0.1229
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.4992318 0.5064612
#sample estimates:
#  probability of success 
#0.5028466 
# hourly does not reject the null hypothesis
##############################################################################

###################################################################################jpy
#7 USDJPY Down      35962
#8 USDJPY Neutral     792
#9 USDJPY Up        37136

#excluding neutral bars, total bars: 
37136+35962 #= 73098
#% of Up Bars: 
37136 / 73098 #= 0.5080303 (0.4957031 minutes)

#neutral count
792/ (792+37136+35962) #0.01071864 (0.1180856 minutes)

#qbinom
n<- 73098
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#1943082 1946948

binom.test(37136, 73098, 1/2, alternative = "two.sided") #up
#Exact binomial test
#data:  37136 and 73098
#number of successes = 37136, number of trials = 73098, p-value = 1.434e-05
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.504399 0.511661
#sample estimates:
#  probability of success 
#0.5080303 
#hourly does not pass the random walk test, reject null hypothesis
##############################################################################

###################################################################################chf
#4 USDCHF Down      36535
#5 USDCHF Neutral     880
#6 USDCHF Up        36810

#excluding neutral bars, total bars: 
36810+36535 #= 73345
#% of Up Bars: 
36810 / 73345 #= 0.5018747 (0.4989491 minute), because the range of the minute up bars is greater than down bars

#neutral count
880/ (880+36810+36535) # 0.01185584 (0.133599 minute)

#qbinom
n<- 73345
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#36407 36938

binom.test(36810, 73345, 1/2, alternative = "two.sided") #up
#	Exact binomial test
#data:  36810 and 73345
#number of successes = 36810, number of trials = 73345, p-value = 0.3117
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.4982494 0.5054999
#sample estimates:
#  probability of success 
#0.5018747 

#chf hourly is random walk 
##############################################################################

###################################################################################xau 
#10 XAUUSD Down      35762
#11 XAUUSD Neutral     415
#12 XAUUSD Up        36324

#excluding neutral bars, total bars: 
36324+35762 #= 72086
#% of Up Bars: 
36324 / 72086 #= 0.5038981 (0.4973676 minute)

#neutral count
415/ (415+36324+35762) #0.005724059 (0.02256719 minute)

#qbinom
n<- 72086
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#35780 36306

binom.test(36324, 72086, 1/2, alternative = "two.sided") #up
#	Exact binomial test
#data:  36324 and 72086
#number of successes = 36324, number of trials = 72086, p-value = 0.03666
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.5002412 0.5075547
#sample estimates:
#  probability of success 
#0.5038981 

#stat sig at 5%
##############################################################################

############################################################################## all?
total %>% group_by(Bar) %>% summarise(Upcount = n()) #daily up/down by currency
#Bar     Upcount
#<chr>     <int>
#1 Down     144935
#2 Neutral    2539
#3 Up       147366

#excluding neutral bars, total bars: 
147366+144935 #= 292301
#% of Up Bars: 
147366 / 292301 #= 0.5041584 (0.4981596 total minutes)

#neutral count
2539/ (2539+147366+144935) #0.00861145 (0.08531657 total minute)

#qbinom
n<- 292301
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#145621 146680

binom.test(147366, 292301, 1/2, alternative = "two.sided") #up
#	Exact binomial test
#data:  147366 and 292301
#number of successes = 147370, number of trials = 292300, p-value = 6.969e-06
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.5023441 0.5059726
#sample estimates:
#  probability of success 
#0.5041584 

##############################################################################


############################################################################## eur. group by bar up or down, calculate range?
#Abs.Range #High-Low absolute value doesn't matter
#colnames(eur)[12] = "Range"

#Real.Range
eur = eur %>% mutate(Real.Range = ifelse(Bar == "Up", (High-Low), ifelse(Bar == "Down", (Low - High), 
                                                                           ifelse(High == Low, 0, NA)))) # Neutral NA, 0 values counted

eur = eur %>% mutate(Neutral.Range = ifelse(Bar == "Neutral", (High-Low), NA))

#range of Neutral bars?

summary(eur$Real.Range)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -0.02    0.00    0.00    0.00    0.00    0.02  240299 

hist(eur$Real.Range,xlab="Pips", main="Histogram of EURUSD Minute Bar Range", xlim=c(-0.002, 0.002), breaks = 2000)

ggplot(data=eur, aes(eur$Real.Range)) + 
  geom_histogram(breaks=seq(-0.0015, 0.0015, by = .00001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of EURUSD Minute Bar Real.Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(-0.0015, 0.0015)) #Histogram of EURUSD Minute Bar Real.Range


ggplot(data=eur, aes(eur$Neutral.Range)) + 
  geom_histogram(breaks=seq(0, 0.0005, by = .00001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of EURUSD Minute Bar Neutral Bar Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(0, 0.0005)) #Histogram of EURUSD Minute Bar Neutral.Range

#####mean, sd, etc
Up.Real.Range = eur[eur$Bar == "Up",]["Real.Range"]
Down.Real.Range = eur[eur$Bar == "Down",]["Real.Range"]

Hmisc::describe(Up.Real.Range) #library(Hmisc)
#1  Variables      2083026  Observations
#--------------------------------------------------------------------------------------------------------------------------
#  Real.Range 
#n   missing  distinct      Info      Mean       Gmd       .05       .10       .25       .50       .75       .90 
#2083026         0       838         1 0.0003052 0.0002648   0.00004   0.00006   0.00012   0.00023   0.00044   0.00064 
#.95 
#0.00078 
#lowest : 0.00001 0.00001 0.00002 0.00002 0.00003, highest: 0.01165 0.01167 0.01194 0.01243 0.01675

Hmisc::describe(abs(Down.Real.Range)) #library(Hmisc)
# 1  Variables      2078754  Observations
#--------------------------------------------------------------------------------------------------------------------------
#  Real.Range 
#n   missing  distinct      Info      Mean       Gmd       .05       .10       .25       .50       .75       .90 
#2078754         0       805         1 0.0003019 0.0002613   0.00004   0.00006   0.00011   0.00023   0.00043   0.00063 
#.95 
#0.00076 
#lowest : 0.00001 0.00001 0.00002 0.00002 0.00003, highest: 0.00916 0.00997 0.01257 0.01399 0.01597

############################################################################## 16 vars
############################################################################## jpy
#Real.Range
jpy = jpy %>% mutate(Real.Range = ifelse(Bar == "Up", (High-Low), ifelse(Bar == "Down", (Low - High), 
                                                                         ifelse(High == Low, 0, NA)))) # Neutral NA, 0 values counted

jpy = jpy %>% mutate(Neutral.Range = ifelse(Bar == "Neutral", (High-Low), NA))


summary(jpy$Real.Range)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-2.8     0.0     0.0     0.0     0.0     2.3  402143 

hist(jpy$Real.Range,xlab="Pips", main="Histogram of USDJPY Minute Bar Range", xlim=c(-0.1, 0.1), breaks = 2000)

ggplot(data=jpy, aes(jpy$Real.Range)) + 
  geom_histogram(breaks=seq(-0.1, 0.1, by = .001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of USDJPY Minute Bar Real.Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(-0.1, 0.1)) #Histogram of USDJPY Minute Bar Real.Range


ggplot(data=jpy, aes(jpy$Neutral.Range)) + 
  geom_histogram(breaks=seq(0, 0.05, by = .001), 
                 col="red", 
                 fill="blue", 
                 alpha = .2) + 
  labs(title="Histogram of USDJPY Minute Bar Neutral Bar Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(0, 0.05)) #Histogram of USDJPY Minute Bar Neutral.Range

#####mean, sd, etc
Up.Real.Range = jpy[jpy$Bar == "Up",]["Real.Range"]
Down.Real.Range = jpy[jpy$Bar == "Down",]["Real.Range"]

Hmisc::describe(Up.Real.Range) #library(Hmisc)
# 1  Variables      1928300  Observations
#--------------------------------------------------------------------------------------------------------------------------
#  Real.Range 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1928300        0      837    0.999  0.02096   0.0177    0.003    0.005    0.010    0.016    0.026    0.040    0.055 
#lowest : 0.001 0.001 0.002 0.002 0.003, highest: 1.425 1.505 1.626 1.827 2.252

Hmisc::describe(abs(Down.Real.Range))
# 1  Variables      1961730  Observations
#--------------------------------------------------------------------------------------------------------------------------
#  Real.Range 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1961730        0      866    0.999   0.0209  0.01786    0.003    0.005    0.009    0.015    0.026    0.040    0.055 
#lowest : 0.001 0.001 0.002 0.002 0.003, highest: 1.570 2.211 2.505 2.705 2.795

############################################################################## 16 vars
############################################################################## chf
#Real.Range
chf = chf %>% mutate(Real.Range = ifelse(Bar == "Up", (High-Low), ifelse(Bar == "Down", (Low - High), 
                                                                         ifelse(High == Low, 0, NA)))) # Neutral NA, 0 values counted

chf = chf %>% mutate(Neutral.Range = ifelse(Bar == "Neutral", (High-Low), NA))


summary(chf$Real.Range)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-2.8     0.0     0.0     0.0     0.0     2.3  402143 

hist(chf$Real.Range,xlab="Pips", main="Histogram of USDCHF Minute Bar Range", xlim=c(-0.001, 0.001), breaks = 2000)

ggplot(data=chf, aes(chf$Real.Range)) + 
  geom_histogram(breaks=seq(-0.001, 0.001, by = .00001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of USDCHF Minute Bar Real.Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(-0.001, 0.001)) #Histogram of USDCHF Minute Bar Real.Range


ggplot(data=chf, aes(chf$Neutral.Range)) + 
  geom_histogram(breaks=seq(0, 0.0005, by = .00001), 
                 col="red", 
                 fill="blue", 
                 alpha = .2) + 
  labs(title="Histogram of USDCHF Minute Bar Neutral Bar Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(0, 0.0005)) #Histogram of USDCHF Minute Bar Neutral.Range

#####mean, sd, etc
Up.Real.Range = chf[chf$Bar == "Up",]["Real.Range"]
Down.Real.Range = chf[chf$Bar == "Down",]["Real.Range"]

Hmisc::describe(Up.Real.Range) #library(Hmisc)
# 1  Variables      1920242  Observations
#--------------------------------------------------------------------------------------------------------------------------
#  Real.Range 
#n   missing  distinct      Info      Mean       Gmd       .05       .10       .25       .50       .75       .90 
#1920242         0      1146     0.999 0.0002096 0.0001867   0.00003   0.00004   0.00009   0.00015   0.00027   0.00043 
#.95 
#0.00055 
#lowest : 0.00001 0.00001 0.00001 0.00002 0.00002, highest: 0.03483 0.03522 0.03672 0.03803 0.04340

Hmisc::describe(abs(Down.Real.Range))
# 1  Variables      1928331  Observations
#--------------------------------------------------------------------------------------------------------------------------
#  Real.Range 
#n   missing  distinct      Info      Mean       Gmd       .05       .10       .25       .50       .75       .90 
#1928331         0      1132     0.999 0.0002108   0.00019   0.00003   0.00004   0.00008   0.00015   0.00027   0.00044 
#.95 
#0.00057 
#lowest : 0.00001 0.00001 0.00001 0.00002 0.00002, highest: 0.04362 0.04616 0.04740 0.07127 0.07885
#-------------------------------------------------------------------------------------------------------

############################################################################## 16 vars
############################################################################## xau
#Real.Range
xau = xau %>% mutate(Real.Range = ifelse(Bar == "Up", (High-Low), ifelse(Bar == "Down", (Low - High), 
                                                                         ifelse(High == Low, 0, NA)))) # Neutral NA, 0 values counted

xau = xau %>% mutate(Neutral.Range = ifelse(Bar == "Neutral", (High-Low), NA))


summary(xau$Real.Range)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-58.80   -0.30    0.00    0.00    0.30   26.79   77025

hist(xau$Real.Range,xlab="Pips", main="Histogram of XAUUSD Minute Bar Range", xlim=c(-1.5, 1.5), breaks = 2000)

ggplot(data=xau, aes(xau$Real.Range)) + 
  geom_histogram(breaks=seq(-1.5, 1.5, by = .01), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of XAUUSD Minute Bar Real.Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(-1.5, 1.5)) #Histogram of XAUUSD Minute Bar Real.Range


ggplot(data=xau, aes(xau$Neutral.Range)) + 
  geom_histogram(breaks=seq(0, .5, by = .01), 
                 col="red", 
                 fill="blue", 
                 alpha = .2) + 
  labs(title="Histogram of XAUUSD Minute Bar Neutral Bar Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(0, .5)) #Histogram of XAUUSD Minute Bar Neutral.Range

#####mean, sd, etc
Up.Real.Range = xau[xau$Bar == "Up",]["Real.Range"]
Down.Real.Range = xau[xau$Bar == "Down",]["Real.Range"]

Hmisc::describe(Up.Real.Range) #library(Hmisc)
# 1  Variables      2058924  Observations
#--------------------------------------------------------------------------------------------------------------------------
#  Real.Range 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#2058924        0     9961        1   0.4004   0.3276    0.080    0.108    0.186    0.303    0.508    0.789    1.005 
#lowest :  0.001  0.001  0.001  0.002  0.002, highest: 16.303 16.759 19.205 20.572 26.790

Hmisc::describe(abs(Down.Real.Range))
# 1  Variables      2080718  Observations
#--------------------------------------------------------------------------------------------------------------------------
#  Real.Range 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#2080718        0    10170        1   0.3937   0.3249    0.075    0.102    0.181    0.300    0.500    0.775    0.994 
#lowest :  0.001  0.001  0.001  0.002  0.002, highest: 21.936 23.231 24.330 31.516 58.801

############################################################################## 16 vars
#save 16 vars
write.csv(eur, "eur16var.csv", row.names = FALSE)
write.csv(jpy, "jpy16var.csv", row.names = FALSE)
write.csv(chf, "chf16var.csv", row.names = FALSE)
write.csv(xau, "xau16var.csv", row.names = FALSE)


#new file below
##############################################################################
#Support, resistance made into hist bins


##############################################################################


##############################################################################


#stats, correlation by year
eurusd2007 = eurusd[year(eurusd$Date) == 2007,]  #for year 2007


################################################################################### 
#if were to predict, range and direction

################################################################################### TO DO 
#do by hour V
#stats by year

#install performanceAnalytics
#tidyquant

#Support, resistance made into hist bins?
#candlestick formation 
#tech analysis, wilder stuff




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
