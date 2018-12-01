
library(readr)
library(anytime)
library(tidyquant)
library(dplyr)

library(Hmisc)
library(psych)
setwd("D:/NYCDSA/Project 4 - Capstone Project")

#read csv
eur= read_csv("eurdate.csv")
jpy= read_csv("jpydate.csv")
chf= read_csv("chfdate.csv")
xau= read_csv("xaudate.csv")

eur = eur[-1]
jpy = jpy[-1]
chf = chf[-1]
xau = xau[-1]

#need symbol for groupby 
jpy$Symbol = "USDJPY"
chf$Symbol = "USDCHF"
xau$Symbol = "XAUUSD"
eur$Symbol = "EURUSD"

#up down bars
eur = eur %>% mutate(Up = ifelse(Close > Open, TRUE, NA)) %>% #up bars
  mutate(Down = ifelse(Open > Close, TRUE, NA)) %>% #down bars
  mutate(Neutral = ifelse(Open == Close, TRUE, NA))
jpy = jpy %>% mutate(Up = ifelse(Close > Open, TRUE, NA)) %>% #up bars
  mutate(Down = ifelse(Open > Close, TRUE, NA)) %>% #down bars
  mutate(Neutral = ifelse(Open == Close, TRUE, NA))
chf = chf %>% mutate(Up = ifelse(Close > Open, TRUE, NA)) %>% #up bars
  mutate(Down = ifelse(Open > Close, TRUE, NA)) %>% #down bars
  mutate(Neutral = ifelse(Open == Close, TRUE, NA))
xau = xau %>% mutate(Up = ifelse(Close > Open, TRUE, NA)) %>% #up bars
  mutate(Down = ifelse(Open > Close, TRUE, NA)) %>% #down bars
  mutate(Neutral = ifelse(Open == Close, TRUE, NA))

#bar column
eur = eur %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down")))
jpy = jpy %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down")))
chf = chf %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down")))
xau = xau %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down")))
#11 variables


totalminutes <- rbind(jpy, chf, xau, eur)
totalminutes %>% group_by(Symbol, Bar) %>% summarise(Upcount = n()) #daily up/down by currency
#Symbol Bar     Upcount
#<chr>  <chr>     <int>
#1 EURUSD Down    2078754
#2 EURUSD Neutral  286234
#3 EURUSD Up      2083026
#4 USDCHF Down    1928331
#5 USDCHF Neutral  593450
#6 USDCHF Up      1920242
#7 USDJPY Down    1961730
#8 USDJPY Neutral  520863
#9 USDJPY Up      1928300
#10 XAUUSD Down    2080718
#11 XAUUSD Neutral   95577
#12 XAUUSD Up      2058924

############################################################################## statistical analysis of up/down bars

###################################################################################eur
#1 EURUSD Down    2078754
#2 EURUSD Neutral  286234
#3 EURUSD Up      2083026

#excluding neutral bars, total bars: 
2083026+2078754 #= 4161780
#% of Up Bars: 
2083026 / 4161780 #= 0.5005132

#neutral count
286234/ (286234+2083026+2078754) #0.06435097

#dbinom
dbinom(2083026, size=4161780, prob=0.5) #up bar 4.365959e-05
dbinom(2078754, size=4161780, prob=0.5) #down bar 4.365959e-05

#qbinom
n<- 4161780
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#2078891 2082889

binom.test(2083026, 4161780, 1/2, alternative = "two.sided") #up
#Exact binomial test
#data:  2083026 and 4161780
#number of successes = 2083000, number of trials = 4161800, p-value = 0.0363
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.5000327 0.5009937
#sample estimates:
#  probability of success 
#0.5005132 
##############################################################################

###################################################################################jpy
#7 USDJPY Down    1961730
#8 USDJPY Neutral  520863
#9 USDJPY Up      1928300

#excluding neutral bars, total bars: 
1928300+1961730 #= 3890030
#% of Up Bars: 
1928300 / 3890030 #= 0.4957031

#neutral count
520863/ (520863+1928300+1961730) #0.1180856

#dbinom
dbinom(1928300, size=3890030, prob=0.5) #up bar 1.667563e-66
dbinom(1961730, size=3890030, prob=0.5) #down bar 1.667563e-66

#qbinom
n<- 3890030
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#1943082 1946948

binom.test(1928300, 3890030, 1/2, alternative = "two.sided") #up
#Exact binomial test
#data:  1928300 and 3890030
#number of successes = 1928300, number of trials = 3890000, p-value < 2.2e-16
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.4952061 0.4962001
#sample estimates:
#  probability of success 
#0.4957031 
##############################################################################

###################################################################################chf
#4 USDCHF Down    1928331
#5 USDCHF Neutral  593450
#6 USDCHF Up      1920242

#excluding neutral bars, total bars: 
1920242+1928331 #= 3848573
#% of Up Bars: 
1920242 / 3848573 #= 0.4989491

#neutral count
593450/ (593450+1920242+1928331) #0.133599

#dbinom
dbinom(1920242, size=3848573, prob=0.5) #up bar 8.268688e-08
dbinom(1928331, size=3848573, prob=0.5) #down bar 8.268688e-08

#qbinom
n<- 3848573
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#1922364 1926209

binom.test(1920242, 3848573, 1/2, alternative = "two.sided") #up
#Exact binomial test
#data:  1920242 and 3848573
#number of successes = 1920200, number of trials = 3848600, p-value = 3.743e-05
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.4984494 0.4994488
#sample estimates:
#  probability of success 
#0.4989491 
##############################################################################

###################################################################################xau 
#10 XAUUSD Down    2080718
#11 XAUUSD Neutral   95577
#12 XAUUSD Up      2058924

#excluding neutral bars, total bars: 
2058924+2080718 #= 4139642
#% of Up Bars: 
2058924 / 4139642 #= 0.4973676

#neutral count
95577/ (95577+2058924+2080718 ) #0.02256719

#dbinom
dbinom(2058924, size=4139642, prob=0.5) #up bar 4.765292e-29
dbinom(2080718, size=4139642, prob=0.5) #down bar 4.765292e-29

#qbinom
n<- 4139642
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#2067827 2071815

binom.test(2058924, 4139642, 1/2, alternative = "two.sided") #up
#Exact binomial test
#data:  2058924 and 4139642
#number of successes = 2058900, number of trials = 4139600, p-value < 2.2e-16
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.4968859 0.4978494
#sample estimates:
#  probability of success 
#0.4973676 
##############################################################################

############################################################################## all?
totalminutes %>% group_by(Bar) %>% summarise(Upcount = n()) #daily up/down by currency
#Bar     Upcount
#<chr>     <int>
#1 Down    8049533
#2 Neutral 1496124
#3 Up      7990492

#excluding neutral bars, total bars: 
7990492+8049533 #= 16040025
#% of Up Bars: 
7990492 / 16040025 #= 0.4981596

#neutral count
1496124/ (1496124+7990492+8049533) #0.08531657

#dbinom
dbinom(7990492, size=16040025, prob=0.5) #up bar 1.283823e-51
dbinom(8049533, size=16040025, prob=0.5) #down bar 1.283823e-51

#qbinom
n<- 16040025
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
#8016088 8023937

binom.test(7990492, 16040025, 1/2, alternative = "two.sided") #up
#Exact binomial test
#data:  7990492 and 16040025
#number of successes = 7990500, number of trials = 16040000, p-value < 2.2e-16
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.4979149 0.4984043
#sample estimates:
#  probability of success 
#0.4981596 

######################################################################################################################################################################

######################################################################################################################################################################

######################################################################################################################################################################

############################################################################## bar range 
eur = eur %>% mutate(Range = abs(High - Low)) #eur$Range= abs(eur$High-eur$Low)
jpy = jpy %>% mutate(Range = abs(High - Low))
chf = chf %>% mutate(Range = abs(High - Low))
xau = xau %>% mutate(Range = abs(High - Low))

totalminutes %>% group_by(Symbol) %>% summarise(Upcount = n()) #total bars
#Symbol Upcount
#<chr>    <int>
#1 EURUSD 4448014
#2 USDCHF 4442023
#3 USDJPY 4410893
#4 XAUUSD 4235219

#high - low = 0? the bar did not move at all?
eur[eur$High == eur$Low,] # out of neutral count 286234, 45,935 bars did not move at all
# A tibble: 45,935 x 12
#Date       Timestamp  Open  High   Low Close Symbol Up    Down  Neutral Bar     Range
#<date>     <time>    <dbl> <dbl> <dbl> <dbl> <chr>  <lgl> <lgl> <lgl>   <chr>   <dbl>
#1 2007-03-25 22:39      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
#2 2007-03-26 05:39      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
#3 2007-03-26 06:42      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
#4 2007-03-26 06:47      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
#5 2007-03-26 07:46      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
#6 2007-03-26 09:35      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
#7 2007-03-26 10:48      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
#8 2007-03-26 10:56      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
#9 2007-03-26 12:05      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
#0 2007-03-26 12:41      1.33  1.33  1.33  1.33 EURUSD NA    NA    TRUE    Neutral     0
# ... with 45,925 more rows
###################################################################################eur
summary(eur$Range)
#Min.     1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000000 0.0001000 0.0002100 0.0002912 0.0004200 0.0167500 

hist(eur$Range,xlab="Pips", main="Histogram of EURUSD Minute Bar Range", breaks = 200) #
#hist(eur$Range,xlab="Pips", main="Histogram of EURUSD Bar Range", xlim=c(0.005, 0.017), ylim=c(0, 20), breaks = 200)
#hist(eur$Range,xlab="Pips", main="Histogram of EURUSD Bar Range", xlim=c(0.0001, 0.005), ylim=c(0, 500), breaks = 200)
hist(eur$Range,xlab="Pips", main="Histogram of EURUSD Minute Bar Range", xlim=c(0.00, 0.0015), breaks = 2000)

max(eur$Range) #0.01675, 167.5 pips

#####mean, sd, etc
Hmisc::describe(eur$Range) #library(Hmisc)
#highest: 0.01243 0.01257 0.01399 0.01597 0.01675

psych::describe(eur$Range) #library(psych)
#   vars       n mean sd median trimmed mad min  max range skew kurtosis se
#X1    1 4448014    0  0      0       0   0   0 0.02  0.02 2.71     35.7  0

sd(eur$Range) #0.0002584813

hist(eur$Range, freq=FALSE, main="Density plot", breaks = 200) #density plot

histinfo<-hist(eur$Range, breaks = 200)
histinfo
#$`breaks`
#[1] 0.0000 0.0001 0.0002 0.0003 0.0004 0.0005 0.0006 0.0007 0.0008 0.0009 0.0010 0.0011 0.0012 0.0013 0.0014 0.0015 0.0016 0.0017 0.0018 0.0019 0.0020
#$counts
#[1] 1120461 1024706  667928  466561  384817  296875  192746  115666   67556   39595   23856   14807    9474    6261    4312    2916    2079    1559


###################################################################################jpy
summary(jpy$Range)
#Min.  1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0080  0.0150  0.0195  0.0250  2.7950 

hist(jpy$Range,xlab="Pips", main="Histogram of USDJPY Minute Bar Range", breaks = 200) #
#hist(jpy$Range,xlab="Pips", main="Histogram of USDJPY Bar Range", breaks = 200, xlim=c(0.05, 0.5), ylim=c(0.0, 5000))
hist(jpy$Range,xlab="Pips", main="Histogram of USDJPY Minute Bar Range", xlim=c(0, 0.08), breaks = 2000)
hist(jpy$Range,xlab="Pips", main="Histogram of USDJPY Minute Bar Range", xlim=c(0, 0.03), breaks = 2000)
jpy[jpy$High == jpy$Low,]      # A tibble: 118,720 x 12
jpy[jpy$High == jpy$Low+0.001,]# A tibble: 51,932 x 12
jpy[jpy$High == jpy$Low+0.002,]# A tibble: 68,464 x 12
jpy[jpy$High == jpy$Low+0.003,]# A tibble: 106,919 x 12
jpy[jpy$High == jpy$Low+0.004,]# A tibble: 73,895 x 12
jpy[jpy$High == jpy$Low+0.005,]# A tibble: 152,348 x 12
jpy[jpy$High == jpy$Low+0.006,]# A tibble: 129,940 x 12
jpy[jpy$High == jpy$Low+0.007,]# A tibble: 88,545 x 12
jpy[jpy$High == jpy$Low+0.008,]# A tibble: 94,074 x 12
jpy[jpy$High == jpy$Low+0.009,]# A tibble: 130,871 x 12
jpy[jpy$High == jpy$Low+0.010,]# A tibble: 227,280 x 12
jpy[jpy$High == jpy$Low+0.011,]# A tibble: 90,882 x 12

###ggplot
# Don't map a variable to y
ggplot(jpy, aes(x=factor(Range)))+
  geom_bar(stat="count", fill="steelblue")+
#  xlim(0, 0.1)+
#  coord_cartesian(xlim = c(0, 2))+
  theme_minimal()

ggplot(data=jpy, aes(jpy$Range)) + 
  geom_histogram(breaks=seq(0, .08, by = .001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of USDJPY Minute Bar Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(0,0.08)) #saved as ggplot2_USDJPYminuteRange


max(jpy$Range) #2.795 , 279.5 pips

#####mean, sd, etc
Hmisc::describe(jpy$Range) #library(Hmisc)
# highest: 2.211 2.252 2.505 2.705 2.795

psych::describe(jpy$Range) #library(psych)
#vars       n mean   sd median trimmed  mad min max range skew kurtosis se
#X1    1 4410893 0.02 0.02   0.02    0.02 0.01   0 2.8   2.8 8.96   467.65  0

sd(jpy$Range) #0.02005472

hist(jpy$Range, freq=FALSE, main="Density plot", breaks = 200) #density plot

histinfo<-hist(jpy$Range, breaks = 200)
histinfo
#$`breaks`
#[1] 0.00 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 0.15 0.16 0.17 0.18 0.19 0.20 0.21 0.22 0.23 0.24 0.25 0.26 0.27 0.28
#$counts
#[1] 1635899 1359641  692919  331709  166394   87380   48827   29083   17978   11427    7590    5314    3742    2690    1979    1487    1115     923


###################################################################################chf
summary(chf$Range)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000000 0.0000700 0.0001400 0.0001922 0.0002500 0.0788500 

hist(chf$Range,xlab="Pips", main="Histogram of USDCHF Bar Range", breaks = 200) #
hist(chf$Range,xlab="Pips", main="Histogram of USDCHF Bar Range", xlim=c(0.0, 0.001), breaks = 4000)

ggplot(data=chf, aes(chf$Range)) + 
  geom_histogram(breaks=seq(0, .001, by = .00001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of USDCHF Minute Bar Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(0,0.001)) #saved as ggplot2_USDCHFminuteRange


max(chf$Range) #0.07885 , 755.5 pips

#####mean, sd, etc
Hmisc::describe(chf$Range) #library(Hmisc)
#highest: 0.04362 0.04616 0.04740 0.07127 0.07885

psych::describe(chf$Range) #library(psych)
#vars       n mean sd median trimmed mad min  max range  skew kurtosis se
#X1    1 4442023    0  0      0       0   0   0 0.08  0.08 38.36  8663.11  0

sd(chf$Range) #0.0002241274

hist(chf$Range, freq=FALSE, main="Density plot", breaks = 200) #density plot

histinfo<-hist(chf$Range, breaks = 200)
histinfo
#$`breaks`
#[1] 0.0000 0.0005 0.0010 0.0015 0.0020 0.0025 0.0030 0.0035 0.0040 0.0045 0.0050 0.0055 0.0060 0.0065 0.0070 0.0075 0.0080 0.0085 0.0090 0.0095 0.0100
#$counts
#[1] 4192082  217489   23775    5213    1639     676     402     254     126      95      83      55      29      20      10       5       8       5


###################################################################################xau
summary(xau$Range)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.1780  0.3000  0.3909  0.5000 58.8010 

hist(xau$Range,xlab="Pips", main="Histogram of XAUUSD Bar Range", breaks = 2000) #
hist(xau$Range,xlab="Pips", main="Histogram of XAUUSD Bar Range", xlim=c(0, 1.5), breaks = 6000)

ggplot(data=xau, aes(xau$Range)) + 
  geom_histogram(breaks=seq(0, .2, by = .001), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of XAUUSD Minute Bar Range") +
  labs(x="Pips", y="Count") + 
  xlim(c(0,.2)) #saved as ggplot2_XAUUSDminuteRange

max(xau$Range) #58.801, 5880.1 pips

#####mean, sd, etc
Hmisc::describe(xau$Range) #library(Hmisc)
# highest: 23.231 24.330 26.790 31.516 58.801

psych::describe(xau$Range) #library(psych)
#   vars       n mean   sd median trimmed  mad min  max range skew kurtosis se
#X1    1 4235219 0.39 0.36    0.3    0.33 0.22   0 58.8  58.8 6.22   262.47  0

sd(xau$Range) #0.3635563

hist(xau$Range, freq=FALSE, main="Density plot", breaks = 200) #density plot

histinfo<-hist(xau$Range, breaks = 200)
histinfo
#$`breaks`
#[1]  0.0  0.5  1.0  1.5  2.0  2.5  3.0  3.5  4.0  4.5  5.0  5.5  6.0  6.5  7.0  7.5  8.0  8.5  9.0  9.5 10.0 10.5 11.0 11.5 12.0 12.5 13.0 13.5 14.0
#$counts
#[1] 3190109  839339  143717   36372   13078    5741    2777    1508     925     537     316     221     129     118      73      45      43      34

###################################################################################
###################################################################################
###################################################################################
####consecutive bar count #rle

for (i in 1:5){
  print (i)
} 

eur2 = eur
eur2$UpCount = sequence(rle(as.character(eur$Up))$lengths) #https://stackoverflow.com/questions/19998836/r-count-consecutive-occurrences-of-values-in-a-single-column
#find NA's
eur2[which(is.na(eur2$Up)),]["UpCount"] = NA

eur2$DownCount = sequence(rle(as.character(eur$Down))$lengths) 
eur2[which(is.na(eur2$Down)),]["DownCount"] = NA

###
which(is.na(eur2$Up))#returns row index
is.na(eur2$Up) #true/ false
###

#http://masterr.org/r/how-to-find-consecutive-repeats-in-r/
eurupcount = rle(eur2$Up)
table(eurupcount)
#values
#lengths   TRUE
#1  596915
#2  277696
#3  130518
#4   60536
#5   27846
#6   12606
#7    5776
#8    2566
#9    1167
#10    512
#11    269
#12    105
#13     55
#14     28
#15      6
#16      4
#17      1
#18      2
#19      1
#20      1

eurupcount = rle(eur2$Up)

rle(eur2$Up)
#Run Length Encoding
#lengths: int [1:3481598] 3 1 3 1 1 1 1 1 4 1 ...
#values : logi [1:3481598] TRUE NA TRUE NA NA NA ...

which(eurupcount$values == TRUE) #[1] 1 3 9 15
runs = which(eurupcount$values == TRUE) #index of values that are TRUE
eurupcount$lengths[runs] #the runs of all TRUE Ups, 3  3  4

eurupcount$lengths #[1] 3 1 3 1 1 1 1 1, the 'run' of each run
cumsum(eurupcount$lengths)#[1] 3 4 7 8 9 10 11, the ends of all runs (NA values are 1 run each)
cumsum(eurupcount$lengths)[runs] #returns ends of TRUE values, [1] 3 7 16 22 24


################################################ clean, use the following
eurupcount = rle(eur2$Up)
runs = which(eurupcount$values == TRUE)
cumsum(eurupcount$lengths)[runs] #returns ends of TRUE values
eur2[-cumsum(eurupcount$lengths)[runs],]["UpCount"] = NA #rows that are not the end of a run
eur2$UpCount = eur2[cumsum(eurupcount$lengths)[runs],]["UpCount"] 

###################################################################################
###################################################################################
###################################################################################

################################################################################### eur Consecutive Bar Count
eur$UpCount = sequence(rle(as.character(eur$Up))$lengths) #https://stackoverflow.com/questions/19998836/r-count-consecutive-occurrences-of-values-in-a-single-column
eur[which(is.na(eur$Up)),]["UpCount"] = NA

eurupcount = rle(eur$Up)
runs = which(eurupcount$values == TRUE)
eur[-cumsum(eurupcount$lengths)[runs],]["UpCount"] = NA #rows that are not the end of a run

##--##

eur$DownCount = sequence(rle(as.character(eur$Down))$lengths) 
eur[which(is.na(eur$Down)),]["DownCount"] = NA

eurdowncount = rle(eur$Down)
runs = which(eurdowncount$values == TRUE)
eur[-cumsum(eurdowncount$lengths)[runs],]["DownCount"] = NA #rows that are not the end of a run

##--##

#descriptive stats on runs
summary(eur$UpCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      1       1       1       2       2      20 3331404

summary(eur$DownCount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      1       1       1       2       2      21 3330165 

hist(eur$UpCount,xlab="Runs", main="Histogram of EURUSD Bull Runs", breaks = 20) #

ggplot(data=eur, aes(eur$UpCount)) + 
  geom_histogram(breaks=seq(0, 20, by = 1), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram of EURUSD Bull Runs") +
  labs(x="Runs", y="Count") + 
  xlim(c(0,20)) #


#####mean, sd, etc
Hmisc::describe(eur$UpCount) #library(Hmisc)
#eur2$UpCount 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1116610  3331404       20     0.83    1.865    1.179        1        1        1        1        2        3        4 

#Value           1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20
#Frequency  596915 277696 130518  60536  27846  12606   5776   2566   1167    512    269    105     55     28      6      4      1      2      1      1
#Proportion  0.535  0.249  0.117  0.054  0.025  0.011  0.005  0.002  0.001  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000


Hmisc::describe(eur$DownCount)
#eur$DownCount 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1117849  3330165       20    0.828     1.86    1.173        1        1        1        1        2        3        4 

#Value           1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     21
#Frequency  600103 277525 129608  59872  27742  12759   5636   2528   1163    521    216     92     43     18      9      6      2      2      2      2
#Proportion  0.537  0.248  0.116  0.054  0.025  0.011  0.005  0.002  0.001  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000

dbinom(1:20, size= 1:20,0.5)
#[1] 5.000000e-01 2.500000e-01 1.250000e-01 6.250000e-02 3.125000e-02 1.562500e-02 7.812500e-03 3.906250e-03 1.953125e-03
#[10] 9.765625e-04 4.882812e-04 2.441406e-04 1.220703e-04 6.103516e-05 3.051758e-05 1.525879e-05 7.629395e-06 3.814697e-06
#[19] 1.907349e-06 9.536743e-07

write.csv(eur, "eur14var.csv", row.names = FALSE)
eur14var= read_csv("eur14var.csv")
################################################################################### 

################################################################################### jpy Consecutive Bar Count
jpy$UpCount = sequence(rle(as.character(jpy$Up))$lengths)
jpy[which(is.na(jpy$Up)),]["UpCount"] = NA

jpyupcount = rle(jpy$Up)
runs = which(jpyupcount$values == TRUE)
jpy[-cumsum(jpyupcount$lengths)[runs],]["UpCount"] = NA #rows that are not the end of a run

##--##

jpy$DownCount = sequence(rle(as.character(jpy$Down))$lengths) 
jpy[which(is.na(jpy$Down)),]["DownCount"] = NA

jpydowncount = rle(jpy$Down)
runs = which(jpydowncount$values == TRUE)
jpy[-cumsum(jpydowncount$lengths)[runs],]["DownCount"] = NA #rows that are not the end of a run

##--##

#descriptive stats on runs
summary(jpy$UpCount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      1       1       1       2       2      16 3318184 

summary(jpy$DownCount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      1       1       1       2       2      17 3312687 

#####mean, sd, etc
Hmisc::describe(jpy$UpCount) #library(Hmisc)
#jpy$UpCount 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1092709  3318184       16    0.802    1.765    1.066        1        1        1        1        2        3        4 

#Value           1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16
#Frequency  618705 267936 116922  50750  22246   9294   3951   1735    640    327    117     46     19     10      6      5
#Proportion  0.566  0.245  0.107  0.046  0.020  0.009  0.004  0.002  0.001  0.000  0.000  0.000  0.000  0.000  0.000  0.000

Hmisc::describe(jpy$DownCount)
#jpy$DownCount 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1098206  3312687       17    0.809    1.786     1.09        1        1        1        1        2        3        4 

#Value           1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17
#Frequency  613837 270709 120033  52761  23263   9938   4352   1908    823    336    141     59     24     11      8      2      1
#Proportion  0.559  0.247  0.109  0.048  0.021  0.009  0.004  0.002  0.001  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000


dbinom(1:20, size= 1:20,0.5)
#[1] 5.000000e-01 2.500000e-01 1.250000e-01 6.250000e-02 3.125000e-02 1.562500e-02 7.812500e-03 3.906250e-03 1.953125e-03
#[10] 9.765625e-04 4.882812e-04 2.441406e-04 1.220703e-04 6.103516e-05 3.051758e-05 1.525879e-05 7.629395e-06 3.814697e-06
#[19] 1.907349e-06 9.536743e-07

write.csv(jpy, "jpy14var.csv", row.names = FALSE)
################################################################################### 

################################################################################### chf Consecutive Bar Count
chf$UpCount = sequence(rle(as.character(chf$Up))$lengths)
chf[which(is.na(chf$Up)),]["UpCount"] = NA

chfupcount = rle(chf$Up)
runs = which(chfupcount$values == TRUE)
chf[-cumsum(chfupcount$lengths)[runs],]["UpCount"] = NA #rows that are not the end of a run

##--##

chf$DownCount = sequence(rle(as.character(chf$Down))$lengths) 
chf[which(is.na(chf$Down)),]["DownCount"] = NA

chfdowncount = rle(chf$Down)
runs = which(chfdowncount$values == TRUE)
chf[-cumsum(chfdowncount$lengths)[runs],]["DownCount"] = NA #rows that are not the end of a run

##--##

#descriptive stats on runs
summary(chf$UpCount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      1       1       1       2       2      17 3350446 

summary(chf$DownCount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      1       1       1       2       2      21 3348091 

#####mean, sd, etc
Hmisc::describe(chf$UpCount) #library(Hmisc)
#chf$UpCount 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1091577  3350446       17    0.798    1.759    1.064        1        1        1        1        2        3        4 

#Value           1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17
#Frequency  622967 265608 114377  50013  21841   9458   4095   1832    775    328    159     66     28     16      8      4      2
#Proportion  0.571  0.243  0.105  0.046  0.020  0.009  0.004  0.002  0.001  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000

Hmisc::describe(chf$DownCount)
#chf$DownCount 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1093932  3348091       20    0.799    1.763    1.069        1        1        1        1        2        3        4 

#Value           1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     21
#Frequency  623308 265520 115664  50364  22074   9563   4215   1812    792    371    139     60     27     14      3      2      1      1      1      1
#Proportion  0.570  0.243  0.106  0.046  0.020  0.009  0.004  0.002  0.001  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000


dbinom(1:20, size= 1:20,0.5)
#[1] 5.000000e-01 2.500000e-01 1.250000e-01 6.250000e-02 3.125000e-02 1.562500e-02 7.812500e-03 3.906250e-03 1.953125e-03
#[10] 9.765625e-04 4.882812e-04 2.441406e-04 1.220703e-04 6.103516e-05 3.051758e-05 1.525879e-05 7.629395e-06 3.814697e-06
#[19] 1.907349e-06 9.536743e-07

write.csv(chf, "chf14var.csv", row.names = FALSE)
################################################################################### 

################################################################################### xau Consecutive Bar Count
xau$UpCount = sequence(rle(as.character(xau$Up))$lengths)
xau[which(is.na(xau$Up)),]["UpCount"] = NA

xauupcount = rle(xau$Up)
runs = which(xauupcount$values == TRUE)
xau[-cumsum(xauupcount$lengths)[runs],]["UpCount"] = NA #rows that are not the end of a run

##--##

xau$DownCount = sequence(rle(as.character(xau$Down))$lengths) 
xau[which(is.na(xau$Down)),]["DownCount"] = NA

xaudowncount = rle(xau$Down)
runs = which(xaudowncount$values == TRUE)
xau[-cumsum(xaudowncount$lengths)[runs],]["DownCount"] = NA #rows that are not the end of a run

##--##

#descriptive stats on runs
summary(xau$UpCount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.0     1.0     1.0     1.9     2.0    48.0 3157718 

summary(xau$DownCount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.0     1.0     1.0     1.9     2.0    39.0 3157342 

#####mean, sd, etc
Hmisc::describe(xau$UpCount) #library(Hmisc)
#xau$UpCount 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1077501  3157718       28    0.839    1.911    1.234        1        1        1        1        2        4        5 

#lowest :  1  2  3  4  5, highest: 27 31 41 47 48

Hmisc::describe(xau$DownCount)
#xau$DownCount 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
#1077877  3157342       28    0.843     1.93    1.257        1        1        1        1        2        4        5 

#lowest :  1  2  3  4  5, highest: 24 25 27 31 39

xauupcount = rle(xau$Up)
table(xauupcount)
#values
#lengths   TRUE
#1  564035
#2  268681
#3  128070
#4   61107
#5   29176
#6   13827
#7    6521
#8    3191
#9    1500
#10    726
#11    352
#12    160
#13     80
#14     29
#15     18
#16      9
#17      7
#18      2
#19      1
#21      1
#22      1
#24      1
#25      1
#27      1
#31      1
#41      1
#47      1
#48      1

xaudowncount = rle(xau$Down)
table(xaudowncount)
#values
#lengths   TRUE
#1  559072
#2  268661
#3  129253
#4   62658
#5   30245
#6   14457
#7    6975
#8    3281
#9    1654
#10    803
#11    396
#12    216
#13     90
#14     47
#15     24
#16     13
#17     14
#18      8
#19      1
#20      1
#21      1
#22      1
#23      1
#24      1
#25      1
#27      1
#31      1
#39      1

write.csv(xau, "xau14var.csv", row.names = FALSE)
################################################################################### 

#do by hour 
#stats by year

#install performanceAnalytics
#save new datasets
#remove outliers, outliers wreck accounts

#bankers like round numbers, ggplot2_XAUUSDminuteRange shows that it is not a normal curvature? 