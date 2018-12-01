
library(readr)
library(anytime)
library(tidyquant)
setwd("D:/NYCDSA/Project 4 - Capstone Project")

#repeat for JPY, CHF, and Gold
#eur= read_csv("EURUSD1.csv")
#jpy= read_csv("USDJPY1.csv")
#chf= read_csv("USDCHF1.csv")
#xau= read_csv("XAUUSD1.csv")
eur= read_csv("eurdate.csv")
jpy= read_csv("jpydate.csv")
chf= read_csv("chfdate.csv")
xau= read_csv("xaudate.csv")

#format date column 
#eur$Date <- anydate(eur$Date)
#jpy$Date <- anydate(jpy$Date)
#chf$Date <- anydate(chf$Date)
#xau$Date <- anydate(xau$Date)
#write.csv(eur, "eurdate.csv")
#write.csv(jpy, "jpydate.csv")
#write.csv(chf, "chfdate.csv")
#write.csv(xau, "xaudate.csv")

#turn minute data to daily
eurusd = eur %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
usdjpy = jpy %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
usdchf = chf %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))
xauusd = xau %>% group_by(Date) %>% summarise(Open = first(Open), High = max(High), 
                                              Low = min(Low), Close = last(Close))

#get one year worth of daily
#eurusd2007 = eurusd[year(eurusd$Date) == 2007,]
#usdjpy2007 = usdjpy[year(usdjpy$Date) == 2007,]
#usdchf2007 = usdchf[year(usdchf$Date) == 2007,]
#xauusd2007 = xauusd[year(xauusd$Date) == 2007,]

#need symbol for groupby 
usdjpy$Symbol = "USDJPY"
usdchf$Symbol = "USDCHF"
xauusd$Symbol = "XAUUSD"
eurusd$Symbol = "EURUSD"

#append all four instruments
total <- rbind(usdjpy, usdchf, xauusd, eurusd) 
#write.csv(total, "allcurrenciestotaldaily.csv")

#run debug in an_exploration_of_FX (geom_chart raw code)
total %>% #
  ggplot(aes(x = Date, y = Close, group = Symbol)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
  labs(title = "Four Currencies Candlestick Chart", 
       subtitle = "Experimenting with Multiple Instruments",
       y = "Price", x = "") + 
  #coord_x_date(xlim = c(start, end)) +
  facet_wrap(~Symbol, ncol = 2, scale = "free_y") + 
  theme_tq()

#next, visualizing trends. or start with tinyquant documentation
##### up down bars
library(dplyr)
eurusd2007 = eurusd[year(eurusd$Date) == 2007,] #for eurusd daily year 2007

eurusd2007 = eurusd2007 %>% mutate(Up = ifelse(Close > Open, TRUE, NA)) %>% #up bars
  mutate(Down = ifelse(Open > Close, TRUE, NA)) %>% #down bars
  mutate(Neutral = ifelse(Open == Close, TRUE, NA)) #neutral bars

summary(eurusd2007)
#Up            Down         Neutral       
#Mode:logical   Mode:logical   Mode:logical  
#TRUE:170       TRUE:142       TRUE:1        
#NA's:143       NA's:171       NA's:312      
#table(eurusd2007$Up)

#####for entire data daily
total <- rbind(usdjpy, usdchf, xauusd, eurusd) 
total = total %>% mutate(Up = ifelse(Close > Open, TRUE, NA)) %>% #up bars
  mutate(Down = ifelse(Open > Close, TRUE, NA)) %>% #down bars
  mutate(Neutral = ifelse(Open == Close, TRUE, NA)) #neutral bars

summary(total)
#Symbol             Up            Down         Neutral       
#Length:14855       Mode:logical   Mode:logical   Mode:logical  
#Class :character   TRUE:7636      TRUE:7198      TRUE:21       
#Mode  :character   NA's:7219      NA's:7657      NA's:14834   
#total %>% group_by(Symbol) %>% summarise(Upcount = n())
#table(total)
#data.frame(table(total["Up"]))
#data.frame(table(total["Down"]))
#data.frame(table(total["Neutral"]))
#na.omit(total, cols = "Up") deletes whole row
#class(total)
# filter(!is.na(clade)), mean(genome_size, na.rm = TRUE)

##### Other way
total <- rbind(usdjpy, usdchf, xauusd, eurusd) 

total = total %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down"))) #Bar column

total %>% group_by(Symbol, Bar) %>% summarise(Upcount = n()) #daily up/down by currency
# Groups:   Symbol [?]
#Symbol Bar     Upcount
#<chr>  <chr>     <int>
# 1 EURUSD Down       1830
#2 EURUSD Neutral       3
#3 EURUSD Up         1886
#4 USDCHF Down       1786
#5 USDCHF Neutral      10
#6 USDCHF Up         1923
#7 USDJPY Down       1817
#8 USDJPY Neutral       7
#9 USDJPY Up         1887
#10 XAUUSD Down       1765
#11 XAUUSD Neutral       1
#12 XAUUSD Up         1940


#####for minutes data

#need symbol for groupby 
jpy$Symbol = "USDJPY"
chf$Symbol = "USDCHF"
xau$Symbol = "XAUUSD"
eur$Symbol = "EURUSD"

totalminutes <- rbind(jpy, chf, xau, eur)
totalminutes = totalminutes %>% mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down"))) #Bar column
totalminutes %>% group_by(Symbol, Bar) %>% summarise(Upcount = n()) #daily up/down by currency
 #not this: t.test(0:5, alternative = "two.sided", mu = 0.5, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#Symbol Bar     Upcount
#<chr>  <chr>     <int>
#1 EURUSD Down    2078754
#2 EURUSD Neutral  286234
#3 EURUSD Up      2083026
#  2078754 / (2083026+2078754) = 0.4994868
#4 USDCHF Down    1928331
#5 USDCHF Neutral  593450
#6 USDCHF Up      1920242
#7 USDJPY Down    1961730
#8 USDJPY Neutral  520863
#9 USDJPY Up      1928300
#10 XAUUSD Down    2080718
#11 XAUUSD Neutral   95577
#12 XAUUSD Up      2058924


### total sum 
totalminutes %>% group_by(Symbol) %>% summarise(Upcount = n())
# A tibble: 4 x 2
# Symbol Upcount
# <chr>    <int>
#1 EURUSD 4448014
#2 USDCHF 4442023
#3 USDJPY 4410893
#4 XAUUSD 4235219


##https://stats.stackexchange.com/questions/21581/how-to-assess-whether-a-coin-tossed-900-times-and-comes-up-heads-490-times-is-bi
## A coin is tossed 900 times and heads appeared 490 times. Does the result support the hypothesis that the coin is unbiased?
## Here the natural null-hypothesis H0 is that the coin is unbiased, that is, that the probability p of a head is equal to 1/2. 
## The most reasonable alternate hypothesis H1 is that pกฺ1/2, though one could make a case for the one-sided alternate hypothesis p>1/2.
## We need to choose the significance level of the test. That's up to you. Two traditional numbers are 5% and 1%.

#H0: Probability p of Up/Down bar is equal to 1/2
#H1: Probability p of Up/Down bar is not equal to 1/2
#Significance at 5%
#1  EURUSD Down    2078754
#2  EURUSD Neutral  286234
#3  EURUSD Up      2083026
#Total  EURUSD 4448014
2083026/ 4161780

dbinom(27, size=100, prob=0.25)
dbinom(2083026, size=4161780, prob=0.5) #up bar 4.365959e-05
dbinom(2078754, size=4161780, prob=0.5) #down bar 4.365959e-05

pbinom(50, size=100, prob=0.5, lower.tail = TRUE, log.p = FALSE)

#dbinom gives the density, pbinom gives the distribution function, 
#qbinom gives the quantile function and rbinom generates random deviates.

## Suppose that the null hypothesis holds. Then the number of heads has *binomial distribution with mean (900)(1/2)=450,
## and standard deviation กิ(900)(1/2)(1/2) = 15.

#if Null Hypothesis holds, number of up bars has *binomial distribution with mean (4448014)(1/2)= 2224007
# standard deviation of กิ(4448014)(1/2)(1/2), sqrt(4448014*(1/2)*(1/2)) = 1054.516

#4161780*(1/2)= 2080890#mean
#sqrt(4161780*(1/2)*(1/2)) = 1020.022 #sd

## Not bothering to approximate by the normal, we can look at a random variable distributed binomial with n=900 and p=0.5 
## under the null hypothesis (i.e. if the coin were unbiased then p=probability of heads(or tails) = 0.5).
## If we would like to test the alternative that Ha: p<>0.5 at alpha 0.05 we can look at the tails of the distribution under 
## the null as follows and see that 490 falls outside the interval {421, 479} and thus we reject Ho.

n<- 4161780
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p)
# 2078891 2082889



##############################################################################
#use stats
#One Sample T-Test
#examines the average difference between a sample and the known value of the population mean

t.test(x, y = NULL, alternative = c("two.sided
", "less", "greater"), mu = 0, paired = 
         FALSE, var.equal = FALSE, 
       conf.level = 0.95
)

####statistical significance of coinflip
#https://www.stat.wisc.edu/~larget/R/prob-R.pdf
#https://www.tutorialspoint.com/r/r_binomial_distribution.htm
#library(prob.R) #not available in R3.5
#This plot will help visualize the probability of getting between 45 and 55 heads in 100 coin tosses.
#gbinom(100, 0.5, a = 45, b = 55, scale = T)

dbinom(0:5, 5, 0.1) #0.59049 0.32805 0.07290 0.00810 0.00045 0.00001
dbinom(10, 100, 0.1) #0.1318653

rbinom(100, 10, 0.5)

pbinom(0:5, 5, .5)

qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)

?qbinom
#bayesian statistics
#http://nickstrayer.me/pValueExplainer/
#seems more useful for calculating consecutive flips of a coin

#http://www.instantr.com/2012/11/06/performing-a-binomial-test/
#https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/binom.test
binom.test(2083026, 4161780, 1/2, alternative = "two.sided")
#Exact binomial test
#data:  2078754 and 4161780
#number of successes = 2078800, number of trials = 4161800, p-value = 0.0363
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.4990063 0.4999673
#sample estimates:
#  probability of success 
#0.4994868 

binom.test(2078754, 4161780, 1/2, alternative = "less")

binom.test(c(2083026,2078754), 1/2, alternative = "two.sided")
##############################################################################

#Slack 
Testing the random walk hypothesis with EURUSD data

The random walk hypothesis is a financial theory stating that stock market prices evolve 
according to a random walk (so price changes are random) and thus cannot be predicted. 
It is consistent with the efficient-market hypothesis.

Using the minutes data of EURUSD from 2007 to 2018. 
For each minute bar, taking the definition of an Up Bar as Close Price > Open Price:
  mutate(Bar = ifelse(Open == Close, "Neutral", ifelse(Close > Open, "Up", "Down")))

H0: Probability p of an Up/Down bar is equal to 1/2
H1: Probability p of an Up/Down bar is not equal to 1/2
(basically, coin flip)

Symbol   Bar      Upcount
<chr>    <chr>    <int>
1 EURUSD Down     2078754
2 EURUSD Neutral  286234
3 EURUSD Up       2083026

excluding neutral bars, total bars: 2083026+2078754 = 4161780
% of Up Bars: 2083026 / 4161780 = 0.5005132

Question: statistical significance? Can I reject the null hypothesis?
  
#dbinom
dbinom(2083026, size=4161780, prob=0.5) #up bar 4.365959e-05
dbinom(2078754, size=4161780, prob=0.5) #down bar 4.365959e-05

It seems that the p-value is statistically significant.

#qbinom
n<- 4161780
p<- 0.5
qbinom(c(0.025,0.975),size=n,prob=p) #5% significance
2078891 2082889

#binom.test
> binom.test(2083026, 4161780, 1/2, alternative = "two.sided")

Exact binomial test

data:  2083026 and 4161780
number of successes = 2083000, number of trials = 4161800, p-value = 0.0363
alternative hypothesis: true probability of success is not equal to 0.5
95 percent confidence interval:
  0.5000327 0.5009937
sample estimates:
  probability of success 
0.5005132 

#Drace
If youre working with stock data, youre far better off examining if something is a random walk process (with drift) 
by looking at its acf/pacf. (random walk will show that all lags has significant coefficients for acf, and pacf 
significant at lag 1). This is almost certain as stock behavior does fall under random walk.  Also another thing to 
note is if you are modeling stock data on how it trades by the minute, you may have to use additional techniques as well.







