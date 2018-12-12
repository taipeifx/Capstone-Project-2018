
library(readr)
library(anytime)
library(tidyquant)
library(dplyr)

library(Hmisc)
library(psych)
setwd("D:/NYCDSA/Project 4 - Capstone Project")


#reuters_data = read_csv("reuters_data.csv")
#eur = read_csv("eurfinal50var.csv")
##################################################################################################
colnames(eur) 
#[1] "Date"          "Timestamp"     "Close"         "Bar"           "UpCount"       "DownCount"     "Real.Range"    "Neutral.Range" "Body"         
#[10] "TopWick"       "BottomWick"    "EURVolume"     "JPYClose"      "JPYRange"      "JPYVolume"     "CHFClose"      "CHFRange"      "CHFVolume"    
#[19] "XAUClose"      "XAURange"      "XAUVolume"     

"and Technical Indicators"
#                                                     "class"         "forceindex"    "WillR5"        "WillR10"       "WillR15"       "RSI5"         
#[28] "RSI10"         "RSI15"         "ROC5"          "ROC10"         "MOM5"          "MOM10"         "ATR5"          "ATR10"         "ADX.DIp"      
#[37] "ADX.DIn"       "ADX.DX"        "ADX"           "SAR"           "Stoch.fastK"   "Stoch.fastD"   "Stoch.slowD"   "BBands.dn"     "BBandsm.avg"  
#[46] "BBands.up"     "BBands.pctB"   "MACD"          "MACD.Signal"   "MACD.Signal2" 

#head(eur[500,],1)
#Date       Timestamp Close Bar   UpCount DownCount Real.Range Neutral.Range    Body TopWick BottomWick EURVolume JPYClose JPYRange JPYVolume CHFClose CHFRange
#<date>     <time>    <dbl> <chr>   <int>     <int>      <dbl>         <dbl>   <dbl>   <dbl>      <dbl>     <dbl>    <dbl>    <dbl>     <dbl>    <dbl>    <dbl>
#  1 2007-01-01 03:19      1.32 Up          3        NA   0.000420            NA 3.20e-4       0  0.0001000      780.     119.    0.019      323.     1.22 -0.00017
# ... with 33 more variables: CHFVolume <dbl>, XAUClose <dbl>, XAURange <dbl>, XAUVolume <dbl>, class <chr>, forceindex <dbl>, WillR5 <dbl>, WillR10 <dbl>,
#   WillR15 <dbl>, RSI5 <dbl>, RSI10 <dbl>, RSI15 <dbl>, ROC5 <dbl>, ROC10 <dbl>, MOM5 <dbl>, MOM10 <dbl>, ATR5 <dbl>, ATR10 <dbl>, ADX.DIp <dbl>,
#   ADX.DIn <dbl>, ADX.DX <dbl>, ADX <dbl>, SAR <dbl>, Stoch.fastK <dbl>, Stoch.fastD <dbl>, Stoch.slowD <dbl>, BBands.dn <dbl>, BBandsm.avg <dbl>,
#   BBands.up <dbl>, BBands.pctB <dbl>, MACD <dbl>, MACD.Signal <dbl>, MACD.Signal2 <chr>

##################################################################################################

colnames(reuters_data)
#[1] "Date"      "Timestamp" "excerpt"   "link"      "page"      "post"      "title"   

head(reuters_data, 3)
#Date       Timestamp excerpt        link       page post                                                                                                title 
#<date>     <time>    <chr>          <chr>     <int> <chr>                                                                                               <chr> 
#1 2018-11-27 02:19     The U.S. doll~ https://~     1 "NEW YORK (Reuters) - The U.S. dollar gained on Tuesday after Federal Reserve Vice Chair Richard C~ Dolla~
#2 2018-11-27 10:30     Sterling slum~ https://~     1 "LONDON (Reuters) - Sterling slumped against the dollar and the euro on Tuesday as doubts grew abo~ Sterl~
#3 2018-11-28 09:25     "Sterling gav~ https://~     1 "LONDON (Reuters) - Sterling gave up most of its earlier gains and traded broadly flat on the day ~ Sterl~
