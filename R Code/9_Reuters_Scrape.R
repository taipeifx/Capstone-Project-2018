
setwd("D:/NYCDSA/Project 4 - Capstone Project")

library(readr)

### Grab Files
reuters = read_csv("reuters.csv", col_names = T)

####################################################################################### CLEANING REUTERS
### Cleaning <U+00A0>
#data <- data.frame(lapply(blog_info$excerpt[1], function(x) {gsub("\u00A0", " ", x)}))

#blog_info$excerpt <- gsub("\u00A0", ' ', blog_info$excerpt) # \u00A0 is java

### Cleaning dates column
#install.packages("anytime")
library(anytime)
reuters$page[9] #"<200 https://uk.reuters.com/news/archive/GCA-ForeignExchange?view=page&page=23&pageSize=10>"


reuters$page <- sub("<200 https://uk.reuters.com/news/archive/GCA-ForeignExchange>", '1', reuters$page)
reuters$page <- gsub("<200 https://uk.reuters.com/news/archive/GCA", '', reuters$page)
reuters$page <- gsub("-", '', reuters$page)
reuters$page <- gsub("=", '', reuters$page)
reuters$page <- gsub("&", '', reuters$page)
reuters$page <- gsub("\\?", '', reuters$page) #escape ?
reuters$page <- gsub("ForeignExchangeviewpagepage", '', reuters$page)
reuters$page <- gsub("pageSize10>", '', reuters$page)


reuters$page = as.numeric(reuters$page)


b = table(reuters$page)
b = data.frame(b)
b[b$Freq<10,][[1]]
#Var1 Freq
#22     22    8
#33     33    9
#57     57    9
#106   106    9
#112   112    9
#113   113    9
#125   125    9
#215   215    8
#225   225    9
#243   243    8
#275   275    9
#278   278    9
#1041 1041    2
#rescrape these pages
# result_urls = ['https://uk.reuters.com/news/archive/GCA-ForeignExchange?view=page&page={}&pageSize=10'.format(x) for x 
# in (22,33,57,106,112,113,125,215,225,243,275,278,1041)]


##########################################################################################################################################
##########################################################################################################################################
reuters_rescrape = read_csv("reuters_rescrape.csv", col_names = T)

#clean rescrape
reuters_rescrape$page <- gsub("<200 https://uk.reuters.com/news/archive/GCA", '', reuters_rescrape$page)
reuters_rescrape$page <- gsub("-", '', reuters_rescrape$page)
reuters_rescrape$page <- gsub("=", '', reuters_rescrape$page)
reuters_rescrape$page <- gsub("&", '', reuters_rescrape$page)
reuters_rescrape$page <- gsub("\\?", '', reuters_rescrape$page) #escape ?
reuters_rescrape$page <- gsub("ForeignExchangeviewpagepage", '', reuters_rescrape$page)
reuters_rescrape$page <- gsub("pageSize10>", '', reuters_rescrape$page)

reuters_rescrape$page = as.numeric(reuters_rescrape$page)

table(reuters_rescrape$page)
# 22   33   57  106  112  113  125  215  225  243  275  278 1041 
# 9    9    9    9    9   10    9    9    9    8    9    9    3 

n_occur <- data.frame(table(reuters$title))
non_duplicate = reuters_rescrape[!reuters_rescrape$title %in% n_occur$Var1,]

# a = reuters[reuters$title %in% n_occur$Var1[n_occur$Freq > 1],] #in, 365
# a = reuters[!reuters$title %in% n_occur$Var1[n_occur$Freq > 1],] #not in, 10022

#some links redirect, thus missing
##########################################################################################################################################
##########################################################################################################################################


# find duplicates
n_occur <- data.frame(table(reuters$title))

n_occur[n_occur$Freq > 1,] #178 repeated articles (some more than one repeat), edited articles
#Var1 Freq
#3                                  'Grexit' becomes base case for many, but markets barely blink    2
#42                                         Agony for Wall St. economists, investors as Fed meets    2
#68                              Analysis - After Portugal nod, Finland faces own fiscal timebomb    2

a = reuters[reuters$title %in% n_occur$Var1[n_occur$Freq > 1],] #365, number of all repeated articles

#reuters_unique = reuters[reuters$title %in% unique(reuters$title),]

# remove duplicates
reuters_unique = reuters[order(reuters$page),] #10387
reuters_unique = reuters_unique[!duplicated(reuters_unique$title),] #10200 

# a = reuters[!reuters$title %in% n_occur$Var1[n_occur$Freq > 1],] #not in, 10022
# n_occur[n_occur$Freq > 1,] #178 
# = 10200

b = table(reuters_unique$page)
b = data.frame(b)
b = b[b$Freq<10,]

##########################################################################################################################################
##########################################################################################################################################


# more data cleaning
library(tidyr)

reuters_unique = reuters_unique %>% separate(article_date, c("Date", "Timestamp", "Days_Ago"), sep = "/")
reuters_unique = reuters_unique[-3]

reuters_unique$Date <- sub(" 1,", ' 01,', reuters_unique$Date)
reuters_unique$Date <- sub(" 2,", ' 02,', reuters_unique$Date)
reuters_unique$Date <- sub(" 3,", ' 03,', reuters_unique$Date)
reuters_unique$Date <- sub(" 4,", ' 04,', reuters_unique$Date)
reuters_unique$Date <- sub(" 5,", ' 05,', reuters_unique$Date)
reuters_unique$Date <- sub(" 6,", ' 06,', reuters_unique$Date)
reuters_unique$Date <- sub(" 8,", ' 08,', reuters_unique$Date)
reuters_unique$Date <- sub(" 9,", ' 09,', reuters_unique$Date)

reuters_unique$Date <- sub(" 7,", ' 07,', reuters_unique$Date)

reuters_unique$Date <- anydate(reuters_unique$Date) # anytime library

library(lubridate)
reuters_unique_time = reuters_unique$Timestamp
reuters_unique_time = data.frame(reuters_unique_time)
reuters_unique_time$reuters_unique_time = as.character(reuters_unique_time$reuters_unique_time)

class(reuters_unique_time$reuters_unique_time)
colnames(reuters_unique_time)[1] = "Timestamp"

class(reuters_unique_time$Timestamp)
#####

#reuters_unique_time = parse_date_time(reuters_unique_time[[1]], 'I:M p')
reuters_unique_time$Timestamp = parse_date_time(reuters_unique_time[[1]], 'I:M p')

reuters_unique_time$Timestamp <- gsub("0000-01-01", '', reuters_unique_time$Timestamp)

reuters_unique_time$Timestamp = format(strptime(reuters_unique_time$Timestamp, format = "%H:%M:%S"), '%H:%M:%S') #this

reuters_unique$Timestamp = reuters_unique_time$Timestamp

class(reuters_unique$Timestamp)
class(reuters_unique$Date)

#write.csv(reuters_unique, "reuters_data.csv", row.names = FALSE)
reuters_data = read_csv("reuters_data.csv")


#########################################################################################################
reuters_data$Timestamp = as.character(reuters_data$Timestamp)
class(reuters_data$Timestamp)
class(reuters_data$Date)

head(reuters_data$Timestamp)
hour(reuters_unique$Timestamp)

#options(tz=)
tz(reuters_data$Timestamp)
tz(eur$Timestamp)
x <- as.POSIXct(format(strptime(reuters_unique$Timestamp, "%H:%M:%S")), '%H:%M:%S') 

eur = read_csv("eurdate.csv")
class(eur$Timestamp)
hour(eur$Timestamp)
head(eur$Timestamp)

library(chron)
a = chron(times = reuters_data$Timestamp)
class(a)
minute(a)
x = as_datetime(w)
minute(x)

w <- strptime(x = reuters_data$Timestamp, format = "%H:%M:%S")
class(w)
w

reuters_data$Timestamp[1] == eur$Timestamp[1]
