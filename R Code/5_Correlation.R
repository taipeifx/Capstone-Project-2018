
############################################################################## correlation
############################################################################## 2007
cor()
#stats, correlation by year
eur2007 = eur[year(eur$Date) == 2007,]  #for year 2007
jpy2007 = jpy[year(jpy$Date) == 2007,] 
chf2007 = chf[year(chf$Date) == 2007,] 
xau2007 = xau[year(xau$Date) == 2007,] 

eur2007cor = eur2007[,c(1:6)]
colnames(eur2007cor) = c("Date", "Timestamp","EUROpen","EURHigh","EURLow","EURClose")

jpy2007cor = jpy2007[,c(1:6)]
colnames(jpy2007cor) = c("Date", "Timestamp","JPYOpen","JPYHigh","JPYLow","JPYClose")

chf2007cor = chf2007[,c(1:6)]
colnames(chf2007cor) = c("Date", "Timestamp","CHFOpen","CHFHigh","CHFLow","CHFClose")

xau2007cor = xau2007[,c(1:6)]
colnames(xau2007cor) = c("Date", "Timestamp","XAUOpen","XAUHigh","XAULow","XAUClose")

merge2007 = merge(chf2007cor, eur2007cor, by= c("Date","Timestamp"), all.x = TRUE)
merge2007 = merge(merge2007, jpy2007cor, by= c("Date","Timestamp"), all.x = TRUE)
merge2007 = merge(merge2007, xau2007cor, by= c("Date","Timestamp"), all.x = TRUE)

#which(is.na(merge2007))
which(is.na(merge2007),arr.ind=TRUE)[1]
merge2007[223454,]
#merge2007[is.na(merge2007),]
#unique(chf2007cor)

#cor(merge2007[,c(3:18)], use = "na.or.complete") #complete.obs , 
corr1 = cor(merge2007[,c(3,7,11,15)], use = "complete.obs") #missing values are handled by casewise deletion
#corr2 = cor(merge2007[,c(3,7,11,15)], use = "pairwise.complete.obs") #using all complete pairs of observations on those variables

library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
#corrplot.mixed(corr1, lower = "number", upper = "circle", title = "Four Currencies 2007 Correlation") #"circle"
corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2007 Correlation", mar=c(0,0,2,0)) #"circle"
#corrplot(corr2, method = "number") # Display the correlation coefficient

cor(merge2007[,c(3,7,11,15)], use = "complete.obs")
#CHFOpen    EUROpen    JPYOpen    XAUOpen
#CHFOpen  1.0000000 -0.9687375  0.8827654 -0.9300167
#EUROpen -0.9687375  1.0000000 -0.7778656  0.9416268
#JPYOpen  0.8827654 -0.7778656  1.0000000 -0.8012310
#XAUOpen -0.9300167  0.9416268 -0.8012310  1.0000000

m2007 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2007)[3:4] = c("cor 2007", "p 2007")

##############################################################################
##############################################################################2008
eur2008cor = eur[year(eur$Date) == 2008,][,c(1:3)]  #for year 2008
jpy2008cor = jpy[year(jpy$Date) == 2008,][,c(1:3)]
chf2008cor = chf[year(chf$Date) == 2008,][,c(1:3)]
xau2008cor = xau[year(xau$Date) == 2008,][,c(1:3)]

colnames(eur2008cor) = c("Date", "Timestamp","EUROpen","EURHigh","EURLow","EURClose")

colnames(jpy2008cor) = c("Date", "Timestamp","JPYOpen","JPYHigh","JPYLow","JPYClose")

colnames(chf2008cor) = c("Date", "Timestamp","CHFOpen","CHFHigh","CHFLow","CHFClose")

colnames(xau2008cor) = c("Date", "Timestamp","XAUOpen","XAUHigh","XAULow","XAUClose")

merge2008 = merge(chf2008cor, eur2008cor, by= c("Date","Timestamp"), all.x = TRUE)
merge2008 = merge(merge2008, jpy2008cor, by= c("Date","Timestamp"), all.x = TRUE)
merge2008 = merge(merge2008, xau2008cor, by= c("Date","Timestamp"), all.x = TRUE)


corr1 = cor(merge2008[,c(3,7,11,15)], use = "complete.obs")
#corr1 = cor(merge2008[,c(3:18)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2008 Correlation", mar=c(0,0,2,0))

cor(merge2008[,c(3,7,11,15)], use = "complete.obs")
#CHFOpen    EUROpen    JPYOpen    XAUOpen
#CHFOpen  1.0000000 -0.9526425 -0.3754851 -0.8133304
#EUROpen -0.9526425  1.0000000  0.5572367  0.8179203
#JPYOpen -0.3754851  0.5572367  1.0000000  0.3437958
#XAUOpen -0.8133304  0.8179203  0.3437958  1.0000000

m2008 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2008)[3:4] = c("cor 2008", "p 2008")

yearlycorrelation = merge(m2007, m2008, by= c("row","column"))

##############################################################################
eur2007cor = eur[year(eur$Date) == 2007,][,c(1:3)]  #for year 2007
jpy2007cor = jpy[year(jpy$Date) == 2007,][,c(1:3)]
chf2007cor = chf[year(chf$Date) == 2007,][,c(1:3)]
xau2007cor = xau[year(xau$Date) == 2007,][,c(1:3)]

colnames(eur2007cor) = c("Date", "Timestamp","EUR")
colnames(jpy2007cor) = c("Date", "Timestamp","JPY")
colnames(chf2007cor) = c("Date", "Timestamp","CHF")
colnames(xau2007cor) = c("Date", "Timestamp","XAU")

merge2007 = merge(chf2007cor, eur2007cor, by= c("Date","Timestamp"), all = TRUE)
merge2007 = merge(merge2007, jpy2007cor, by= c("Date","Timestamp"), all = TRUE)
merge2007 = merge(merge2007, xau2007cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2007[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2007 Correlation", mar=c(0,0,2,0))

m2007 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2007)[3:4] = c("cor 2007", "p 2007")

eur2008cor = eur[year(eur$Date) == 2008,][,c(1:3)]  #for year 2008
jpy2008cor = jpy[year(jpy$Date) == 2008,][,c(1:3)]
chf2008cor = chf[year(chf$Date) == 2008,][,c(1:3)]
xau2008cor = xau[year(xau$Date) == 2008,][,c(1:3)]

colnames(eur2008cor) = c("Date", "Timestamp","EUR")
colnames(jpy2008cor) = c("Date", "Timestamp","JPY")
colnames(chf2008cor) = c("Date", "Timestamp","CHF")
colnames(xau2008cor) = c("Date", "Timestamp","XAU")

merge2008 = merge(chf2008cor, eur2008cor, by= c("Date","Timestamp"), all = TRUE)
merge2008 = merge(merge2008, jpy2008cor, by= c("Date","Timestamp"), all = TRUE)
merge2008 = merge(merge2008, xau2008cor, by= c("Date","Timestamp"), all = TRUE)

corr1 = cor(merge2008[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2008 Correlation", mar=c(0,0,2,0))

m2008 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2008)[3:4] = c("cor 2008", "p 2008")

yearlycorrelation = merge(m2007, m2008, by= c("row","column"))
##############################################################################2009
eur2009cor = eur[year(eur$Date) == 2009,][,c(1:3)]  #for year 2009
jpy2009cor = jpy[year(jpy$Date) == 2009,][,c(1:3)]
chf2009cor = chf[year(chf$Date) == 2009,][,c(1:3)]
xau2009cor = xau[year(xau$Date) == 2009,][,c(1:3)]

colnames(eur2009cor) = c("Date", "Timestamp","EUR")
colnames(jpy2009cor) = c("Date", "Timestamp","JPY")
colnames(chf2009cor) = c("Date", "Timestamp","CHF")
colnames(xau2009cor) = c("Date", "Timestamp","XAU")

merge2009 = merge(chf2009cor, eur2009cor, by= c("Date","Timestamp"), all = TRUE)
merge2009 = merge(merge2009, jpy2009cor, by= c("Date","Timestamp"), all = TRUE)
merge2009 = merge(merge2009, xau2009cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2009[,c(3:6)], use = "complete.obs")
#corr1 = cor(merge2009[,c(3:18)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2009 Correlation", mar=c(0,0,2,0))

corr1
#CHF        EUR        JPY        XAU
#CHF  1.0000000 -0.9874863  0.5968889 -0.7804952
#EUR -0.9874863  1.0000000 -0.5374892  0.7619884
#JPY  0.5968889 -0.5374892  1.0000000 -0.5865985
#XAU -0.7804952  0.7619884 -0.5865985  1.0000000

m2009 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2009)[3:4] = c("cor 2009", "p 2009")


m2009$column = as.character(m2009$column)
m2009$row = as.character(m2009$row)
m2009[2,1:2] = c("CHF", "EUR")
m2009[3,1:2] = c("CHF", "XAU")
m2009[4,1:2] = c("JPY", "EUR")
m2009[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2009, by= c("row","column"))

##############################################################################
##############################################################################2010
eur2010cor = eur[year(eur$Date) == 2010,][,c(1:3)]  #for year 2010
jpy2010cor = jpy[year(jpy$Date) == 2010,][,c(1:3)]
chf2010cor = chf[year(chf$Date) == 2010,][,c(1:3)]
xau2010cor = xau[year(xau$Date) == 2010,][,c(1:3)]

colnames(eur2010cor) = c("Date", "Timestamp","EUR")
colnames(jpy2010cor) = c("Date", "Timestamp","JPY")
colnames(chf2010cor) = c("Date", "Timestamp","CHF")
colnames(xau2010cor) = c("Date", "Timestamp","XAU")

merge2010 = merge(chf2010cor, eur2010cor, by= c("Date","Timestamp"), all = TRUE)
merge2010 = merge(merge2010, jpy2010cor, by= c("Date","Timestamp"), all = TRUE)
merge2010 = merge(merge2010, xau2010cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2010[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2010 Correlation", mar=c(0,0,2,0))

corr1
#CHF         EUR        JPY         XAU
#CHF  1.0000000 -0.55889751  0.8025418 -0.64610248
#EUR -0.5588975  1.00000000 -0.1458686 -0.07696425
#JPY  0.8025418 -0.14586858  1.0000000 -0.80729149
#XAU -0.6461025 -0.07696425 -0.8072915  1.00000000

m2010 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2010)[3:4] = c("cor 2010", "p 2010")


m2010$column = as.character(m2010$column)
m2010$row = as.character(m2010$row)
m2010[2,1:2] = c("CHF", "EUR")
m2010[3,1:2] = c("CHF", "XAU")
m2010[4,1:2] = c("JPY", "EUR")
m2010[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2010, by= c("row","column"))

##############################################################################
##############################################################################2011
eur2011cor = eur[year(eur$Date) == 2011,][,c(1:3)]  #for year 2011
jpy2011cor = jpy[year(jpy$Date) == 2011,][,c(1:3)]
chf2011cor = chf[year(chf$Date) == 2011,][,c(1:3)]
xau2011cor = xau[year(xau$Date) == 2011,][,c(1:3)]

colnames(eur2011cor) = c("Date", "Timestamp","EUR")
colnames(jpy2011cor) = c("Date", "Timestamp","JPY")
colnames(chf2011cor) = c("Date", "Timestamp","CHF")
colnames(xau2011cor) = c("Date", "Timestamp","XAU")

merge2011 = merge(chf2011cor, eur2011cor, by= c("Date","Timestamp"), all = TRUE)
merge2011 = merge(merge2011, jpy2011cor, by= c("Date","Timestamp"), all = TRUE)
merge2011 = merge(merge2011, xau2011cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2011[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2011 Correlation", mar=c(0,0,2,0))

corr1
#CHF         EUR        JPY         XAU
#CHF  1.0000000 -0.69674196  0.4202642 -0.52180475
#EUR -0.6967420  1.00000000  0.1604737  0.02475498
#JPY  0.4202642  0.16047369  1.0000000 -0.87386454
#XAU -0.5218048  0.02475498 -0.8738645  1.00000000

m2011 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2011)[3:4] = c("cor 2011", "p 2011")


m2011$column = as.character(m2011$column)
m2011$row = as.character(m2011$row)
m2011[2,1:2] = c("CHF", "EUR")
m2011[3,1:2] = c("CHF", "XAU")
m2011[4,1:2] = c("JPY", "EUR")
m2011[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2011, by= c("row","column"))

##############################################################################
##############################################################################2012
eur2012cor = eur[year(eur$Date) == 2012,][,c(1:3)]  #for year 2012
jpy2012cor = jpy[year(jpy$Date) == 2012,][,c(1:3)]
chf2012cor = chf[year(chf$Date) == 2012,][,c(1:3)]
xau2012cor = xau[year(xau$Date) == 2012,][,c(1:3)]

colnames(eur2012cor) = c("Date", "Timestamp","EUR")
colnames(jpy2012cor) = c("Date", "Timestamp","JPY")
colnames(chf2012cor) = c("Date", "Timestamp","CHF")
colnames(xau2012cor) = c("Date", "Timestamp","XAU")

merge2012 = merge(chf2012cor, eur2012cor, by= c("Date","Timestamp"), all = TRUE)
merge2012 = merge(merge2012, jpy2012cor, by= c("Date","Timestamp"), all = TRUE)
merge2012 = merge(merge2012, xau2012cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2012[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2012 Correlation", mar=c(0,0,2,0))

corr1
#CHF        EUR          JPY          XAU
#CHF  1.0000000 -0.9930762 -0.439259154 -0.549248046
#EUR -0.9930762  1.0000000  0.408679600  0.592621230
#JPY -0.4392592  0.4086796  1.000000000 -0.008705802
#XAU -0.5492480  0.5926212 -0.008705802  1.000000000

m2012 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2012)[3:4] = c("cor 2012", "p 2012")


m2012$column = as.character(m2012$column)
m2012$row = as.character(m2012$row)
m2012[2,1:2] = c("CHF", "EUR")
m2012[3,1:2] = c("CHF", "XAU")
m2012[4,1:2] = c("JPY", "EUR")
m2012[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2012, by= c("row","column"))

##############################################################################
##############################################################################2013
eur2013cor = eur[year(eur$Date) == 2013,][,c(1:3)]  #for year 2013
jpy2013cor = jpy[year(jpy$Date) == 2013,][,c(1:3)]
chf2013cor = chf[year(chf$Date) == 2013,][,c(1:3)]
xau2013cor = xau[year(xau$Date) == 2013,][,c(1:3)]

colnames(eur2013cor) = c("Date", "Timestamp","EUR")
colnames(jpy2013cor) = c("Date", "Timestamp","JPY")
colnames(chf2013cor) = c("Date", "Timestamp","CHF")
colnames(xau2013cor) = c("Date", "Timestamp","XAU")

merge2013 = merge(chf2013cor, eur2013cor, by= c("Date","Timestamp"), all = TRUE)
merge2013 = merge(merge2013, jpy2013cor, by= c("Date","Timestamp"), all = TRUE)
merge2013 = merge(merge2013, xau2013cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2013[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2013 Correlation", mar=c(0,0,2,0))

corr1
#CHF        EUR         JPY        XAU
#CHF  1.00000000 -0.9431601 -0.04988184  0.2601211
#EUR -0.94316011  1.0000000  0.15612299 -0.3470781
#JPY -0.04988184  0.1561230  1.00000000 -0.8273922
#XAU  0.26012114 -0.3470781 -0.82739216  1.0000000

m2013 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2013)[3:4] = c("cor 2013", "p 2013")


m2013$column = as.character(m2013$column)
m2013$row = as.character(m2013$row)
m2013[3,1:2] = c("EUR", "XAU")
m2013[6,1:2] = c("JPY", "EUR")
m2013[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2013, by= c("row","column"))

##############################################################################
##############################################################################2014
eur2014cor = eur[year(eur$Date) == 2014,][,c(1:3)]  #for year 2014
jpy2014cor = jpy[year(jpy$Date) == 2014,][,c(1:3)]
chf2014cor = chf[year(chf$Date) == 2014,][,c(1:3)]
xau2014cor = xau[year(xau$Date) == 2014,][,c(1:3)]

colnames(eur2014cor) = c("Date", "Timestamp","EUR")
colnames(jpy2014cor) = c("Date", "Timestamp","JPY")
colnames(chf2014cor) = c("Date", "Timestamp","CHF")
colnames(xau2014cor) = c("Date", "Timestamp","XAU")

merge2014 = merge(chf2014cor, eur2014cor, by= c("Date","Timestamp"), all = TRUE)
merge2014 = merge(merge2014, jpy2014cor, by= c("Date","Timestamp"), all = TRUE)
merge2014 = merge(merge2014, xau2014cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2014[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2014 Correlation", mar=c(0,0,2,0))

corr1
#CHF        EUR        JPY        XAU
#CHF  1.0000000 -0.9934687  0.9150325 -0.8725159
#EUR -0.9934687  1.0000000 -0.9069526  0.8379539
#JPY  0.9150325 -0.9069526  1.0000000 -0.8135803
#XAU -0.8725159  0.8379539 -0.8135803  1.0000000

m2014 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2014)[3:4] = c("cor 2014", "p 2014")


m2014$column = as.character(m2014$column)
m2014$row = as.character(m2014$row)
m2014[2,1:2] = c("CHF", "EUR")
m2014[3,1:2] = c("CHF", "XAU")
m2014[4,1:2] = c("JPY", "EUR")
m2014[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2014, by= c("row","column"))

##############################################################################
##############################################################################2015
eur2015cor = eur[year(eur$Date) == 2015,][,c(1:3)]  #for year 2015
jpy2015cor = jpy[year(jpy$Date) == 2015,][,c(1:3)]
chf2015cor = chf[year(chf$Date) == 2015,][,c(1:3)]
xau2015cor = xau[year(xau$Date) == 2015,][,c(1:3)]

colnames(eur2015cor) = c("Date", "Timestamp","EUR")
colnames(jpy2015cor) = c("Date", "Timestamp","JPY")
colnames(chf2015cor) = c("Date", "Timestamp","CHF")
colnames(xau2015cor) = c("Date", "Timestamp","XAU")

merge2015 = merge(chf2015cor, eur2015cor, by= c("Date","Timestamp"), all = TRUE)
merge2015 = merge(merge2015, jpy2015cor, by= c("Date","Timestamp"), all = TRUE)
merge2015 = merge(merge2015, xau2015cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2015[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2015 Correlation", mar=c(0,0,2,0))

corr1
#CHF        EUR        JPY        XAU
#CHF  1.0000000 -0.4456420  0.2794639 -0.6922657
#EUR -0.4456420  1.0000000 -0.4319441  0.5012132
#JPY  0.2794639 -0.4319441  1.0000000 -0.6047258
#XAU -0.6922657  0.5012132 -0.6047258  1.0000000

m2015 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2015)[3:4] = c("cor 2015", "p 2015")


m2015$column = as.character(m2015$column)
m2015$row = as.character(m2015$row)
m2015[2,1:2] = c("CHF", "EUR")
m2015[3,1:2] = c("CHF", "XAU")
m2015[4,1:2] = c("JPY", "EUR")
m2015[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2015, by= c("row","column"))

##############################################################################
##############################################################################2016
eur2016cor = eur[year(eur$Date) == 2016,][,c(1:3)]  #for year 2016
jpy2016cor = jpy[year(jpy$Date) == 2016,][,c(1:3)]
chf2016cor = chf[year(chf$Date) == 2016,][,c(1:3)]
xau2016cor = xau[year(xau$Date) == 2016,][,c(1:3)]

colnames(eur2016cor) = c("Date", "Timestamp","EUR")
colnames(jpy2016cor) = c("Date", "Timestamp","JPY")
colnames(chf2016cor) = c("Date", "Timestamp","CHF")
colnames(xau2016cor) = c("Date", "Timestamp","XAU")

merge2016 = merge(chf2016cor, eur2016cor, by= c("Date","Timestamp"), all = TRUE)
merge2016 = merge(merge2016, jpy2016cor, by= c("Date","Timestamp"), all = TRUE)
merge2016 = merge(merge2016, xau2016cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2016[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2016 Correlation", mar=c(0,0,2,0))

corr1
#CHF        EUR        JPY        XAU
#CHF  1.0000000 -0.9204729  0.6687391 -0.6937640
#EUR -0.9204729  1.0000000 -0.5025284  0.5609942
#JPY  0.6687391 -0.5025284  1.0000000 -0.9222259
#XAU -0.6937640  0.5609942 -0.9222259  1.0000000

m2016 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2016)[3:4] = c("cor 2016", "p 2016")


m2016$column = as.character(m2016$column)
m2016$row = as.character(m2016$row)
m2016[2,1:2] = c("CHF", "EUR")
m2016[3,1:2] = c("CHF", "XAU")
m2016[4,1:2] = c("JPY", "EUR")
m2016[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2016, by= c("row","column"))

##############################################################################
##############################################################################2017
eur2017cor = eur[year(eur$Date) == 2017,][,c(1:3)]  #for year 2017
jpy2017cor = jpy[year(jpy$Date) == 2017,][,c(1:3)]
chf2017cor = chf[year(chf$Date) == 2017,][,c(1:3)]
xau2017cor = xau[year(xau$Date) == 2017,][,c(1:3)]

colnames(eur2017cor) = c("Date", "Timestamp","EUR")
colnames(jpy2017cor) = c("Date", "Timestamp","JPY")
colnames(chf2017cor) = c("Date", "Timestamp","CHF")
colnames(xau2017cor) = c("Date", "Timestamp","XAU")

merge2017 = merge(chf2017cor, eur2017cor, by= c("Date","Timestamp"), all = TRUE)
merge2017 = merge(merge2017, jpy2017cor, by= c("Date","Timestamp"), all = TRUE)
merge2017 = merge(merge2017, xau2017cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2017[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2017 Correlation", mar=c(0,0,2,0))

corr1
#CHF        EUR        JPY        XAU
#CHF  1.0000000 -0.7135868  0.6048669 -0.5731959
#EUR -0.7135868  1.0000000 -0.3248278  0.7227290
#JPY  0.6048669 -0.3248278  1.0000000 -0.7010302
#XAU -0.5731959  0.7227290 -0.7010302  1.0000000

m2017 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2017)[3:4] = c("cor 2017", "p 2017")


m2017$column = as.character(m2017$column)
m2017$row = as.character(m2017$row)
m2017[2,1:2] = c("CHF", "EUR")
m2017[3,1:2] = c("CHF", "XAU")
m2017[4,1:2] = c("JPY", "EUR")
m2017[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2017, by= c("row","column"))

##############################################################################
##############################################################################2018
eur2018cor = eur[year(eur$Date) == 2018,][,c(1:3)]  #for year 2018
jpy2018cor = jpy[year(jpy$Date) == 2018,][,c(1:3)]
chf2018cor = chf[year(chf$Date) == 2018,][,c(1:3)]
xau2018cor = xau[year(xau$Date) == 2018,][,c(1:3)]

colnames(eur2018cor) = c("Date", "Timestamp","EUR")
colnames(jpy2018cor) = c("Date", "Timestamp","JPY")
colnames(chf2018cor) = c("Date", "Timestamp","CHF")
colnames(xau2018cor) = c("Date", "Timestamp","XAU")

merge2018 = merge(chf2018cor, eur2018cor, by= c("Date","Timestamp"), all = TRUE)
merge2018 = merge(merge2018, jpy2018cor, by= c("Date","Timestamp"), all = TRUE)
merge2018 = merge(merge2018, xau2018cor, by= c("Date","Timestamp"), all = TRUE)


corr1 = cor(merge2018[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2018 Correlation", mar=c(0,0,2,0))

corr1
#CHF        EUR        JPY        XAU
#CHF  1.0000000 -0.8358781  0.6655372 -0.5792928
#EUR -0.8358781  1.0000000 -0.7933730  0.8769781
#JPY  0.6655372 -0.7933730  1.0000000 -0.7525739
#XAU -0.5792928  0.8769781 -0.7525739  1.0000000

m2018 = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(m2018)[3:4] = c("cor 2018", "p 2018")


m2018$column = as.character(m2018$column)
m2018$row = as.character(m2018$row)
m2018[2,1:2] = c("CHF", "EUR")
m2018[3,1:2] = c("CHF", "XAU")
m2018[4,1:2] = c("JPY", "EUR")
m2018[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, m2018, by= c("row","column"))

write.csv(yearlycorrelation,"yearlycorrelation.csv", row.names = FALSE)
##############################################################################
##############################################################################all
eurallcor = eur[,c(1:3)]  #for year all
jpyallcor = jpy[,c(1:3)]
chfallcor = chf[,c(1:3)]
xauallcor = xau[,c(1:3)]

colnames(eurallcor) = c("Date", "Timestamp","EUR")
colnames(jpyallcor) = c("Date", "Timestamp","JPY")
colnames(chfallcor) = c("Date", "Timestamp","CHF")
colnames(xauallcor) = c("Date", "Timestamp","XAU")

mergeall = merge(chfallcor, eurallcor, by= c("Date","Timestamp"), all = TRUE)
mergeall = merge(mergeall, jpyallcor, by= c("Date","Timestamp"), all = TRUE)
mergeall = merge(mergeall, xauallcor, by= c("Date","Timestamp"), all = TRUE)

corr1 = cor(mergeall[,c(3:6)], use = "complete.obs")

corrplot(corr1, method = "number",type = "lower", title = "Four Currencies 2007-2018 Correlation", mar=c(0,0,2,0))

corr1
#CHF        EUR        JPY        XAU
#CHF  1.0000000  0.1194948  0.3401413 -0.8391059
#EUR  0.1194948  1.0000000 -0.4493861 -0.1682256
#JPY  0.3401413 -0.4493861  1.0000000 -0.6001504
#XAU -0.8391059 -0.1682256 -0.6001504  1.0000000

mall = rquery.cormat(corr1, type="flatten", graph=FALSE)$r
colnames(mall)[3:4] = c("cor all", "p all")


mall$column = as.character(mall$column)
mall$row = as.character(mall$row)
mall[2,1:2] = c("CHF", "EUR")
mall[3,1:2] = c("CHF", "XAU")
mall[4,1:2] = c("JPY", "EUR")
mall[5,1:2] = c("JPY", "XAU")

yearlycorrelation = merge(yearlycorrelation, mall, by= c("row","column"))

##############################################################################
#save yearly correlation list

#scatterplot , 2014 EUR CHF
ggplot(merge2014, aes(x=EUR, y=CHF)) + ggtitle("2014 EURUSD USDCHF Correlation (-0.99)") +
  geom_point(shape=1) + xlab("EURUSD") + ylab("USDCHF")
g

ggplot(merge2017, aes(x=JPY, y=CHF)) + ggtitle("2014 USDJPY USDCHF Correlation (0.6)") +
  geom_point(shape=1) + xlab("USDJPY") + ylab("USDCHF")

ggplot(mergeall, aes(x=JPY, y=CHF)) + ggtitle("2007-2018 XAUUSD USDCHF Correlation (-0.84)") +
  geom_point(shape=1) + xlab("XAUUSD") + ylab("USDCHF")

ggplot(mergeall, aes(x=JPY, y=CHF)) + ggtitle("2007-2018 XAUUSD USDCHF Correlation (-0.84)") +
  geom_point(shape=1) + xlab("XAUUSD") + ylab("USDCHF")

ggplot(mergeall, aes(x=EUR, y=JPY)) + ggtitle("2007-2018 EURUSD USDJPY Correlation (-0.45)") +
  geom_point(shape=1) + xlab("EURUSD") + ylab("USDJPY")

ggplot(mergeall, aes(x=EUR, y=CHF)) + ggtitle("2007-2018 EURUSD USDCHF Correlation (0.12)") +
  geom_point(shape=1) + xlab("EURUSD") + ylab("USDCHF")

#dplyr group by
#merge2018 %>% group_by(year(Date), month(Date)) %>% summarise(cor(merge2018$CHF,merge2018$EUR))
monthlycorrelation = mergeall %>% group_by(year(Date), month(Date)) %>% summarise("CHF, EUR" = cor(CHF, EUR, use = "complete.obs"), "CHF, JPY" = cor(CHF, JPY, use = "complete.obs"),
                                                                                  "CHF, XAU" = cor(CHF, XAU, use = "complete.obs"), "EUR, XAU" = cor(EUR, XAU, use = "complete.obs"),
                                                                                  "JPY, EUR" = cor(JPY, EUR, use = "complete.obs"), "JPY, XAU" = cor(JPY, XAU, use = "complete.obs"))



write.csv(monthlycorrelation,"monthlycorrelation.csv", row.names = FALSE)
mean(monthlycorrelation[3][[1]])

a = mergeall %>% group_by(year(Date)) %>% summarise("CHF, EUR" = cor(CHF, EUR, use = "complete.obs"))
mergeall %>% summarise("CHF, EUR" = cor(CHF, EUR, use = "complete.obs"))
mean(a[2][[1]])

mergeall %>% group_by(year(Date)) %>% summarise("CHF, XAU" = cor(CHF, XAU, use = "complete.obs"))
mergeall %>% summarise("CHF, XAU" = cor(CHF, XAU, use = "complete.obs"))

#correlation for 2007-2018 is gibberish because it layers the yearly correlation on top of each other
#3d plots?

#new file below

##############################################################################
#Support, resistance made into hist bins


##############################################################################
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