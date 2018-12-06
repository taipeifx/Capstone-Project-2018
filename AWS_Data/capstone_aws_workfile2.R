colnames(eur) 
#[1] "Date"          "Timestamp"     "Close"         "Bar"           "UpCount"       
# "DownCount"     "Real.Range"    "Neutral.Range" "Body"         
#[10] "TopWick"       "BottomWick"    "EURVolume"     "JPYClose"      "JPYRange"      
# "JPYVolume"     "CHFClose"      "CHFRange"      "CHFVolume"    
#[19] "XAUClose"      "XAURange"      "XAUVolume"     

# "and Technical Indicators"                                                     "class"         "forceindex"    "WillR5"        "WillR10"       "WillR15"       "RSI5"         
#[28] "RSI10"         "RSI15"         "ROC5"          "ROC10"         "MOM5"          "MOM10"         "ATR5"          "ATR10"         "ADX.DIp"      
#[37] "ADX.DIn"       "ADX.DX"        "ADX"           "SAR"           "Stoch.fastK"   "Stoch.fastD"   "Stoch.slowD"   "BBands.dn"     "BBandsm.avg"  
#[46] "BBands.up"     "BBands.pctB"   "MACD"          "MACD.Signal"   "MACD.Signal2" 

#merge neutral and real.range
library(dplyr)
eur <- eur %>% select(Date, everything())
eur <- eur %>% select(Bar, everything())

#head(new_df, n = 2)
eur1 = eur

eur1[is.na(eur1$UpCount),]["UpCount"] = 0
eur1[is.na(eur1$DownCount),]["DownCount"] = 0
eur1$Body = abs(eur1$Body)
eur1$TopWick = abs(eur1$TopWick)
eur1$BottomWick = abs(eur1$BottomWick)
eur1$CHFRange = abs(eur1$CHFRange)
eur1$JPYRange = abs(eur1$JPYRange)
eur1$XAURange = abs(eur1$XAURange)



eur1 = eur1[-c(5,6,7,9:12,15,16)]
eur1 = eur1[-3]
colnames(eur1)
#[1] "Date"       "Timestamp"  "Range"      "Close"      "UpCount"    "DownCount"  "Body"      
#[8] "TopWick"    "BottomWick" "EURVolume"  "JPYClose"   "JPYRange"   "JPYVolume"  "CHFClose"  
#[15] "CHFRange"   "CHFVolume"  "XAUClose"   "XAURange"   "XAUVolume" 

eur1$Range = c(eur1$Range[2:nrow(eur1)], NA) #this
#everything is lagged against both bar and range

colnames(eur1)[3] = "EURM1Range"

#TEST$UNIT[is.na(TEST$UNIT)] <- as.character(TEST$STATUS[is.na(TEST$UNIT)])
#test = eur[c(1,2)]
#test$Bar = c(NA, head(test$Bar,-1))
#test$Bar = c(test$Bar[2:nrow(test)], NA) #this

##  Visualizing the eur1 (eur) using a correlation matrix
dataset23 = eur1[-c(1,2)]
dataset23 = na.omit(dataset23)

correlations2 = cor(dataset23)
print(head(correlations2)) #library(corrplot)
corrplot(correlations2, method="circle",title = "2007-2018, CorrPlot of EURUSD M1 Bar Range w/ Minute Lagged Variables", mar=c(0,0,2,0))

put_object("eurcorrplot.png", bucket = bucket_name) #save file to bucket


#merge eur and datasetTA

datasetTA2 = datasetTA
datasetTA2[1:4448013,] = datasetTA2[2:4448014,]
eur_data = cbind(eur1,datasetTA2)
eur_data = eur_data[1:4448013,]
eur_data2 = eur_data[-20]
eur_data2 = eur_data2[34:nrow(eur_data2),]

##  Visualizing corr_all using a correlation matrix
corr_all = eur_data2[-c(1,2)]
corr_all = na.omit(corr_all)
correlations3 = cor(corr_all[,c(1:44)])
corrplot(correlations3, method="circle",title = "2007-2018, CorrPlot of All Variables", mar=c(0,0,2,0))
put_object("eurcorrplotall.png", bucket = bucket_name) #save file to bucket

##eur_data2 is the cleaned dataset
write.csv(eur_data2, "eurfinaldec5.csv", row.names = F)
put_object("eurfinaldec5.csv", bucket = bucket_name) #save file to bucket

#install.packages("lubridate")
library("lubridate")
eurusd2007 = eur_data2[year(eur_data2$Date) == 2007,]
eurusd2008 = eur_data2[year(eur_data2$Date) == 2008,]
eurusd2009 = eur_data2[year(eur_data2$Date) == 2009,]
eurusd2010 = eur_data2[year(eur_data2$Date) == 2010,]
eurusd2011 = eur_data2[year(eur_data2$Date) == 2011,]

eurusd2012 = eur_data2[year(eur_data2$Date) == 2012,]
eurusd2013 = eur_data2[year(eur_data2$Date) == 2013,]
eurusd2014 = eur_data2[year(eur_data2$Date) == 2014,]
eurusd2015 = eur_data2[year(eur_data2$Date) == 2015,]
eurusd2016 = eur_data2[year(eur_data2$Date) == 2016,]

eurusd2017 = eur_data2[year(eur_data2$Date) == 2017,]
eurusd2018 = eur_data2[year(eur_data2$Date) == 2018,]

##  Visualizing a correlation matrix
corr_year = eurusd2018[-c(1,2)]
corr_year = na.omit(corr_year)
correlations_year = cor(corr_year[1:44])
corrplot(correlations_year, method="circle",title = "Correlation Plot 2018", mar=c(0,0,2,0))

put_object("corrplot2009.png", bucket = bucket_name) #save file to bucket
put_object("corrplot2010.png", bucket = bucket_name) #save file to bucket
put_object("corrplot2011.png", bucket = bucket_name) #save file to bucket
put_object("corrplot2007.png", bucket = bucket_name) #save file to bucket
put_object("corrplot2008.png", bucket = bucket_name) #save file to bucket


## take log
eurlog = eur_data2
eurlog[c(3,4,5:46)] = log(eurlog[c(3,4,5:46)]+1)

corr_year = eur_data3[-c(1,2)]
corr_year = na.omit(corr_year)
correlations_year = cor(corr_year)
corrplot(correlations_year, method="circle",title = "New CorrPlot", mar=c(0,0,2,0))

############# keep needed columns 
eur_data3 = eur_data2[-c(20:30, 36:38, 44:47)]
eur_data3 = eur_data3[-c(25,26)]
corr_year = eur_data3[-c(1,2)]
corr_year = na.omit(corr_year)
correlations_year = cor(corr_year)
corrplot(correlations_year, method="circle",title = "New CorrPlot", mar=c(0,0,2,0))


eurusd2007 = eur_data3[year(eur_data3$Date) == 2007,]
eurusd2008 = eur_data3[year(eur_data3$Date) == 2008,]
eurusd2009 = eur_data3[year(eur_data3$Date) == 2009,]
eurusd2010 = eur_data3[year(eur_data3$Date) == 2010,]
eurusd2011 = eur_data3[year(eur_data3$Date) == 2011,]

eurusd2012 = eur_data3[year(eur_data3$Date) == 2012,]
eurusd2013 = eur_data3[year(eur_data3$Date) == 2013,]
eurusd2014 = eur_data3[year(eur_data3$Date) == 2014,]
eurusd2015 = eur_data3[year(eur_data3$Date) == 2015,]
eurusd2016 = eur_data3[year(eur_data3$Date) == 2016,]

eurusd2017 = eur_data3[year(eur_data3$Date) == 2017,]
eurusd2018 = eur_data3[year(eur_data3$Date) == 2018,]


###
write.csv(eurusd2007, "eurusd2007.csv", row.names = F)
write.csv(eurusd2008, "eurusd2008.csv", row.names = F)
write.csv(eurusd2009, "eurusd2009.csv", row.names = F)
write.csv(eurusd2010, "eurusd2010.csv", row.names = F)
write.csv(eurusd2011, "eurusd2011.csv", row.names = F)

write.csv(eurusd2012, "eurusd2012.csv", row.names = F)
write.csv(eurusd2013, "eurusd2013.csv", row.names = F)
write.csv(eurusd2014, "eurusd2014.csv", row.names = F)
write.csv(eurusd2015, "eurusd2015.csv", row.names = F)
write.csv(eurusd2016, "eurusd2016.csv", row.names = F)

write.csv(eurusd2017, "eurusd2017.csv", row.names = F)
write.csv(eurusd2018, "eurusd2018.csv", row.names = F)
###

library(dplyr)
#eur %>% group_by(Date) %>% summarise(max(High))
#eur %>% group_by(year(Date)) %>% summarise(max(High)) #can use lubridate year() to group
#look = eur %>% group_by(Date, hour(Timestamp)) %>% summarise(max(High))
#look = eur %>% group_by(Date) %>% summarise(max(High))

#put_object("capstone_aws_workfile.R", bucket = bucket_name)
#put_object("eur29var.csv", bucket = bucket_name, file = "eur29var.csv")
#########################################################################################
#########################################################################################
#########################################################################################

#get all years before running any random forests
eurusd2007m = eurusd2007  #by month first
eurusd2007m = eurusd2007m[month(eurusd2007m$Date) == 1,] #33120 obs
eurusd2007m = eurusd2007m[-c(3,4)]
eurusd2007m = eurusd2007m[-c(4,5)] #barup/down missing too many values?

eurusd2007mtest = eurusd2007m[1:50,]
## Selecting features using the random.forest.importance function from the FSelector package
set.seed(5)
#eurusd2007mtest$Bar = as.factor(eurusd2007mtest$Bar)
#class(eurusd2007mtest$Bar)
#table(factor(eurusd2007mtest$Bar))

weights = random.forest.importance(Bar~., eurusd2007mtest, importance.type = 1)
print(weights)
########################finished testing

#########################################################################################
#########################################################################################
#########################################################################################
#get all years before running any random forests
eurusd2007run = eurusd2007
eurusd2007run = eurusd2007run[-c(3,4)]

## Selecting features using the random.forest.importance function from the FSelector package
set.seed(5)

#upcount down count as factor?
#eurusd2007run$UpCount = as.factor(eurusd2007run$UpCount)
#eurusd2007run$DownCount = as.factor(eurusd2007run$DownCount)
#eurusd2007run$Bar = as.factor(eurusd2007run$Bar)
#eurusd2007run$UpCount = eurusd2007run$UpCount.

#eurusd2007run$UpCount = as.numeric(eurusd2007run$UpCount)
eurusd2007run[is.na(eurusd2007run$UpCount),]["UpCount"] = 0
#eurusd2007run$DownCount = as.numeric(eurusd2007run$DownCount)
eurusd2007run[is.na(eurusd2007run$DownCount),]["DownCount"] = 0


weights = random.forest.importance(Bar~., eurusd2007run, importance.type = 1)
print(weights)

set.seed(5)
subset = cutoff.k(weights, 10)
print(subset)

## Creating a dataframe using the selected features
dataset_rf = data.frame(class,forceindex,WillR5,WillR10,WillR15,RSI5,RSI10,RSI15,ROC5,
                        ROC10,MOM5,MOM10,ATR5,ATR10, ADX, SAR, STOCH, BBands, MACD)
dataset_rf = na.omit(dataset_rf)

# Resampling method used - 10-fold cross validation 
# with "Accuracy" as the model evaluation metric.
trainControl = trainControl(method="cv", number=10)
metric = "Accuracy"

## Trying four different Classification algorithms
# k-Nearest Neighbors (KNN)
set.seed(5)


fit.knn = train(class~., data=dataset_rf, method="knn", 
                metric=metric, preProc=c("range"),trControl=trainControl)

# Classification and Regression Trees (CART)
set.seed(5)
fit.cart = train(class~., data=dataset_rf, method="rpart", 
                 metric=metric,preProc=c("range"),trControl=trainControl)

# Naive Bayes (NB)
set.seed(5)
fit.nb = train(class~., data=dataset_rf, method="nb", 
               metric=metric, preProc=c("range"),trControl=trainControl)

# Support Vector Machine with Radial Basis Function (SVM)
set.seed(5)
fit.svm = train(class~., data=dataset_rf, method="svmRadial", 
                metric=metric,preProc=c("range"),trControl=trainControl)

## Evaluating the algorithms using the "Accuracy" metric
results = resamples(list(KNN=fit.knn,CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(results)
dotplot(results)

## Tuning the shortlisted algorithm (KNN algorithm)
set.seed(5)
grid = expand.grid(.k=seq(1,10,by=1))
fit.knn = train(class~., data=dataset_rf, method="knn", metric=metric, tuneGrid=grid,
                preProc=c("range"), trControl=trainControl)
print(fit.knn)
