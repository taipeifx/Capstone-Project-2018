#remove NA's
#divide into years
#test in python 

library(readr)
#library(anytime)
#library(tidyquant)
#library(dplyr)

#library(Hmisc)
#library(psych)
setwd("D:/NYCDSA/Project 4 - Capstone Project")

library(quantmod); library(TTR); library(caret);library(corrplot);library(pROC);library(FSelector);

## Use set.seed function to ensure the results are repeatable
set.seed(5)

## Read the stock and index data
dataset= read_csv("eurTA29varNA.csv") #4448014 obs, 29 var

#dataset= dataset[-1]
#write.csv(dataset, "eurTA29varNA.csv", row.names = F)
#datasetNA = data.frame(class,forceindex,WillR5,WillR10,WillR15,RSI5,RSI10,RSI15,ROC5,
#                       ROC10,MOM5,MOM10,ATR5,ATR10, ADX, SAR, STOCH, BBands, MACD)

## Selecting features using the random.forest.importance function from the FSelector package
set.seed(5)
weights = random.forest.importance(class~., dataset, importance.type = 1)
print(weights)

#attr_importance
#forceindex          77.51583
#WillR5              68.87048
#WillR10             74.25381
#WillR15             66.42217
#RSI5                63.80084
#RSI10               66.63375
#RSI15               62.88849
#ROC5                78.03314
#ROC10               69.45223
#MOM5                61.75009
#MOM10               57.27347
#ATR5                56.37970
#ATR10               57.29584
#ADX.DIp             77.67709
#ADX.DIn             67.52814
#ADX.DX              61.79014
#ADX                 48.61381
#SAR                 74.41116
#Stoch.fastK         69.13132
#Stoch.fastD         69.04780
#Stoch.slowD         69.41646
#BBands.dn           76.91887
#BBandsm.avg         75.03613
#BBands.up           74.80274
#BBands.pctB         79.92473
#MACD                83.40559
#MACD.Signal         90.82730
#MACD.Signal2        28.07881
#write.csv(weights, "weights.csv")

############################################################################################### 
#need more CPU
#https://www.r-bloggers.com/interacting-with-aws-from-r/
#https://www.r-bloggers.com/interacting-with-aws-from-r/

#https://blog.sicara.com/speedup-r-rstudio-parallel-cloud-performance-aws-96d25c1b13e2

#https://aws.amazon.com/blogs/big-data/running-r-on-aws/ #If you use R packages such as foreach, parallel, or snow 
#  https://aws.amazon.com/blogs/machine-learning/using-r-with-amazon-sagemaker/
#  https://aws.amazon.com/blogs/big-data/running-r-on-amazon-athena/ 

#buckets
#http://seankross.com/2017/05/02/Access-Amazon-Web-Services-in-R.html
############################################################################################### 
  
set.seed(5)
subset = cutoff.k(weights, 10)
print(subset)
#[1] "MACD.Signal" "MACD"        "BBands.pctB" "ROC5"        "ADX.DIp"     "forceindex"  "BBands.dn"   "BBandsm.avg"
#[9] "BBands.up"   "SAR" 

## Creating a dataframe using the selected features
dataset_rf = dataset[c("class", "MACD.Signal", "MACD", "BBands.pctB", "ROC5", "ADX.DIp", "forceindex", "BBands.dn", "BBandsm.avg",
                       "BBands.up", "SAR")]
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