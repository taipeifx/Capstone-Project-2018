load 2 files: 
  eur29var
  eurTA
  
eur29var correlations
  ##  Visualizing the dataset using a correlation matrix
  correlations = cor(dataset[,c(2:28)])
  print(head(correlations))
  library(corrplot)
  corrplot(correlations, method="circle")
###################################### create new ec2 instance and install needed R files
  # https://console.aws.amazon.com/ec2/home?region=us-east-1#launchAmi=ami-061a59e27c8da0b93 #this
  
  #RStudio-1.1.456_R-3.5.1_CUDA-9.0_cuDNN-7.2.1_ubuntu-16.04-LTS-64bit (ami-061a59e27c8da0b93)
  
  #dec 6
  #52.90.129.56
  #c4.8xlarge, i-04180f98bd869e640
  #change security group settings
  
  #choose AMI - r5.xlarge 	4 	19 	32 GiB 	EBS Only 	$0.252 per Hour
  #configure instance - request spot instances
  #storage = volume type gp2, size 20 GiB
  #security group - HTTP, port 80  
  #choose existing key pair
  
  #instance launch has been initiated : i-089fbd83eeb31a909
  #IPv4 Public IP : 35.171.193.134
  #Username:	rstudio
  #Password:	<Your instance ID>

#Configuring ec2 instance using pUTTY
# https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/putty.html?icmpid=docs_ec2_console #this, connect using putty
  #ubuntu@ec2-35-171-193-134.compute-1.amazonaws.com
  #top
  #sudo apt-get install r-base-dev   #already using newest version

  #sudo apt-get install -y default-jre  #Install the Java Runtime Environment (JRE).
  #sudo apt-get install -y default-jdk  #Install the Java Development Kit (JDK).
  #sudo R CMD javareconf  #Update where R expects to find various Java files.
  
  #1) sudo rstudio-server stop
  #2) export LD_LIBRARY_PATH=/usr/lib/jvm/jre/lib/amd64:/usr/lib/jvm/jre/lib/amd64/default
  #3) sudo rstudio-server start

  #sudo R  #Install the packages:
  #install.packages("rJava")
  #install.packages("RWeka") 
  #install.packages("FSelector")
  
  #login to R   #IPv4 Public IP : 35.171.193.134 #wget localhost:8787
  #Username:	rstudio
  #Password:	<Your instance ID>
  ############################################################################################### 
  ############################################################################################### 
  ############################################################################################### 


############################################################################################### 

############################################################################################### buckets
#US East (N. Virginia) , us-east-1 , closer to New York


#install.packages("aws.s3")
#library(aws.s3)
#Sys.setenv("AWS_ACCESS_KEY_ID" = "EA6TDV7ASDE9TL2WI6RJ",
#           "AWS_SECRET_ACCESS_KEY" = "OSnwITbMzcAwvHfYDEmk10khb3g82j04Wj8Va4AA",
#           "AWS_DEFAULT_REGION" = "us-east-1")

#bucket_name = "capstoneproject-770851433061"
#save_object("eurTA29varNA.csv", bucket = bucket_name, file = "eurTA29varNA.csv")
#put_object("trees.csv", bucket = bucket_name) #save file to bucket

#library(readr)

############################################################################################### 




############################################################################################### 

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

#example test
dataset1 = dataset[100:110,1:28]
dataset2 = dataset[100:110,1:29]

library(randomForest)
#https://stackoverflow.com/questions/28127429/the-explanation-of-the-verbose-mode-during-running-randomforest-in-r
dataset1.rf <- randomForest(class ~ ., data=dataset, importance=TRUE,
                            proximity=TRUE, do.trace=100)

set.seed(5)
weights = random.forest.importance(class~., dataset, importance.type = 1)
weights2 = random.forest.importance(class~., dataset2, importance.type = 1)
print(weights)

#dataset= dataset[-1]
#write.csv(dataset, "eurTA29varNA.csv", row.names = F)
#datasetNA = data.frame(class,forceindex,WillR5,WillR10,WillR15,RSI5,RSI10,RSI15,ROC5,
#                       ROC10,MOM5,MOM10,ATR5,ATR10, ADX, SAR, STOCH, BBands, MACD)

## Selecting features using the random.forest.importance function from the FSelector package
set.seed(5)
weights = random.forest.importance(class~., dataset, importance.type = 1)
print(weights)
#parallelization
#https://stackoverflow.com/questions/32889499/why-importance-is-affected-after-parallelization-of-randomforest


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