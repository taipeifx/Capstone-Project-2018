
# https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/putty.html #windows connect ec2 linux command line
# https://docs.aws.amazon.com/quickstarts/latest/vmlaunch/step-2-connect-to-instance.html#browser #connect using browser
# https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/putty.html?icmpid=docs_ec2_console #this, connect using putty
# sudo apt-get install r-base-dev

# https://www.r-bloggers.com/installing-rjava-on-ubuntu/
#Install the Java Runtime Environment (JRE).
#sudo apt-get install -y default-jre
#Install the Java Development Kit (JDK).
#sudo apt-get install -y default-jdk
#Update where R expects to find various Java files.
#sudo R CMD javareconf
#Install the package.
#> install.packages("rJava")

#THIS https://stackoverflow.com/questions/28462302/libjvm-so-cannot-open-shared-object-file-no-such-file-or-directory
#1) sudo rstudio-server stop
#2) export LD_LIBRARY_PATH=/usr/lib/jvm/jre/lib/amd64:/usr/lib/jvm/jre/lib/amd64/default
#3) sudo rstudio-server start
#library(rJava)

#sudo R
#install.packages("RWeka") / FSelector

#UBUNTU

#https://stackoverflow.com/questions/50439533/cannot-install-package-data-table-on-aws-ec2 #sudo yum install R-devel
#https://stackoverflow.com/questions/45873334/installing-r-packages-on-amazon-linux-ec2-instance #sudo yum install -y R



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

# https://770851433061.signin.aws.amazon.com/console
#1. create IAM USER
#2. S3 Bucket , arn:aws:s3:::capstoneproject-770851433061
  # added files "eurTA29varNA.csv" , "eurfinal50var.csv"
#3. set up an emr cluster

############################################################################################### 

############################################################################################### buckets
#http://seankross.com/2017/05/02/Access-Amazon-Web-Services-in-R.html
#Now let¡¦s test out our big cloud hard drive with the aws.s3 package which you can install with install.packages("aws.s3")
#US East (N. Virginia) , us-east-1 , closer to New York



#install.packages("aws.s3")
#library(aws.s3)
# Set up your keys and your region here.
#Sys.setenv("AWS_ACCESS_KEY_ID" = "EA6TDV7ASDE9TL2WI6RJ",
#           "AWS_SECRET_ACCESS_KEY" = "OSnwITbMzcAwvHfYDEmk10khb3g82j04Wj8Va4AA",
#           "AWS_DEFAULT_REGION" = "us-east-1")

#bucket_name = "capstoneproject-770851433061"
#save_object("eurTA29varNA.csv", bucket = bucket_name, file = "eurTA29varNA.csv")
#put_object("trees.csv", bucket = bucket_name) #save file to bucket

#memory.limit()
#[1] 8103
## To increase the storage capacity
#memory.limit(size=56000)

#library(readr)

##########################################
# Let's put our CSV file in the bucket.
#put_object("trees.csv", bucket = bucket_name)

# We've put data in The Cloud! Now let's get it back on our computer:
#save_object("trees.csv", bucket = bucket_name, file = "trees_s3.csv")

# Are the files the same?
#trees_s3 <- read.csv("trees_s3.csv")
#all.equal(trees, trees_s3)
############################################################################################### 

############################################################################################### emr cluster (actually EC2 and AMI)
#https://www.r-bloggers.com/interacting-with-aws-from-r/
# devtools::install_github("cloudyr/aws.ec2")
# 
# https://console.aws.amazon.com/ec2/home?region=us-east-1#launchAmi=ami-061a59e27c8da0b93 #this

#choose AMI - m4.xlarge # https://aws.amazon.com/ec2/instance-types/ # https://aws.amazon.com/ec2/pricing/on-demand/
   #R is typically RAM bound and so a memory optimized (r family) instance is probably appropriate. 
   #r5.xlarge 	4 	19 	32 GiB 	EBS Only 	$0.252 per Hour
#security group - HTTP, port 80
#storage = volume type gp2, size 20 GiB, #ec2 instance, https://aws.amazon.com/ec2/spot/pricing/  #ebs storage https://aws.amazon.com/ebs/pricing/
#create key pair .pem file 
#instance launch has been initiated : i-0dec7593282ba83fa
#copy and paste the public DNS address or IP address (IPv4 Public IP 100.26.109.141) from the instance properties to your browser 
#and you should receive an RStudio login page. The default login details are:
#Username:	rstudio
#Password:	<Your instance ID>


# Welcome!  This RStudio Amazon AMI contains RStudio Server version 1.1.456,
# running R 3.5.1 on Ubuntu 16.04 LTS.
# Includes support for Shiny (add /shiny/rstudio to URL).
# NEW: experimental support for CUDA 9.0 (incl. cuDNN 7.2.1) and Magma 2.4.0
#      enabling use of GPU packages in R and higher performance for deep
#      learning frameworks such as TensorFlow.
# AMI created by Louis Aslett (http://www.louisaslett.com/).  If you've
# any comments or suggestions please mail louis.aslett@durham.ac.uk

# NOTE: It is *highly* recommended that you immediately change the
# default password for logging into RStudio, which you can do by logging
# in via SSH (recommended) in the usual EC2 fashion.  Alternatively,
# since this AMI was created to make RStudio Server accessible to those
# who are less comfortable with Linux commands you can follow the
# instructions below to change it without touching Linux.

# There is now a mini package where functions to manipulate the server will be
# placed.  This includes a function to change the password.  First load the
# package:
library("RStudioAMI")

# Now you can change the password by just running the following function.  It
# will prompt you to provide the existing password (just the instance ID) and
# then type in a new password
passwd()

# It is strongly recommended that you clear the console so that the password is
# not visible after running the function.  Either press Ctrl+L or go to
# Edit -> Clear Console within the RStudio interface.

# There is also a function to assist with linking to a Dropbox account to
# make loading scripts/data on and off the server much easier.  Just run the
# following function once you are ready to link to your Dropbox and follow the
# instructions
linkDropbox()

# Once Dropbox is linked you will notice a new folder called Dropbox appear in
# the Files pane on the right ===>
# This will begin syncing immediately.  If you have a large Dropbox then it is
# strongly recommended that you selectively sync only what you need.  Use the
# excludeSyncDropbox and includeSyncDropbox functions for this.

############################################################################################### 
############################################################################################### 
############################################################################################### 


#https://blog.sicara.com/speedup-r-rstudio-parallel-cloud-performance-aws-96d25c1b13e2
#Let us stick to the basics :
#  go to Amazon AWS page and create a free account
#click on Services/EC2  

#https://www.r-bloggers.com/instructions-for-installing-using-r-on-amazon-ec2/

### #what about R on EMR - https://aws.amazon.com/blogs/big-data/statistical-analysis-with-open-source-r-and-rstudio-on-amazon-emr/

##https://aws.amazon.com/blogs/big-data/running-r-on-aws/
# choose AMI: When launching an EC2 instance, you must choose an Amazon Machine Image (AMI), which contains all 
#information required to start an instance. For example, an AMI defines which operating system is 
#installed on your EC2 instance and which software is included."""

#choose an instance type
#Choose an EC2 instance type that matches the data size and processing that your analysis requires. 
#graph of recommended instance types -  If you use R packages such as foreach, parallel, or snow, use M4 instance types

#configure instance details
#The tasks can even run scripts for installation after the instance starts







############################################################################################### 

# Verbose = True for log files
# top in putty to see tasks

# jupyter notebook in AWS
# https://docs.aws.amazon.com/dlami/latest/devguide/setup-jupyter.html
# https://towardsdatascience.com/setting-up-and-using-jupyter-notebooks-on-aws-61a9648db6c5













############################################################################################### 
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