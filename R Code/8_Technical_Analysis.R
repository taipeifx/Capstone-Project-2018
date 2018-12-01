
#https://www.quantinsti.com/blog/predictive-modeling-algorithmic-trading
#17. tech analysis

library(quantmod); library(TTR); library(caret);library(corrplot);library(pROC);library(FSelector);

## Use set.seed function to ensure the results are repeatable
set.seed(5)

## Read the stock and index data
eur= read_csv("eur16var.csv") #4448014 obs
eurTA = eur

## Compute the price change for the stock and classify as UP/DOWN
price = eurTA$Close-eurTA$Open
class = ifelse(price > 0,"UP","DOWN") #all indicators are lagged against class

## Compute the various technical indicators that will be used 
# Force Index Indicator
#The Force Index is an indicator that uses price and volume to assess the power 
#behind a move or identify possible turning points
forceindex = (eurTA$Close - eurTA$Open) * eurTA$EURVolume 
forceindex = c(NA,head(forceindex,-1)) #returns forceindex excluding the Close variable

# Buy & Sell signal Indicators (Williams R% and RSI)
# Williams R%, momentum indicator that moves between 0 and -100 and measures overbought and oversold levels
# The relative strength index (RSI) is a momentum indicator that measures the magnitude of recent price changes 
# to evaluate overbought or oversold conditions (Welles Wilder)
WillR5  = WPR(eurTA[,c("High","Low","Close")], n = 5) ; WillR5 = c(NA,head(WillR5,-1)) ;
WillR10 = WPR(eurTA[,c("High","Low","Close")], n = 10) ; WillR10 = c(NA,head(WillR10,-1)) ;
WillR15 = WPR(eurTA[,c("High","Low","Close")], n = 15) ; WillR15 = c(NA,head(WillR15,-1)) ;

RSI5  = RSI(eurTA$Close, n = 5,maType="WMA") ;RSI5 = c(NA,head(RSI5,-1)) ;
RSI10 = RSI(eurTA$Close, n = 10,maType="WMA") ;RSI10 = c(NA,head(RSI10,-1)) ;
RSI15 = RSI(eurTA$Close, n = 15,maType="WMA") ;RSI15 = c(NA,head(RSI15,-1)) ;

# Price change Indicators (ROC and Momentum)
# The Rate of Change (ROC) indicator measures the percentage change of the current price 
# as compared to the price a certain number of periods ago
# MOM is a leading indicator measuring a security's rate-of-change
ROC5 = ROC(eurTA$Close, n = 5,type ="discrete")*100 ; ROC5 = c(NA,head(ROC5,-1)) ;
ROC10 = ROC(eurTA$Close, n = 10,type ="discrete")*100 ; ROC10 = c(NA,head(ROC10,-1)) ;

MOM5 = momentum(eurTA$Close, n = 5, na.pad = TRUE) ; MOM5 = c(NA,head(MOM5,-1)) ;
MOM10 = momentum(eurTA$Close, n = 10, na.pad = TRUE) ; MOM10 = c(NA,head(MOM10,-1)) ;

# Volatility signal Indicator (ATR)
#True range (TR) is a measure of volatility of a High-Low-Close series; average true range (ATR) is a 
#Welles Wilder's style moving average of the TR. Developed by J. Welles Wilder in 1978.
ATR5 = ATR(eurTA[,c("High","Low","Close")], n = 5, maType="WMA")[,1] ; ATR5 = c(NA,head(ATR5,-1)) ;
ATR10 = ATR(eurTA[,c("High","Low","Close")], n = 10, maType="WMA")[,1]; ATR10 = c(NA,head(ATR10,-1)) ;

#additional TA

# Average Directional Movement Index (ADX) was developed in 1978 by J. Welles Wilder as an indicator of 
# trend strength in a series of prices of a financial instrument
ADX = ADX(HLC = eurTA[,c("High","Low","Close")], maType = EMA)
ADX = data.frame(ADX)
ADX$DIp = c(NA, head(ADX$DIp,-1))
ADX$DIn = c(NA, head(ADX$DIn,-1))
ADX$DX = c(NA, head(ADX$DX,-1))
ADX$ADX = c(NA, head(ADX$ADX,-1))

colnames(ADX) #"DIp" "DIn" "DX"  "ADX"

# Parabolic SAR - method devised by J. Welles Wilder, Jr., to find potential reversals in the market price direction
SAR = SAR(eurTA[,c("High","Low")], accel = c(0.02, 0.2))
SAR = c(NA, head(SAR,-1))

#Stochastic Oscillator - The stochastic oscillator is a momentum indicator comparing the closing price of a security 
#to the range of its prices over a certain period of time. The sensitivity of the oscillator to market movements is 
#reducible by adjusting that time period or by taking a moving average of the result.
STOCH = stoch(eurTA[,c("High","Low","Close")], maType = EMA)
STOCH = data.frame(STOCH)
STOCH$fastK = c(NA, head(STOCH$fastK,-1))
STOCH$fastD = c(NA, head(STOCH$fastD,-1))
STOCH$slowD = c(NA, head(STOCH$slowD,-1))

colnames(STOCH) #"fastK" "fastD" "slowD"
colnames(STOCH) = c("Stoch.fastK", "Stoch.fastD", "Stoch.slowD")

#Bollinger Bands
#%B quantifies a security's price relative to the upper and lower Bollinger Band. 
BBands = BBands(eurTA[,c("High","Low","Close")])
BBands = data.frame(BBands)
BBands$dn = c(NA, head(BBands$dn,-1))
BBands$mavg = c(NA, head(BBands$mavg,-1))
BBands$up = c(NA, head(BBands$up,-1))
BBands$pctB = c(NA, head(BBands$pctB,-1))

colnames(BBands) #"dn"   "mavg" "up"   "pctB"
colnames(BBands) = c("BBands.dn", "BBandsm.avg", "BBands.up", "BBands.pctB")

# MACD ,developed by Gerald Appel and is probably the most popular price oscillator
# A nine-day EMA of the MACD, called the "signal line", is then plotted on top of the MACD line 
# which can function as a trigger for buy and sell signals. Traders may buy the security when the 
# MACD crosses above its signal line and sell, or short, the security when the MACD crosses below the signal line.
MACD = MACD(eurTA$Close)
MACD = data.frame(MACD)
colnames(MACD) = c("MACD", "MACD.Signal")

MACD = MACD %>% mutate(MACD.Signal2= ifelse(MACD > MACD.Signal, "Buy", "Sell"))
MACD$MACD = c(NA, head(MACD$MACD ,-1))
MACD$MACD.Signal = c(NA, head(MACD$MACD.Signal,-1))
MACD$MACD.Signal2 = c(NA, head(MACD$MACD.Signal2,-1))


## Combining all the Indicators and the Class into one dataframe
datasetNA = data.frame(class,forceindex,WillR5,WillR10,WillR15,RSI5,RSI10,RSI15,ROC5,
                     ROC10,MOM5,MOM10,ATR5,ATR10, ADX, SAR, STOCH, BBands, MACD)

colnames(datasetNA)[15] =  "ADX.DIp"
colnames(datasetNA)[16] = "ADX.DIn"
colnames(datasetNA)[17] = "ADX.DX"

write.csv(datasetNA, "eurTA29var.csv")
#colnames(datasetNA)["sar"] = "SAR"

dataset = na.omit(datasetNA)

## Understanding the dataset using descriptive statistics
print(head(dataset),5)
dim(dataset)
y = dataset$class
cbind(freq=table(y), percentage=prop.table(table(y))*100)

summary(dataset)

##  Visualizing the dataset using a correlation matrix
correlations = cor(dataset[,c(2:28)])
print(head(correlations))
library(corrplot)
corrplot(correlations, method="circle")

#write.csv(datasetNA, "eurTA29var.csv")
#datasetNA$Date = eur$Date
#datasetNA$Timestamp = eur$Timestamp
# eur = eur[-7]
# eur = eur[-c(3,4,5,7,8,9,11)]
eur1 = eur
eur1 = cbind(eur1,datasetNA)

write.csv(eur1, "eurfinal50var.csv")
#predict up/down, price, range?
#divide into years
##################################################################################################################
##################################################################################################################
##################################################################################################################

## Selecting features using the random.forest.importance function from the FSelector package
set.seed(5)
weights = random.forest.importance(class~., dataset, importance.type = 1)
print(weights)

set.seed(5)
subset = cutoff.k(weights, 10)
print(subset)

## Creating a dataframe using the selected features
dataset_rf = data.frame(class,forceindex,WillR5,WillR10,RSI5,RSI10,RSI15,ROC5,ROC10,MOM5,MOM10Indx)
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