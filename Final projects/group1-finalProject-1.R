## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats


library(tidyverse) # metapackage with lots of helpful functions
library(data.table)
train = fread("C:\\Users\\zackw\\Documents\\Classes\\Intelligent Data Analytics\\Project\\train_v2.csv", header = TRUE,
              colClasses=c(fullVisitorId='character'),drop=c('hits'))

#print('train:'); dim(train)
#str(train)
#head(train, n=1)

library(jsonlite)
train$customDimensions<-gsub("[]","[{}]",train$customDimensions,fixed=TRUE)
train$customDimensions<-gsub("'","\"",train$customDimensions,fixed=TRUE)
train$customDimensions<-gsub("[","",train$customDimensions,fixed=TRUE)
train$customDimensions<-gsub("]","",train$customDimensions,fixed=TRUE)
train$customDimensions <- factor(train$customDimensions)
#head(train$customDimensions)

train.customDimensions <- fromJSON(paste('[',paste(train$customDimensions,collapse = ','),']'),flatten = TRUE)
train.customDimensions <- data.frame(apply(train.customDimensions, 2,function(x){as.factor(x)}))
#head(train.customDimensions)

train$device<-gsub("\"\"","\"",train$device,fixed=TRUE)
train.device <- fromJSON(paste('[',paste(train$device,collapse = ','),']'),flatten = TRUE)
train.device <- data.frame(apply(train.device, 2,function(x){as.factor(x)}))
#head(train.device)

train$geoNetwork<-gsub("\"\"","\"",train$geoNetwork,fixed=TRUE)
train.geoNetwork <- fromJSON(paste('[',paste(train$geoNetwork,collapse = ','),']'),flatten = TRUE)
train.geoNetwork <- data.frame(apply(train.geoNetwork, 2,function(x){as.factor(x)}))
#head(train.geoNetwork)

train$totals<-gsub("\"\"","\"",train$totals,fixed=TRUE)
train.totals <- fromJSON(paste('[',paste(train$totals,collapse = ','),']'),flatten = TRUE)
train.totals <- data.frame(apply(train.totals, 2,function(x){as.numeric(x)}))
#head(train.totals)

train$trafficSource<-gsub("\"\"","\"",train$trafficSource,fixed=TRUE)
train.trafficSource <- fromJSON(paste('[',paste(train$trafficSource,collapse = ','),']'),flatten = TRUE)
train.trafficSource <- data.frame(apply(train.trafficSource, 2,function(x){as.factor(x)}))
#head(train.trafficSource)

train_data <- cbind(train,train.customDimensions,train.device,train.geoNetwork,train.totals,train.trafficSource)
train_data <- train_data[,-c(2,4,6,8,9)]
train_data$date <- as.Date(as.character(train_data$date), format= "%Y%m%d")
class(train_data$visitStartTime) <- c('POSIXt','POSIXct')
train_data$channelGrouping <- as.factor(train_data$channelGrouping)
train_data$fullVisitorId <- as.factor(train_data$fullVisitorId)
train_data$socialEngagementType <- as.factor(train_data$socialEngagementType)
train_data$visitId <- as.factor(train_data$visitId)
#print(dim(train_data))
#str(train_data)
#head(train_data)

####################################################################################################################
# find missingness in data
rm(list = setdiff(ls(), "train_data"))
dataSet = train_data
rm(list = setdiff(ls(), "dataSet"))

# find missingness in data
propMissing = function (x) mean(is.na(x))
propNaInData = function (x) (x == as.factor("not available in demo dataset"))
propNotSet = function (x) (x == as.factor("(not set)"))

# create target "revenueCondition"
dataSet$revenueCondition = 0
dataSet$revenueCondition [!is.na(dataSet$transactionRevenue)] = 1 
dataSet$revenueCondition = as.factor(dataSet$revenueCondition)

# replace NA in transaction and transaction revenue with 0
dataSet$transactions[is.na(dataSet$transactions)] = 0
dataSet$transactionRevenue [is.na(dataSet$transactionRevenue)] = 0


# convert all "not available in demo dataset" to NA
naAsString =  apply(dataSet,2,propNaInData)
naAsString = naAsString == TRUE  
dataSet[naAsString] = NA

# convert all "(not set)" to NA
naNotSet = apply(dataSet, 2, propNotSet)
naNotSet = naNotSet == TRUE
dataSet[naNotSet] = NA  

rm(list = c("naAsString","naNotSet"))

# find all variables with only 2 unique values
numUnique = function (x) (length(unique(x)))
uniquePerVar = apply(dataSet, 2, numUnique)
which(uniquePerVar == 2)



# convert new visits to a factor
dataSet$newVisits = ifelse(is.na(dataSet$newVisits), "REGULAR","NEW")
dataSet$newVisits = as.factor(dataSet$newVisits)

dataSet$bounces = ifelse(is.na(dataSet$bounces), "UNBOUNCED","BOUNCED")
dataSet$bounces = as.factor(dataSet$bounces)



cor(dataSet$transactionRevenue, dataSet$totalTransactionRevenue)
# 0.87 

missing = apply(dataSet,2, propMissing)
deleteVar = as.integer(which(missing > 0.4))

# remove variables with >.4 missing
dataSet2 = dataSet
dataSet2[,deleteVar] = NULL

rm(list = "dataSet")

# move fullVisitorId to new variable. remove visitId
fullVisitorId = dataSet2$fullVisitorId
dataSet2$fullVisitorId = NULL
dataSet2$visitId = NULL

dataSet2$index = NULL
dataSet2$visitStartTime = NULL

# Channel grouping has information in source and medium. Take out source and medium
dataSet2$source = NULL
dataSet2$medium = NULL

dataSet2$country = NULL # too many factor levels
dataSet2$networkDomain = NULL
dataSet2$value = NULL # subContinent has same info

dataSet2$weekDays = as.factor(weekdays(dataSet2$date))
dataSet2$month = as.factor(month(dataSet2$date))
dataSet2$year = as.factor(year(dataSet2$date))
dataSet2$date = NULL

dataSet2$socialEngagementType = NULL # has one factor level
dataSet2$isMobile = NULL # same info in device category

# form new data frame with all positive revenue and random sample of non-positive revenue 40 % of non positive values
rowPos = which(dataSet2$revenueCondition == "1")
rowNonPos = which(dataSet2$revenueCondition == "0")

# change revenue condition (target) to 1 and 0 
dataSet3 = dataSet2
rm(list = "dataSet2")


set.seed(7)
rowNonPos = sample(rowNonPos, round(0.04*length(rowNonPos))) # 40 % of zero revenue

# use 70% of new samples for train , 30% for test
trainRowPos = sample(rowPos, round(0.7*length(rowPos)))
testRowPos = setdiff(rowPos, trainRowPos)

trainRowNonPos = sample(rowNonPos, round(0.7*length(rowNonPos)))
testRowNonPos = setdiff(rowNonPos, trainRowNonPos)

rowTrain = c(trainRowPos, trainRowNonPos)
rowTest = c(testRowPos, testRowNonPos)

# create train and test
idTrain = fullVisitorId[rowTrain]
train = dataSet3[rowTrain,]

idTest = fullVisitorId[rowTest]
test = dataSet3[rowTest,]


# treat missing values
# train
apply(train, 2, propMissing)

# remove rows with missing continent
train = train[!is.na(train$continent),]
train$browser = NULL
train$transactionRevenue = NULL
train$transactions = NULL
train$visits = NULL # has one unique value

# table(train$operatingSystem)
# Mode is windows. Impute missing values with mode
train$operatingSystem[is.na(train$operatingSystem)] = train$operatingSystem[12]
train$pageviews[is.na(train$pageviews)] = median(train$pageviews, na.rm = T)

# test
test = test[!is.na(test$continent),]
test$browser = NULL
test$transactionRevenue = NULL
test$transactions = NULL
test$visits = NULL # has one unique value

apply(test, 2, propMissing)

# table(train$operatingSystem)
# Mode is windows. Impute missing values with mode
test$operatingSystem[is.na(test$operatingSystem)] = test$operatingSystem[6]
test$pageviews[is.na(test$pageviews)] = median(test$pageviews, na.rm = T)

train = as.data.frame(train)
test = as.data.frame(test)
#################################################################################################################
train$X = NULL
train$revenueCondition = as.factor(train$revenueCondition)
train$revenueCondition = relevel(train$revenueCondition, ref = "1")
train$weekDays = as.factor(train$weekDays)
train$month = as.factor(train$month)
train$year = as.factor(train$year)
test$weekDays = as.factor(test$weekDays)
test$month = as.factor(test$month)
test$year = as.factor(test$year)
test$X = NULL
test.omit = test[!test$subContinent == "Polynesia",]
testTarget = as.factor(test.omit$revenueCondition)
test$revenueCondition = relevel(test$revenueCondition, ref = "1")
test.omit$revenueCondition = NULL
test = test.omit
#################################################################################################################

#logistic regression
library(ipflasso)
x = data.matrix(train[,-which(colnames(train) == "revenueCondition")])
y = data.matrix(train[,which(colnames(train) == "revenueCondition")])
cv.glmModel = cvr.glmnet(x, y, family = "binomial", nfolds = 5, ncv = 5)#train and tune
lambdaOpt = cv.glmModel$lambda[which.min(cv.glmModel$cvm)]#find optimal lambda
lambdaOpt
ggplot(data = data.frame(MAE = cv.glmModel$cvm, lambda = cv.glmModel$lambda), aes(x = lambda, y = MAE)) + geom_point()#plot

glm.model = glmnet(x = x,y = y, lambda = lambdaOpt, family = "binomial", alpha = 0)#Plot with lambda
glm.predict = predict(object = glm.model, newx = data.matrix(test.omit), type = "class")#predict
glm.predict
library(caret)
confusionMatrix(as.factor(glm.predict),as.factor(testTarget))
#################################################################################################################
library(ggplot2)
library(rpart)         # CART algorithm
library(party)         # to print trees
library(partykit)      # to print trees using "party"
library(rattle)        # for graphics     
library(DMwR)
library(caret)
library(ROCR)
library(ipred)
library(caret)

#decision trees
#In this part for predicting our revnue= reveueConitin, we used Rpart package to grow our tree. 
#The blow code grows a tree from a root that has all the observations, splitting binary to reduce the impurity of its nodes, 
#until some stopping rule is met. I set up this rules with the rpart.control.  
#cp = 0.0001 is the minimum factor of decreasing lack of fit for a split to be attempted and xval = 20 is number of cross validation.


fitDT<-rpart(revenueCondition ~.,  data = train,        
             parms=list(split="information"),   
             control=rpart.control(cp=0.001), xval = 20) 
fitDT$cptable
fitDT


### Tuning

#The result of our tree it is quite big because we use a very small cp. 
#While we prefer a simpler tree, since they are less likely to over fit the data. 
#So for having a smaller tree we need to prune our tree. The best parameter we found from the plot was 0.0078 based on the blew plot


plotcp(fitDT)


#.
#Now you can see that our tree is nicer than the first one. 
#Starting from the top as you can see the "pageview" is on the top of the root and 
#it is the important one based on variance importance.


pruneDT = prune(fitDT, cp = 0.0078)
pruneDTparty = as.party(pruneDT)
fancyRpartPlot(pruneDT,cex=0.6,uniform=TRUE,
               main="Pruned Tree")


### Confusion Matrix


#Now we can see the result of our confusion matrix that shows that our accuracy is high 94% and kappa also is around 85.


tree.predict = predict(pruneDT, newdata=test, type="class")
tree.ConfMat = confusionMatrix(tree.predict, testTarget,positive = "1")
#################################################################################################################
#random forest
library(rpart)         # CART algorithm
library(party)         # to print trees
library(partykit)      # to print trees using "party"
library(rattle)        # for graphics
library(ipred)         # for bagging and error estimation
library(randomForest)  # for Random Forests

mtry = round(sqrt(ncol(train)))
ntrees = 100
rf.model = randomForest(revenueCondition ~., data = train, importance = T, ntrees  = numTrees, mtry = 7)

plot(rf.model)


predTrain = rf.model$predicted
rfmodel.conf =  confusionMatrix(predTrain, train$revenueCondition, positive = "1")
rfmodel.conf

test2 = test
testTarget = test$revenueCondition
test2$revenueCondition = NULL

predTest = predict(rf.model, newdata = test2, type = "class")
testmodel.conf = confusionMatrix(predTest, testTarget, positive = "1")
testmodel.conf


#################################################################################################################
#gboost
library(xgboost)


p = list(objective = "binary:logistic", eval_metric = "rmse", booster = "gbtree")

boost.cv = xgb.cv(data = data.matrix(train[,-which(colnames(train) == "revenueCondition")]),
                  label = data.matrix(train[,which(colnames(train) == "revenueCondition")]),
                  params = p,
                  max_depth = 100,
                  nrounds = 1000,
                  early_stopping_rounds = 100,
                  nfold = 5
)

gboost.model = xgboost(data = data.matrix(train[,-which(colnames(train) == "revenueCondition")]), 
                       label = data.matrix(train[,which(colnames(train) == "revenueCondition")]),
                       params = p,
                       nrounds = boost.cv$best_ntreelimit,
                       max.depth = 100)

gboost.prob = predict(gboost.model, data.matrix(test))
gboost.class = as.factor(as.numeric(gboost.prob > .5))

gboost.ConfMat = confusionMatrix(gboost.class,testTarget,positive = "1")
gboost.ConfMat