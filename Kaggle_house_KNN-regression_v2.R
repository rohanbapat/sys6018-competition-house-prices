#### Kaggle competition House

#Rohan Bapat, rb2te 	
#Caitlin Dreisbach, CND2Y ;
#Yi Hao, yh8a

###------------KNN Regression Model------------###
library("RWeka")
library("rminer")
library(RCurl)

# Read traindataset
train.url <- getURL('https://raw.githubusercontent.com/rohanbapat/sys6018-competition-house-prices/master/train.csv')
test.url <- getURL('https://raw.githubusercontent.com/rohanbapat/sys6018-competition-house-prices/master/test.csv')
master.train <- read.csv(text = train.url)
master.test <- read.csv(text = test.url) 

summary(master.train) #NeighborhoodRank has 225 NA's
summary(master.test)  #NeighborhoodRank has 225 NA's
sapply(master.train, function(x) sum(is.na(x)))

#drop NeighborhoodRank variable
master.train <- subset(master.train, select= -NeighborhoodRank)
na_count <-sapply(master.train, function(y) sum(length(which(is.na(y))))); na_count
summary(master.train)

#master.test <- subset(master.test, select= -NeighborhoodRank)
na_count <-sapply(master.test, function(y) sum(length(which(is.na(y))))); na_count
summary(master.test)

#split the train data 50-50 into train and valid

library("caret")
set.seed(500)
inTrain<-createDataPartition(y=master.train$SalePrice, p=0.70, list=FALSE)
train.data<-master.train[inTrain,]
test.data<-master.train[-inTrain,]
nrow(train.data)
nrow(test.data)

##Build and evaluate KNN model using all the variables-- Model 1
model_knn <- IBk(SalePrice~., data=train.data, control=Weka_control(K=10, X=TRUE))
model_knn
summary(model_knn)
evaluate_Weka_classifier(model_knn,test.data, numFolds=10, complexity= FALSE, seed=1, class=TRUE)

#Cross-validation for IBk
model_knn <- IBk(SalePrice~., data=master.train, control=Weka_control(K=15, X=TRUE))
evaluate_Weka_classifier(model_knn,master.train, numFolds=2, complexity= FALSE, seed=1, class=TRUE)

#prediction
prediction_knn <-predict(model_knn, master.test)
summary(prediction_knn)

#write to csv file
kaggle.submission = cbind(test.data$Id, prediction_knn)
colnames(kaggle.submission) = c("Id", "SalePrice")
write.csv(kaggle.submission, file = "kaggle_house_submission3.csv", row.names = FALSE)


##Build and evaluate KNN model using most corelated variables-- Model 2
master.train2 <- master.train[,c('MSZoning','LotFrontage','LotArea','Street','Utilities','Neighborhood','OverallCond','YearBuilt','CentralAir','Fireplaces','GarageArea','GarageCars','PoolArea','YrSold','SaleType','SaleCondition','OverallQual','SalePrice')]
master.pred2 <- master.test[,c('MSZoning','LotFrontage','LotArea','Street','Utilities','Neighborhood','OverallCond','YearBuilt','CentralAir','Fireplaces','GarageArea','GarageCars','PoolArea','YrSold','SaleType','SaleCondition','OverallQual')]

#split the train data 50-50 into train and valid
library("caret")
set.seed(500)
inTrain2<-createDataPartition(y=master.train2$SalePrice, p=0.50, list=FALSE)
train.data2<-master.train2[inTrain2,]
test.data2<-master.train2[-inTrain2,]
nrow(train.data2)
nrow(test.data2)

##Build and evaluate KNN model using all the variables-- Model 1
model_knn2 <- IBk(SalePrice~., data=train.data2, control=Weka_control(K=20, X=TRUE))
model_knn2
summary(model_knn2)
evaluate_Weka_classifier(model_knn,test.data2, numFolds=0, complexity= FALSE, seed=1, class=TRUE)

#Cross-validation for IBk
model_knn2 <- IBk(SalePrice~., data=master.train2)
evaluate_Weka_classifier(model_knn,master.train2, numFolds=0, complexity= FALSE, seed=1, class=TRUE)

#prediction
prediction_knn2 <-predict(model_knn2, master.pred2)
summary(prediction_knn2)

#write to csv file
kaggle.submission2 = cbind(master.test[,'Id'], prediction_knn2)
colnames(kaggle.submission2) = c("Id", "SalePrice")
write.csv(kaggle.submission2, file = "kaggle_house_submission2.csv", row.names = FALSE)

