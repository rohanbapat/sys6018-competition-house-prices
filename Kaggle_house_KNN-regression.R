#### Kaggle competition House

#Rohan Bapat, rb2te 	
#Caitlin Dreisbach, CND2Y ;
#Yi Hao, yh8a

###------------KNN Regression Model------------###
install.packages("RWeka")
install.packages("rminer")
library("RWeka")
library("rminer")
master.train <- read.csv("SYSmasterfiletrain.csv", header=TRUE, stringsAsFactors=TRUE)
master.test <- read.csv("SYSmasterfiletest.csv", header=TRUE, stringsAsFactors=TRUE)
summary(master.train) #NeighborhoodRank has 225 NA's
summary(master.test)  #NeighborhoodRank has 225 NA's

#drop NeighborhoodRank variable
master.train <- subset(master.train, select= -NeighborhoodRank)
na_count <-sapply(master.train, function(y) sum(length(which(is.na(y))))); na_count
summary(master.train)

master.test <- subset(master.test, select= -NeighborhoodRank)
na_count <-sapply(master.test, function(y) sum(length(which(is.na(y))))); na_count
summary(master.test)

#split the train data 50-50 into train and valid

library("caret")
set.seed(500)
inTrain<-createDataPartition(y=master.train$SalePrice, p=0.50, list=FALSE)
train.data<-master.train[inTrain,]
test.data<-master.train[-inTrain,]
nrow(train.data)
nrow(test.data)

##Build and evaluate KNN model using all the variables-- Model 1
model_knn <- IBk(SalePrice~., data=train.data, control=Weka_control(K=20, X=TRUE))
model_knn
summary(model_knn)
evaluate_Weka_classifier(model_knn,test.data, numFolds=0, complexity= FALSE, seed=1, class=TRUE)

#Cross-validation for IBk
model_knn <- IBk(SalePrice~., data=master.train)
evaluate_Weka_classifier(model_knn,master.train, numFolds=0, complexity= FALSE, seed=1, class=TRUE)

#prediction
prediction_knn <-predict(model_knn, master.test)
summary(prediction_knn)

#write to csv file
kaggle.submission = cbind(test.data$Id, prediction_knn)
colnames(kaggle.submission) = c("Id", "SalePrice")
write.csv(kaggle.submission, file = "kaggle_house_submission.csv", row.names = FALSE)


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
kaggle.submission2 = cbind(test.data$Id, prediction_knn)
colnames(kaggle.submission2) = c("Id", "SalePrice")
write.csv(kaggle.submission2, file = "kaggle_house_submission2.csv", row.names = FALSE)

