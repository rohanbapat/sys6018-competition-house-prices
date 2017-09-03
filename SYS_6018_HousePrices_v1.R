library("tidyverse")
library(RCurl)

trainURL <- getURL('https://raw.githubusercontent.com/rohanbapat/sys6018-competition-house-prices/master/train.csv') 
train_data <- read.csv(text = trainURL)

train_data2 <- train_data[,c('MSZoning', 'LotFrontage', 'LotArea', 'Street', 'Utilities', 'Neighborhood', 'OverallCond', 'YearBuilt', 'CentralAir','Fireplaces' ,'GarageArea', 'GarageCars' ,'PoolArea', 'YrSold','SaleType','SaleCondition', 'SalePrice')]


train_data2$LotFrontage[is.na(train_data2$LotFrontage)] = median(train_data2$LotFrontage, na.rm = T)


# Cross validation
train_test_split <- 0.7

cv_sample <- sample(1:nrow(train_data2),round(train_test_split*nrow(train_data2)))

# Create test and train datasets
train1 <- train_data2[cv_sample,]
test1 <- train_data2[-cv_sample,]

lin_model <- lm(SalePrice~., train_data2)

pred_test1 <- predict(lin_model, newdata = test1)



testURL <- getURL('https://raw.githubusercontent.com/rohanbapat/sys6018-competition-house-prices/master/test.csv') 
test_data <- read.csv(text = testURL)
test_data2 <- test_data[,c('MSZoning', 'LotFrontage', 'LotArea', 'Street', 'Utilities', 'Neighborhood', 'OverallCond', 'YearBuilt', 'CentralAir','Fireplaces' ,'GarageArea', 'GarageCars' ,'PoolArea', 'YrSold','SaleType','SaleCondition')]
test_data2$LotFrontage[is.na(test_data2$LotFrontage)] = median(test_data2$LotFrontage, na.rm = T)
test_data2$GarageArea[is.na(test_data2$GarageArea)] = median(test_data2$GarageArea, na.rm = T)
test_data2$GarageCars[is.na(test_data2$GarageCars)] = median(test_data2$GarageCars, na.rm = T)
test_data2$MSZoning[is.na(test_data2$MSZoning)] = "RL"
test_data2$Utilities[is.na(test_data2$Utilities)] = "AllPub"
test_data2$SaleType[is.na(test_data2$SaleType)] = "WD"

pred_test1 <- predict(lin_model, newdata = test_data2)
submission_df <- cbind(test_data['Id'], pred_test1)
colnames(submission_df) <- c("Id", "SalePrice")
submission_df$SalePrice[submission_df$SalePrice<0]=median(submission_df$SalePrice)
write.csv(submission_df, file = "rb2te_house_price.csv", row.names = F)






