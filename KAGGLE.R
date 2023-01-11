data = read.csv("analysisData.csv")

# deleting useless data
data <- data[,c(17:19,24,25,37:44,46:69,72:83,85:91)]
apply(is.na(data), 2, sum)

# deleting useless data
data <- data[,c(3:6,8:13,15,20:45,48:56)]
apply(is.na(data), 2, sum)

# deleting useless data
data <- data[,c(-22,-23,-37)]
# deleting missing value
data <- na.omit(data)


# true or false transforming
for(i in c(1:4,35:38)){
  data[,i] <- factor(data[,i],levels = c("t","f"),labels = c(1,0))
}


for (i in 1:ncol(data)){
  if (is.numeric(data[,i])==FALSE&is.factor(data[,i])==FALSE){
    print(names(data)[i])
  }
}

# transforming variable
data$room_type <- factor(data$room_type,
                         levels = c("Entire home/apt","Hotel room","Private room","Shared room"),
                         labels = c(1,2,3,4))

data$bed_type <- factor(data$bed_type,
                         levels = c("Airbed","Couch","Futon","Pull-out Sofa","Real Bed"),
                         labels = c(1,2,3,4,5))


for (i in 1:ncol(data)){
  data[,i] <- as.numeric(data[,i])
}

data <- na.omit(data)

# split data
library(caret)
set.seed(61710)
split = createDataPartition(y = data$price, p = 0.8, list = F, groups = 50)
train = data[split,]
test = data[-split,]


train.data <- train[,-11]
test.data <- test[,-11]
train.label <- train[,11]
test.label <- test[,11]

# random forest
library(randomForest)
set.seed(617)
# train
forest = randomForest(price~., data=train, ntree = 1000)
forest2 = randomForest(price~., data=train, ntree = 1500) 


pred_train = predict(forest, newdata=train)
rmse_train_forest = sqrt(mean((pred_train-train$price)^2))
rmse_train_forest

pred.forest = predict(forest, newdata=test)
rmse_forest = sqrt(mean((pred.forest-test$price)^2))
rmse_forest

pred_train2 = predict(forest2, newdata=train)
rmse_train_forest2 = sqrt(mean((pred_train2-train$price)^2))
rmse_train_forest2
pred.forest2 = predict(forest2, newdata=test)
rmse_forest2 = sqrt(mean((pred.forest2-test$price)^2))
rmse_forest2

library(xgboost)
set.seed(123)
# xgboost
tune_nrounds = xgb.cv(data=as.matrix(train.data), label = train$price, eta=0.1,gamma=0.5,
                      nrounds=500, nfold = 5, verbose = 0, subsample = 0.7)

ggplot(data=tune_nrounds$evaluation_log, aes(x=iter, y=test_rmse_mean)) +
  geom_point(size=0.4, color='sienna') + geom_line(size=0.1, alpha=0.1) + theme_bw()

which.min(tune_nrounds$evaluation_log$test_rmse_mean)
# setup xgboost
xgboost.model= xgboost(data=as.matrix(train.data), label = train$price,
                       nrounds=33, verbose = 0)

# testing rmse
pred_train = predict(xgboost.model, newdata=as.matrix(train.data))
rmse_train_xgboost = sqrt(mean((pred_train - train$price)^2))
rmse_train_xgboost
# testing rmse
pred.xgboost = predict(xgboost.model, newdata=as.matrix(test.data))
rmse_xgboost = sqrt(mean((pred.xgboost - test$price)^2))
rmse_xgboost


xgboost.model2 <- xgboost(data = data.matrix(train.data), 
                          label = train$price, eta = 0.05,gamma=0.1,
                          max_depth = 8, nround=500, 
                          objective="reg:squarederror",
                          print_every_n=50, subsample = 0.6)


pred_train2 = predict(xgboost.model2, newdata=as.matrix(train.data))
rmse_train_xgboost2 = sqrt(mean((pred_train2 - train$price)^2))
rmse_train_xgboost2

pred.xgboost2 = predict(xgboost.model2, newdata=as.matrix(test.data))
rmse_xgboost2 = sqrt(mean((pred.xgboost2 - test$price)^2))
rmse_xgboost2

# combine models
pred.test <- (pred.forest2+pred.xgboost2)/2
rmse = sqrt(mean((pred.test - test$price)^2))
rmse



scoringData <- read.csv("scoringData.csv")
id <- scoringData$id 
scoringData <- scoringData[,names(data)[-11]] 

# true or false transformation
for(i in c(1:4,34:37)){
  scoringData[,i] <- factor(scoringData[,i],levels = c("t","f"),labels = c(1,0))
}

for (i in 1:ncol(scoringData)){
  if (is.numeric(scoringData[,i])==FALSE&is.factor(scoringData[,i])==FALSE){
    print(names(scoringData)[i])
  }
}

# other variable transformation
scoringData$room_type <- factor(scoringData$room_type,
                         levels = c("Entire home/apt","Hotel room","Private room","Shared room"),
                         labels = c(1,2,3,4))

scoringData$bed_type <- factor(scoringData$bed_type,
                        levels = c("Airbed","Couch","Futon","Pull-out Sofa","Real Bed"),
                        labels = c(1,2,3,4,5))

for (i in 1:ncol(scoringData)){
  scoringData[,i] <- as.numeric(scoringData[,i])
}

apply(is.na(scoringData), 2, sum)

# Filling up missings
scoringData$beds[is.na(scoringData$beds)] <- median(scoringData$beds,na.rm=TRUE)
scoringData$host_is_superhost[is.na(scoringData$host_is_superhost)] <- 1
scoringData$host_has_profile_pic[is.na(scoringData$host_has_profile_pic)] <- 1
scoringData$host_identity_verified[is.na(scoringData$host_identity_verified)] <- 1

apply(is.na(scoringData), 2, sum)

# Predictions
pred.forest.submit <- predict(forest2, newdata=scoringData)
pred.xgboost.submit <- predict(xgboost.model2, newdata=as.matrix(scoringData))
pred <- (pred.forest.submit+pred.xgboost.submit)/2


submissionFile = data.frame(id=id, price=pred)
write.csv(submissionFile, 'submission.csv',row.names = F)




