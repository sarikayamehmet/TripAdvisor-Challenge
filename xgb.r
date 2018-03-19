

library(xgboost)
library(ROCR)
library(pROC)
setwd("C:/Users/Daqi/Dropbox/TripAdvisorChallenge")
train <- read.csv("train.csv", as.is = TRUE)
valid <- read.csv("valid.csv", as.is = TRUE)
HalfClean <- read.csv("HalfClean.csv",as.is = TRUE)
x <- read.csv("Clean.csv",as.is = TRUE)



s <- sample(1:nrow(x),0.8*nrow(x),replace=TRUE)
xtrain <- x[s,] 
xtest <- x[-s,]

ytrain <- scale(xtrain[,-15])

ytest <- scale(xtest[,-15])
start.time <- Sys.time()

bstSparse <- xgboost(data = as.matrix(train), label=train[,15] ,max.depth = 30, nthread = 10, nround = 10, objective = "reg:logistic",weight = 4*train[,15]+1)
end.time <- Sys.time()

time.taken <- end.time - start.time
time.taken

pred <- predict(bstSparse, as.matrix(valid))

err <- rep(NA,101)
m <- seq(0, 1, 0.01)
for(i in 1:101){
prediction <- as.numeric(pred > m[i])
err[i] <- mean(as.numeric(pred> m[i]) != xtest$BookingPurchase)
err[i]
}
which.min(err)


m <- seq(0, 1, 0.01)
for(i in 30:90){
predict1 <- as.numeric(pred > m[i])
auc=auc(predict1,as.numeric(xtest$BookingPurchase))
print(auc)
}
which.min(auc)

predict1 <- as.numeric(pred > 0.23)
pred_new=prediction(prediction,as.numeric(test$BookingPurchase))
perf=performance(pred_new,"tpr","fpr")
plot(perf)
auc(pred,valid$BookingPurchase)
```



Half <- HalfClean[HalfClean$user_id == train$user_id,]
library(randomForest)
library(mice)
miceMod <- mice(Half[,!names(Half) %in% "BookingPurchase"], method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

HalfClean$age[HalfClean$os == 5] <- 3
HalfClean$age[HalfClean$os == 2 | HalfClean$os == 6] <- 2
HalfClean$age[HalfClean$os == 1 | HalfClean$os == 3 | HalfClean$os == 4] <- 1
s <- sample(1:nrow(HalfClean),50000,replace = FALSE)
xx <- HalfClean[s,]
miceMod <- mice(xx[,!names(xx) %in% "BookingPurchase"], method="rf") 
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

write.csv(miceOutput,"imputation.csv",row.names = FALSE)


miceOutput$BookingPurchase <- xx$BookingPurchase

set.seed(665)
news <- sample(1:50000,40000,replace = FALSE)
newtrain <- miceOutput[news,]
newtest <- miceOutput[-news,]


write.csv(newtrain,"train.csv",row.names = FALSE)
write.csv(newtest,"test.csv",row.names = FALSE)

library(rocr)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
mm <- cbind(pred,valid$BookingPurchase)
write.csv(mm,"daqi.csv",col.names = FALSE)

x <- read.csv('TripAdvisor.csv', header = T, sep = ',')
library(ggplot2)
x$osTypeName[grep("android",x$osTypeName)] <- "android"
x$osTypeName[grep("ipad",x$osTypeName)] <- "ipad"
x$osTypeName[grep("other",x$osTypeName)] <- "other"
table(x$osTypeName)


qplot(x$osTypeName, geom="histogram", fill=I("pink"), xlab = x$osTypeName, col=I("grey"), main ="Histogram of Operating systems",
      stat = "count")



