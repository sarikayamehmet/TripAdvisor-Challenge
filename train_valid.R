library(data.table)
library("e1071")
library(mice)
system.time(x <- fread('Clean.csv', header = T, sep = ',')) 
x <- data.frame(x)
system.time(x1 <- fread('TripAdvisor.csv', header = T, sep = ',')) 
x1 <- data.frame(x1)
x2<-na.omit(x1)
x$osTypeName <- x1$osTypeName
# reclassify the ostypename
tb <- table(x$osTypeName)
names <- names(tb)
x$osTypeName[grep("android",x$osTypeName)] <- "android"
x$osTypeName[grep("ipad",x$osTypeName)] <- "ipad"
x$osTypeName[grep("other",x$osTypeName)] <- "other"
table(x$osTypeName)
# x <- mice(x)
# x <-complete(x)
table(x$BookingPurchase)
set.seed(0)
ind1 <- which(x$BookingPurchase==0)[sample(1:sum(x$BookingPurchase==0),25000)]
ind2 <- which(x$BookingPurchase==1)[sample(1:sum(x$BookingPurchase==1),25000)]
x <- x[c(ind1,ind2),]
table(x$BookingPurchase)
write.csv(x,file = "clean_new2.csv",row.names = FALSE)
library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(cluster)
library(StatMatch)
xnew <- train[,c(3,21)]

gower_dist <- gower.dist(xnew)
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)


# ggplot(aes(x = xnew$osTypeName, y = xnew$p_sessionActivity), data = xnew) +
#   geom_point(aes(color = factor(pam_fit$clustering)))
# 
train$agegroup[pam_fit$clustering==1] <- 0
train$agegroup[pam_fit$clustering==2] <- 1
train$agegroup[pam_fit$clustering==3] <- 2
train$agegroup[pam_fit$clustering==1] <- 0
x$agegroup[pam_fit$clustering==2] <- 1
x$agegroup[pam_fit$clustering==3] <- 2
# foo <- sample(1:n,n/2)
# 
# model <- svm(BookingPurchase~. , data=train[foo,])
# y <- predict(model, newx = train[-foo,][, -15])
# 
# for(i in 1:length(y)){
#   if(y[i] < 0.5){
#     y[i] = 0;
#   }else{
#     y[i] = 1;
#   }
# }
# 
# mean(y!= train[-foo,][, 15])
# plot(model, data=train[, -15])
# # 0.2022 for n=20000
x<-x[,-c(1,21)]

ind <- sample(1:50000,50000*0.8)
train <- x[ind,]
valid <- x[-ind, ]
write.csv(train,file = "train.csv",row.names = FALSE)
write.csv(valid,file = "valid.csv",row.names = FALSE)
