library(data.table)
library("e1071")
library(mice)
system.time(x <- fread('TripAdvisor.csv', header = T, sep = ',')) 
x <- data.frame(x)
x <- mice(x)
x <-complete(x)
set.seed(0)
ind <- sample(1:nrow(x),size = 5000)
train <- x[ind,]
train <- mice(train)
train <- complete(train)
xnew <- train[,c(3,4,11)]
library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(cluster)
library(StatMatch)
gower_dist <- gower.dist(xnew)
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)



ggplot(aes(x = xnew$osTypeName, y = xnew$p_sessionActivity), data = xnew) +
  geom_point(aes(color = factor(pam_fit$clustering)))

train$agegroup[pam_fit$clustering==1] <- "young"
train$agegroup[pam_fit$clustering==2] <- "medium"
train$agegroup[pam_fit$clustering==3] <- "old"

foo <- sample(1:5000,2500)

model <- svm(BookingPurchase~. , data=train[foo,])
y <- predict(model, newdata  = valid[,-19])

for(i in 1:length(y)){
  if(y[i] < 0.5){
    y[i] = 0;
  }else{
    y[i] = 1;
  }
}
library(pROC)
auc(y,as.numeric(valid[, 19]))
mean(y!= train[-foo,][, 15])
plot(model, data=train[, -15])
