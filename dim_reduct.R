library(Rtsne)
library(rgl)
library(FactoMineR)
library(vegan)
library(car)
library(party)
library(FNN)
library(Matrix)
library(foreach)
library(glmnet)
train <- read.csv("train.csv", as.is = TRUE)
valid <- read.csv("valid.csv", as.is = TRUE)

km <- kmeans(train,5,10000)
# run principle component analysis
pc <- prcomp(train)
# plot original data on projection
plot(pc$x[,1], pc$x[,2],col=km$cluster+1,pch=16, xlab = "principle component 1", ylab = "principle component 2")
# plot spiderweb and connect outliners with dotted line
pc_bind<-cbind(pc$x[,1], pc$x[,2])
ordispider(pc_bind, factor(km$cluster), label = TRUE)
ordihull(pc_bind, factor(km$cluster), lty = "dotted")

# plot the third dimension
scatter3d(x = pc$x[,1], y = pc$x[,2], z = pc$x[,3], groups = as.factor(km$cluster),
          surface=FALSE, ellipsoid = TRUE)

# we find that 3 dimension is better than 2 dimension.
# project new data onto the PCA space
projected_train <- data.frame(BookingPurchase = train$BookingPurchase, pc$x)[, 1:4]


install.packages("devtools")
devtools::install_github("drsimonj/corrr")
library(corrr)
airquality %>% correlate() %>% network_plot(min_cor = .2)

# ridge

set.seed(665)

cv_ridge = cv.glmnet(as.matrix(projected_train[, -1]), projected_train[, 1], alpha=1, nfolds = 10)
plot(cv_ridge)

pca_valid <- predict(pc, valid)
pca_valid <- as.data.frame(pca_valid)[, 1:3]

y <- predict(cv_ridge, s=cv_ridge$lambda.min, newx = as.matrix(pca_valid))

for(i in 1:length(y)){
  if(y[i] < 0.2145){
    y[i] = 0;
  }else{
    y[i] = 1;
  }
}
mean(valid[, 19] != y)


