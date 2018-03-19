library(Rtsne)
library(ggplot2)

x <- read.csv("TripAdvisor5.csv", as.is = TRUE)
c <- names(x)[grep("p_+", names(x))] # previous

summary(x[,names(x) %in% c])

table(x$p_trafficChannel)
x[x$p_trafficChannel=="A","p_mytrafficChannel"] <- as.numeric(1)
x[x$p_trafficChannel=="B","p_mytrafficChannel"] <- as.numeric(2)
x[x$p_trafficChannel=="C","p_mytrafficChannel"] <- as.numeric(3)
x[x$p_trafficChannel=="D","p_mytrafficChannel"] <- as.numeric(4)
x[x$p_trafficChannel=="H","p_mytrafficChannel"] <- as.numeric(5)
x[x$p_trafficChannel=="O","p_mytrafficChannel"] <- as.numeric(6)
x <- x[,-which(names(x) %in% c("p_trafficChannel"))]
colnames(x)[17] <- "p_trafficChannel"
names(x)

c <- c[!c %in% c("p_TotalPrice")]

n <- c(3,4,5,6,7,9,11,14)
set.seed(123)
tsne.1 = Rtsne(as.matrix(x[,names(x) %in% c]), check_duplicates=FALSE, 
               pca=TRUE, perplexity=50, theta=0.1, dims=2, max_iter=500)
tsne.1.f = as.data.frame(tsne.1$Y)
ggplot(tsne.1.f, aes(x=V1, y=V2)) + geom_point(size=0.25, colour=x$BookingPurchase+1) + 
  ggtitle("check_duplicates=FALSE,pca=TRUE, perplexity=50, theta=0.1, dims=2, max_iter=500")
  
tsne.2 = Rtsne(as.matrix(x[,n]), check_duplicates=FALSE, 
               pca=TRUE, perplexity=50, theta=0.1, dims=2, max_iter=500)

tsne.2.f = as.data.frame(tsne.2$Y)
ggplot(tsne.2.f, aes(x=V1, y=V2)) + geom_point(size=0.25, colour=x$BookingPurchase+1) + 
  ggtitle("check_duplicates=FALSE,pca=TRUE, perplexity=50, theta=0.1, dims=2, max_iter=500")
