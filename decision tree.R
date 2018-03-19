library("party")
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)
library(plotly)
library(pROC)
train <- read.csv("train.csv", as.is = TRUE)
valid <- read.csv("valid.csv", as.is = TRUE)

daqi_boost <- 
tree.2 <- ctree(formula = BookingPurchase~., data = train)			# A more reasonable tree
#class(tree.2)  # different class from before
# "constparty" "party"  
png("Conditional_inference_tree.png", width=10,height=5,units="in",res=1200)
plot(tree.2, main = "Conditional inference tree", gp = gpar(fontsize = 6),     # font size changed to 6
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = FALSE, 
       id = TRUE)
)
dev.off()
y <- predict(tree.2, newdata = valid[, -19])

y1 <- data.frame(BookingPurchase = y)
y2 <- data.frame(BookingPurchase = valid$BookingPurchase)
y1$type = 'predicted by Conditional Inference Tree'
y2$type = 'true Value of Validation Set'

y3 <- read.csv("daqi.csv", as.is = TRUE)
y3 <- data.frame(BookingPurchase = y3$pred)
y3$type = 'predicted by XGBoost'

library("Matrix")
library("foreach")
library("glmnet")
set.seed(665)
cv_ridge = cv.glmnet(as.matrix(train[, -19]), train[, 19], alpha=0, nfolds = 10)
y4 <- as.vector(predict(cv_ridge, s=cv_ridge$lambda.min, newx = as.matrix(valid[, -19])))
y4 <- data.frame(BookingPurchase = y4)
y4$type = 'predicted by Ridge Regression'

model_logistic <- glm(BookingPurchase ~ ., data = train, family = binomial)
y5 <- predict(model_logistic, newdata = valid[, -19], type="response")
y5 <- data.frame(BookingPurchase = y5)
y5$type = 'predicted by logistic regression'


Y <- rbind(y1, y2, y4, y5, y3)

p <- ggplot(Y, aes(BookingPurchase, fill = type)) + geom_density(alpha = 0.2) + 
  labs(title="Density of Predicted BookingPurchase by several models")

p <- ggplotly(p)
chart_link = plotly_POST(p, filename="intro-2")

chart_link



y5_int <- as.vector(y5$BookingPurchase)
for(i in 1:length(y5_int)){
  if(y5_int[i] < 0.35){
    y5_int[i] = 0;
  }else{
    y5_int[i] = 1;
  }
}
ridge_auc <- auc(y5_int, valid$BookingPurchase)

party_tree <- ctree(formula = BookingPurchase~., data = train)
png("decision_tree_on_BookingPurchase.png", width=10,height=5,units="in",res=1200)
plot(party_tree)
dev.off()

y <- predict(party_tree, newx = valid[, -13])
y <- as.vector(y)

output <- threshold(valid$BookingPurchase, y)
predicted_y <- output[2][[1]]
best_threshold <- output[1][[1]]
mean(predicted_y != valid$BookingPurchase)
best_threshold
