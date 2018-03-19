library("e1071")

train <- read.csv("train.csv", as.is = TRUE)
valid <- read.csv("valid.csv", as.is = TRUE)

model <- svm(BookingPurchase~. , data=train)
y <- predict(model, newx = valid[, -19])

# for(i in 1:length(y)){
#   if(y[i] < 0.5){
#     y[i] = 0;
#   }else{
#     y[i] = 1;
#   }
# }

plot(model, data=train[, -19])
