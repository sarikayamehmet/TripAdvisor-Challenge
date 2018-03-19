# Accuracy is not the metric to use when working with an imbalanced dataset. 
# We have seen that it is misleading.
# There are metrics that have been designed to tell you a more truthful story
# when working with imbalanced classes.

# Confusion Matrix: A breakdown of predictions into a table showing correct predictions
# (the diagonal) and the types of incorrect predictions made (what classes incorrect predictions were assigned).
# https://artax.karlin.mff.cuni.cz/r-help/library/caret/html/confusionMatrix.html
library(caret)
ypred <-   # predicted values
ytrue <-   # true values
confusionMatrix(ypred, ytrue)

# Precision: A measure of a classifiers exactness.

# Recall: A measure of a classifiers completeness
# F1 Score (or F-score): A weighted average of precision and recall.
library (ROCR);

y <- ... # logical array of positive / negative cases
predictions <- ... # array of predictions

pred <- prediction(predictions, y);

# Recall-Precision curve  
# precision = TP / (TP + FP)
# recall = TP / (TP + FN)
RP.perf <- performance(pred, "prec", "rec");

plot (RP.perf);

# ROC curve
ROC.perf <- performance(pred, "tpr", "fpr");
plot (ROC.perf);

# ROC area under the curve
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
# Fscore = (2*Precision*Recall) / sum(Precision, Recall)

# ROC Curves: Like precision and recall, accuracy is divided into sensitivity and specificity 
# and models can be chosen based on the balance thresholds of these values.


# Kappa (or Cohen’s kappa): Classification accuracy normalized by the imbalance of the classes in the data.
library(fmsb)
Kappa.test(ypred, ytrue, conf.level=0.95) 
