library(randomForest)
library(rpart.plot)
library(rpart)
library(dplyr)
library(caret)
library(ROCR)

set.seed(123)
load(file="data/train.RData")
load(file="data/test.RData")

unnecesary.vars <- c("shooter", "defender", "shot_cat_pct", "shot_difficulty_pct", "position_pct")
train <- select(train, -one_of(unnecesary.vars))
test <- select(test, -one_of(unnecesary.vars))


###################################################
## 1 - DECISION TREE
###################################################

tree <- rpart(success ~ ., data = train, method="class", 
              control = rpart.control(cp = 0.0001, xval = 10))
alpha <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]

tree.pruned <- prune(tree, cp=alpha)
printcp(tree.pruned)

bplot <- barplot(tree.pruned$variable.importance[1:10], main = "Variable importance (Decision tree)", xaxt="n")
labs <- names(tree.pruned$variable.importance[1:10])
text(cex=1, x=bplot-.25, y=-250, labs, xpd=TRUE, srt=45)

predictions.dt <- as.data.frame(predict(tree.pruned, test[,-1]))
(tb <- table(round(predictions.dt$`1`), test[,1]))

(accuracy.dt <- sum(diag(tb)) / sum(tb))
(precision.p <- tb[2,2] / sum(tb[,2]))
precision.n <- tb[1,1] / sum(tb[,1])
(precision.dt <- (precision.p + precision.n) / 2)



###################################################
## 2 - RANDOM FOREST
###################################################

# Grid search to tune model's parameters.
nvars.rf <- seq(3, ncol(train)-1, by=2)
ntrees.rf <- c(100, 500, 1000)
rf.parameters <- expand.grid(ntrees=ntrees.rf, nvars=nvars.rf)

res <- vector("numeric", nrow(rf.parameters))
for(i in 1:length(res)) {
  model.rf <- randomForest(success ~ ., data=train, ntree=rf.parameters[i,1], 
                           mtry=rf.parameters[i,2], proximity=FALSE)
  res[i] <- model.rf$err.rate[rf.parameters[i,1], 1]
}

# Fit the model with the set of optimum parameters.
(best.nvars <- rf.parameters[which.min(res),2])
(best.ntrees <- rf.parameters[which.min(res),1])
model.rf <- randomForest(success ~ ., data=train, ntree=best.ntrees, 
                         mtry=best.nvars, proximity=FALSE, importance=TRUE)

# Variable importance plot.
varImpPlot(model.rf, main="Variable importance (Random Forest)")

# Confusion matrix after using the model to predict the test set.
prediction.rf <- predict(model.rf, newdata=test)
(tb <- table(prediction.rf, test$success))

(accuracy.rf <- sum(diag(tb)) / sum(tb))
(precision.p <- tb[2,2] / sum(tb[,2]))
precision.n <- tb[1,1] / sum(tb[,1])
(precision.rf <- (precision.p + precision.n) / 2)



###################################################
## 3 - COMPARISON
###################################################

pred.dt <- prediction(predictions.dt$`1`, test$success)
pred.rf <- prediction(predict(model.rf, type="prob", newdata=test)[,2], test$success)

perf.dt <- performance(pred.dt, "tpr", "fpr")
perf.rf <- performance(pred.rf, "tpr", "fpr")

plot(perf.dt, col="red", main="ROC Curve comparison")
plot(perf.rf, add = TRUE)
legend(0, 1, legend=c("Decision Tree", "Random Forest"),
       col=c("red", "black"), lty=1:2, cex=1)
