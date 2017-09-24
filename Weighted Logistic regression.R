creditcard1 <- subset(creditcard, Class == 1)
creditcard0 <- subset(creditcard, Class == 0)
creditcard00 <- creditcard0[sample(nrow(creditcard0), 3500), ]
creditcard01 <- rbind(creditcard1, creditcard00)
class(creditcard01)
creditcard01 <- as.data.frame(creditcard01)
prop.table(table(creditcard01$Class))
table(creditcard01$Class)

set.seed(1234)
splitIndex <- createDataPartition(creditcard01$Class, p = .50,
                                  list = FALSE,
                                  times = 1) 
trainSplitc <- creditcard01[ splitIndex,]
testSplitc <- creditcard01[-splitIndex,]
wt <- trainSplitc$Amount


prop.table(table(trainSplitc$Class))
prop.table(table(creditcard$Class))
glm.fit <- glm(Class~., data = trainSplitc, weights = wt, family = binomial)
summary(glm.fit)
glm.fit$coefficients[1] 
glm.probs = predict(glm.fit, newdata = testSplitc, type = "response") 
glm.pred=ifelse(glm.probs>0.5, "1", "0")
glm.pred<-as.numeric(glm.pred)
class(glm.pred)
class(glm.probs)
attach(testSplitc)
table(glm.pred, Class)
mean(glm.pred == Class)
cmatrix <- table(glm.pred, Class)
cmatrix
precision <- cmatrix[1,1]/sum(cmatrix[1,1:2]) 
precision
recall<-cmatrix[1,1]/sum(cmatrix[1:2,1])
recall
Fscore <- (2*precision*recall)/(precision + recall) Fscore confusionMatrix(as.numeric(glm.pred), Class)
kappa(cmatrix)
confusionMatrix(as.numeric(glm.pred), Class)
library(pROC)
auc <- roc(testSplitc$Class, glm.pred)
print(auc)
plot(auc, ylim=c(0,1),  print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)