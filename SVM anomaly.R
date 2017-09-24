
# Packages ----------------------------------------------------------------



library("e1071")
library("caret")
library("pROC")


# Data Manipulation -------------------------------------------------------



creditcard$UID <- NULL
prop.table(table(creditcard$Class))
table(creditcard$Class)
data <-  sort(sample(nrow(creditcard), nrow(creditcard)*0.5))
train_df<- creditcard[data,]
validate_df <- creditcard[-data,]

train_df <- as.data.frame(train_df)
validate_df <- as.data.frame(validate_df)

train_df <- subset(train_df, Class == 0)

response <- train_df$Class

train_df <- train_df[sample(nrow(train_df), 50000), ]

train_df$Class <- NULL

validate_df1 <- validate_df
validate_df1$Class <- NULL


# Fitting and predicting --------------------------------------------------


SVM_fit <- svm(train_df, y = NULL,  type='one-classification',nu=0.05, scale=TRUE, kernel="sigmoid")



prediction <- predict(SVM_fit,validate_df1)

prediction <- as.data.frame(prediction)

prediction <- ifelse(prediction == TRUE, 0, 1)

validate_df$pred <- prediction


# Diagnostics -------------------------------------------------------------



table(validate_df$pred, validate_df$Class)

cmatrix <- table(validate_df$pred, validate_df$Class)
cmatrix
precision <- cmatrix[1,1]/sum(cmatrix[1,1:2]) 
precision
recall<-cmatrix[1,1]/sum(cmatrix[1:2,1])
recall
Fscore <- (2*precision*recall)/(precision + recall) 
Fscore 

confusionMatrix(validate_df$pred, validate_df$Class)

auc <- roc(validate_df$pred, validate_df$Class)
print(auc)
plot(auc, ylim=c(0,1),  print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

