
# Initialization ----------------------------------------------------------

library(h2o)
library(randomForest)

localH2O = h2o.init()
set.seed(1234)
creditcard <- na.omit(creditcard)
creditcard1 <- creditcard[sample(nrow(creditcard), 10000), ]
anyNA(creditcard1)
prop.table(table(creditcard1$Class))
set.seed(1234)
splitIndex <- createDataPartition(creditcard1$Class, p = .5,
                                  list = FALSE,
                                  times = 1)
train_df <- creditcard1[ splitIndex,]
validate_df <- creditcard1[-splitIndex,]


train_df <- as.data.frame(train_df)
validate_df <- as.data.frame(validate_df)

table(train_df$Class)
table(validate_df$Class)

outcome_name <- 'Class'
feature_names <- setdiff(names(creditcard1), outcome_name)



# Autoencoder -------------------------------------------------------------

train.hex<-as.h2o(train_df, destination_frame="train.hex")
validate.hex <- as.h2o( validate_df,destination_frame="validate.hex" )

train.dl <-  h2o.deeplearning(x = feature_names, training_frame = train.hex,
                              autoencoder = TRUE,
                              reproducible = T,
                              seed = 1234,
                              hidden = c(18,16,14,11,18), epochs = 2)


train.anon = h2o.anomaly(train.dl,train.hex, per_feature=FALSE)


head(train.anon)
err <- as.data.frame(train.anon)
train_df1 <- train_df
train_df1$err <- err

plot(err$Reconstruction.MSE, main='Reconstruction Error')
MSE <- as.data.frame(err$Reconstruction.MSE)

train_df_anomaly <- train_df1[train_df1$Class == 1, ]

creditcard11 <- creditcard[creditcard$Class == 1, ]
table(train_df$Class)
train_df_auto <- train_df1[err$Reconstruction.MSE >= 0.05,]




# Predict -----------------------------------------------------------------
class(validate.hex)
prediction <- h2o.predict(train.dl, newdata = validate.hex, per_feature=TRUE)

validate.anon <- h2o.anomaly(prediction ,validate.hex, per_feature=FALSE)

plot(train.dl)



