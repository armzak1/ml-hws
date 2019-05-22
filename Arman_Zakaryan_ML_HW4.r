library('caret')
library('ggplot2')
library('ROCR')

df = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(df) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")

df <- na.omit(df)

df$response <- factor(df$response, labels=c('F', 'T'))

set.seed(1)
train_idx <- createDataPartition(df$response, p=0.8, list=FALSE)
df_train <- df[train_idx,]
df_test <- df[-train_idx,]

#LDA
lda_model = train(response ~ ., 
                       data = df_train, 
                       method = "lda")

y_pred <- predict(lda_model, newdata = df_train, type='prob')
lda_pred <- prediction(y_pred$T, as.numeric(df_train$response))

perf <- performance(lda_pred,'tpr','fpr')
plot(perf)
performance(lda_pred, 'auc')@y.values
#AUC is 0.824

#QDA
qda_model = train(response ~ ., 
                  data = df_train, 
                  method = "qda")

y_pred <- predict(qda_model, newdata = df_train, type='prob')
qda_pred <- prediction(y_pred$T, as.numeric(df_train$response))

perf <- performance(qda_pred,'tpr','fpr')
plot(perf)
performance(qda_pred, 'auc')@y.values
#AUC is 0.9

#Naive Bayes
nb_model = train(response ~ ., 
                  data = df_train, 
                  method = "nb")

y_pred <- predict(nb_model, newdata = df_train, type='prob')
nb_pred <- prediction(y_pred$T, as.numeric(df_train$response))
perf <- performance(nb_pred,'tpr','fpr')
plot(perf)
performance(nb_pred, 'auc')@y.values
#AUC is 0.79

#Logistic Regression
lr_model = train(response ~ ., 
                 data = df_train, 
                 method = "glm",
                 family = 'binomial')

y_pred <- predict(lr_model, newdata = df_train, type='prob')
lr_pred <- prediction(y_pred$T, as.numeric(df_train$response))

perf <- performance(lr_pred,'tpr','fpr')
plot(perf)
performance(lr_pred, 'auc')@y.values
#AUC is 0.825

#kNN
set.seed(1)
train_control <- trainControl(method = "cv",
                             number = 10)
knn_model = train(response ~ ., 
                 data = df_train, 
                 method = "knn",
                 trControl = train_control,
                 tuneGrid = expand.grid(k=seq(1, 30, 1)))
plot(knn_model)
knn_model$bestTune
#best k=23
#predict function automatically takes the best model

y_pred <- predict(knn_model, newdata = df_train, type='prob')
knn_pred <- prediction(y_pred$T, as.numeric(df_train$response))
perf <- performance(knn_pred, 'tpr','fpr')
plot(perf)
performance(knn_pred, 'auc')@y.values
#AUC is 0.66


# The question doesn't specify on based on which AUC we should pick the best model, so I'll check AUC on test data,
# in order to avoid any results caused by overfitting

models = list(lda_model, qda_model, nb_model, lr_model, knn_model)

for(model in models){
  y_pred_tr <- predict(model, newdata = df_train, type='prob')
  pred_tr <- prediction(y_pred_tr$T, as.numeric(df_train$response))
  cat(model$method, 'Train-AUC', performance(pred_tr, 'auc')@y.values[[1]], '\n')
  
  y_pred <- predict(model, newdata = df_test, type='prob')
  pred <- prediction(y_pred$T, as.numeric(df_test$response))
  cat(model$method, 'Test-AUC', performance(pred, 'auc')@y.values[[1]], '\n\n')
  
}

#Notice that QDA had the highest train AUC, but it was due to overfitting.
#LDA model is more stable, and has the best test AUC, so we will consider it the best model out of these 5.

y_pred <- predict(lda_model, newdata = df_test, type='raw')
confusionMatrix(y_pred, df_test$response, positive = 'T')

#Accuracy is 0.795, Balanced Accuracy is 0.748, Sensitivity is 0.63 and Precision (same as Pos Pred Value) is 0.66
