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

lda_model = train(response ~ ., 
                  data = df_train, 
                  method = "lda")

y_pred <- predict(lda_model, newdata = df_train, type='prob')
lda_pred <- prediction(y_pred$T, as.numeric(df_train$response))

#ROC Curve/AUC
perf <- performance(lda_pred,'tpr','fpr')
plot(perf)
performance(lda_pred, 'auc')@y.values

#Precision-Recall Curve/AUC
perf <- performance(lda_pred,'prec','rec')
plot(perf)


#x <- perf@x.values[[1]]
#y <- perf@y.values[[1]]
#y[1] = 1


f <- approxfun(data.frame(x , y))
auc <- integrate(f, 0, 1)$value
auc
##################################################
##################################################
##################################################

mygrid = expand.grid(k = seq(1,100,by=2))

train_control = trainControl(method = "cv", 
                             number = 10,
                             verbose = T,
                             classProbs = T,
                             summary = prSummary)

model_knn_class = train(Risk ~ ., 
                        data = train_Data, 
                        method = "knn", 
                        trControl = train_control,
                        tuneGrid = mygrid,
                        metric = "Precision")

#   Default ->   metric = "Accuracy"
#   Default  -> metric = "Kappa"


#### metric = "Sens" (Sensitivity) with 
#### summary = twoClassSummary but only
####  with classProbs = T

#### metric = "Precision" with summary = prSummary only 
####  with classProbs = T and
#### after installing the package = "MLmetrics"

