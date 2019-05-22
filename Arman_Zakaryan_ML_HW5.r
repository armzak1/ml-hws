library('caret')
library('ggplot2')
library('ROCR')
library('rpart.plot')
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


eval_model <- function(model, eval_set, verbose=TRUE){
  y_pred_tr <- predict(model, newdata = eval_set, type='prob')
  pred_tr <- prediction(y_pred_tr$T, as.numeric(eval_set$response))
  perf <- performance(pred_tr, 'tpr','fpr')
  if (verbose) {plot(perf)}
  
  y_pred_tr_raw <- predict(model, newdata = eval_set, type='raw')
  cm <- confusionMatrix(y_pred_tr_raw, eval_set$response, positive = 'T') 
  if(verbose){show(cm)}
  
  return(cm$byClass['Pos Pred Value'])
}


set.seed(1)
train_control <- trainControl(method = "cv",
                              number = 10)
tree_model <- train(response ~ ., 
                  data = df_train, 
                  method = "rpart",
                  trControl = train_control)

prp(tree_model$finalModel, extra = 3, type = 4, 
    box.palette = "auto",faclen = 0)

eval_model(tree_model, df_train)
eval_model(tree_model, df_test)

set.seed(1)
forest_model <- train(response ~ ., 
                    data = df_train, 
                    method = "rf",
                    num.threads = 3,
                    ntree = 50)

eval_model(forest_model, df_train)
eval_model(forest_model, df_test)

boost_model <- train(response ~ ., 
                      data = df_train,
                      method = 'adaboost',
                      tuneGrid=expand.grid(nIter=10, method='adaboost'))

eval_model(boost_model, df_train)
eval_model(boost_model, df_test)

#I specified number of iterations, because without doing it, my computer took more than an hour to train
#and it wasn't even done yet. However, even with small number of iterations we observe overfitting.
#So more iterations wouldn't have had any significant effects on the result.

eval_model(tree_model, df_test, verbose=FALSE)
eval_model(forest_model, df_test, verbose=FALSE)
eval_model(boost_model, df_test, verbose=FALSE)

#Comparing these, we see that random forest model yields the best precision out of the 3.
#The other two have pretty similar precisions (~0.5), but significantly lower than random forest.
