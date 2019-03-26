library(caret)

#ex. 1
set.seed(1)
x <- rnorm(100)
e <- rnorm(100, 0, 0.25)
y <- 1 + 2*x + 3*x^2 + 4*x^3 + e

df <- data.frame(y,x)
set.seed(1)
inds <- createDataPartition(df$x, p = 0.8, list=FALSE)
df_train <- df[inds,]
df_test <- df[-inds,]

#the glmnet performs a mix of ridge and lasso regression, and the coefficient alpha is the mixing weight
#for ridge alpha=0
#for lasso alpha=0

### Instructor's comment: For the Lasso alpha=1.

#ex.2
set.seed(1)
ridge_model <- train(y ~ poly(x, 10), data = df_train,
                     method = 'glmnet',
                     trControl = trainControl(method = 'cv', number = 10),
                     tuneGrid = expand.grid(alpha = 0,
                                            lambda = seq(0.01, 2, by = 0.05)))

### Instructor's comment: For using x, x^2, ... insert raw=TRUE in poly(x,10), otherwise you get the
### orthogonalized polynomials of degree 1 to 10 over the specified set of points x (see ?poly for details).

plot(ridge_model)
ridge_model$bestTune
coef(ridge_model$finalModel, ridge_model$bestTune$lambda)
getTrainPerf(ridge_model)

pred_ridge <- predict(ridge_model, newdata = df_test)
cat('TestMAE:', mean(abs(df_test$y - pred_ridge)))

#Ridge regression uses all the features by giving them non-zero coefficients. The optimal value for lambda is 0.61.
#The model doesn't seem to be overfitted, as the ratio of train/test MAEs is not huge. Overall the model provides 
#sufficient results.

#ex.3
set.seed(1)
lasso_model <- train(y ~ poly(x, 10), data = df_train,
                     method = 'glmnet',
                     trControl = trainControl(method = 'cv', number = 10),
                     tuneGrid = expand.grid(alpha = 1,
                                            lambda = seq(0.001, 0.2, by = 0.005)))

### Instructor's comment: For using x, x^2, ... insert raw=TRUE in poly(x,10), otherwise you get the
### orthogonalized polynomials of degree 1 to 10 over the specified set of points x (see ?poly for details).

plot(lasso_model)
lasso_model$bestTune
coef(lasso_model$finalModel, lasso_model$bestTune$lambda)
getTrainPerf(lasso_model)

pred_lasso <- predict(lasso_model, newdata = df_test)
cat('TestMAE:', mean(abs(df_test$y - pred_lasso)))

#Lasso regression uses only the first the features (degree of our original polynomial).
#The optimal value for lambda is 0.016 which is very small, so the regularization term doesn't have that big of an impact.
#Very high R^2 may be an indicator of overfitting, but the test error is considerably smaller than in case of ridge regression,
#so using this model will be a better choice.
#Overall the model provides very good results.

#Overall, the lasso regression works better, as it only considers the features that are necessery for making predictions.
#All the metrics of lasso regression yield better results. As in case of lasso only the actually necessery features are used,
#there isn't much use of regularization, as overfitting doesn't occur. In case of ridge, model uses all features, 
#the number of which is greater than the degree of original polynomial, so overfitting is a risk, hence bigger lambda.





### Instructor's comment: 90/100.
