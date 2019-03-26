#Ex.1
set.seed(1)
x <- rnorm(100, 0, 1)
e <- rnorm(100, 0, 0.25)

### Instructor's comment: Standard deviation=0.5 not 0.25, variance=0.25

y <- -1 + 0.5*x + e

plot(x,y)
#positive linear relationship
model <- lm(y ~ x)
summary(model)
#b0_hat and b1_hat are pretty close to the original coefficients
#p-values are both <0.05 so we can reject the null hypothesis that
#the b_hats are zero

plot(x,y) 
abline(model$coefficients, col='blue')
abline(c(-1,0.5), col='red')
legend('topleft', legend=c('LS Line', 'Pop. regression line'), col=c('blue', 'red'), lty=c(1,1), cex=0.75)

model2 <- lm(y ~ x + I(x^2))
summary(model2)
#p value for the coefficient of the quadratic term is >0.05,
#so we must accept the null hypothesis that b2_hat is zero
#which means that the linear model was a better fit for our data

set.seed(42)
e_noisy <- rnorm(100,0,1)
y_noisy <- -1 + 0.5*x + e_noisy

set.seed(42)
e_less_noisy <- rnorm(100,0,0.01)
y_less_noisy <- -1 + 0.5*x + e_less_noisy

set.seed(42)
model_noisy <- lm(y_noisy ~ x)
model_less_noisy <- lm(y_less_noisy ~ x)

confint(model)
confint(model_noisy)
confint(model_less_noisy)
#the noisier e gets, the wider is the confidence interval
#--------------------------------------------------------

### Instructor's comment: Use set.seed(1) in random functions

#Ex2.
library('ISLR')
df <- Auto

set.seed(42)
model <- lm(mpg~horsepower, df)
summary(model)
#There seems to be a negative relationship between the variables

### Instructor's comment: p-value for horsepower <2*10^{-16}, that is why there is a relation.

#The relationship is pretty decent, as R^2 of 0.6 indicates that the model
#explains about 60% of the variability around the mean

predict(model, data.frame('horsepower'=98),
        interval='confidence')
predict(model, data.frame('horsepower'=98),
        interval='prediction')

plot(df$horsepower, df$mpg)
abline(model$coefficients, col='blue')

par(mfrow=c(2,2))
plot(model)
#residuals vs fitted values yields a non-straight line, indicating non-linear
#relationship between the predictor and target. The rest of the plots seem ok
#The plots yield some outliers, but no high leverage points
dev.off()

### Instructor's comment: Use plot(predict(), rstudent()) or plot(fitted.values(), rstudent()) and
### plot(hatvalues()) for identifying outliers and high leverage points.
#--------------------------------------------------------

#Ex.3
df <- Carseats

model <- lm(Sales ~ Urban + Price + US, data=df)
summary(model)
#For US and Price the null hypothesis can be rejected due to <0.05 p-values

model2 <- lm(Sales ~ Price + US, data=df)
summary(model2)
#the models perform extremely similarly, as they yield almost equal R^2 and RSS
#adjusted r^2 is slightly better in the second case

confint(model2)

par(mfrow=c(2,2))
plot(model2)
#non constant variance of residuals
#Some outliers, no high leverage points
#the rest is ok

### Instructor's comment: Use plot(predict(), rstudent()) or plot(fitted.values(), rstudent()) and
### plot(hatvalues()) for identifying outliers and high leverage points.

dev.off()

#--------------------------------------------------------
#Ex.4
library('ISLR')

df <- Auto
pairs(df)
library('corrplot')
corr <- cor(df[,-9])
corrplot(corr)

df_num <- df[,-9]
model <- lm(mpg ~ ., data=df_num)
summary(model)
#theres a relation, high r^2 indicates that

### Instructor's comment: F-stat.=252.4>>1 and corresponding p-value<2.2*10^{-16} implies that there is a relationship.

#displacement, weight, year, origin are significantly related
#coefficient of year shows how much the target would increase 
#when the year is increased by 1, keeping all other variables constant 

par(mfrow = c(2, 2))
plot(model)
#slightly non linear relationship suggested by residuals vs fitted plot
#slightly non constant residual variance
#some outliers, no leverage points
#QQ-plot suggests that the distribution of residuals is almost normal,
#with some skewness
dev.off()

### Instructor's comment: Use plot(predict(), rstudent()) or plot(fitted.values(), rstudent()) and
### plot(hatvalues()) for identifying outliers and high leverage points.

model <- lm(mpg ~ displacement:year + origin, data=df_num)
summary(model)
#displacement:year has <0.05 p value => stat. significant

### Instructor's comment: The hierarchical principle states that if we include an interaction in a model, we should
### also include the main effects, even if the p-values associated with their coefficients are not significant. 

model <- lm(mpg ~ year*origin*displacement + weight, data=df_num)
summary(model)
#year*origin*displacement has >0.05 p value => not significant

model <- lm(mpg ~ year*origin + displacement + weight, data=df_num)
summary(model)
#year*origin has <0.05 p value => stat. significant

model <- lm(log(mpg) ~ ., data=df_num)
summary(model)

model <- lm(sqrt(mpg) ~ ., data=df_num)
summary(model)

model <- lm(mpg^2 ~ ., data=df_num)
summary(model)

hist(df$mpg)
hist(log(df$mpg))
hist(df$mpg ^ 2)

#as we see, taking the log of mpg takes care of the skewness of the target variable
#making the distribution closter to normal, and it increases the R^2 of the model
#taking the square of mpg on the other hand, skews the data even more, thus 
#decreasing the R^2


dev.off()





### Instructor's comment 83/100.
