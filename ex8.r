library('ISLR')
df <- Auto
#head(df)
df_copy <- na.omit(df)
#1. all except the last are quantitative, the last is qualitative

#2. ranges
data.frame(min=sapply(df[,-9],min),max=sapply(df[,-9],max))

#3. mean and std  
data.frame(mean=sapply(df[,-9],mean),sd=sapply(df[,-9],sd))

#4. visualizations
library('ggcorrplot')
library('ggplot2')

corr <- cor(df[,-9])
ggcorrplot(corr,type = "upper",
           outline.col = "white")

ggplot(df, aes(mpg, year, color=horsepower)) + geom_jitter() + geom_smooth()
ggplot(df, aes(mpg, weight, color=horsepower)) + geom_jitter() + geom_smooth()
ggplot(df, aes(displacement, mpg, color=cylinders)) + geom_jitter() + geom_smooth()
head(df)

library('corrplot')
corrplot(corr)
plot(df$mpg, df$year)
plot(df$mpg, df$weight)
plot(df$mpg, df$displacement)

#We see non trivial correlation matrix. Majority of predictors are correlated with each other
#The separate scatterplots show either positive or negative correlation between the picked variables
#positive: mpg ~ year
#negative: mpg ~ weight, mpg ~ displacement


#5. all variables that are sufficiently correlated will positively impact our prediction.
# The fitted lines on each of the scatterplots show relativiely simple functional relationship
# between mpg and the predictors, so functions can be easily fitted thus providing a robust estimation of mpg





### Instructor's comment: 30/30.

