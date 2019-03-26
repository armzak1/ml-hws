library('MASS')
df <- Boston

#1. rows are different houses in boston, cols are the given houses' parameters (features)

### Instructor's comment: Not different houses but different suburbs of Boston

nrow(df)
ncol(df)

head(df)
library('corrplot')
corrplot(cor(df))
#very close-to-sparse correlation matrix, meaning most of the features are either uncorrelated or
#correlated very poorly

#2. scatterplots
library('ggplot2')

ggplot(df, aes(indus, dis)) + geom_jitter() + geom_smooth() #negative correlation
ggplot(df, aes(nox, dis)) + geom_jitter() + geom_smooth() #negative correlation
ggplot(df, aes(lstat, medv)) + geom_jitter() + geom_smooth() #negative correlation
ggplot(df, aes(rm, indus)) + geom_jitter() + geom_smooth() #uncorrelated
ggplot(df, aes(age, dis)) + geom_jitter() + geom_smooth() #negative correlation
ggplot(df, aes(nox, indus)) + geom_jitter() + geom_smooth() #positive correlation

#3. correlation plot suggests positive relationship with rad, tax
# negative relationship with black, medv, dis. Others are not significant.

#4. outliers
summary(df)

#we notice significant outliers in crim, zn, medv (the original names are too long to write)
#we detect these outliers by observing the min/max value distance from the median. 
#The rest don't have significantly many outliers.
#These can also be discovered by plotting boxplots.

boxplot(df$crim)
boxplot(df$zn)
boxplot(df$medv)
boxplot(df$tax)

#5. setting bound the charles river
sum(df$chas)

#as 1's indicate setting bound the charles river, and 0 otherwise, summing up the entire column
#will give us the number of 1's in the column





### Instructor's comment: 20/20.