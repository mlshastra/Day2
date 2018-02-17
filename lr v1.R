library(MASS)
library(ISLR)
library(ggplot2)


attach(Boston)
names(Boston)


head(Boston)

summary(Boston)

str(Boston)

lm.fit = lm(medv~lstat)

lm.fit

summary(lm.fit)

names(lm.fit)

coef(lm.fit)

confint(lm.fit)

predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval = 'confidence')

predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval = 'prediction')

plot(lstat,medv)
abline(lm.fit)

#par(mfrow=c(2,2))

# let's look at the plots

plot(lm.fit)

plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

boxplot(Boston[,-c(7,10,12)])
qplot(y=lstat,x=1:506, geom = "boxplot") 

qplot(lstat, data = Boston, geom = "density", fill= rad, main="pdf of lstat", xlab="lstat", ylab="Density")

library("GGally")

ggcorr(Boston, label = TRUE)

str(Boston)


abline(lm.fit)
v = ggplot(Boston, aes(lstat,medv))

v + geom_point() + abline(lm.fit)


v + geom_line()

v + geom_point() + aes(color = age, size = dis) + facet_wrap(chas~rad) + theme_grey()

vis_fun <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="purple", color="purple", ...)
  p
}



Boston[,4] = as.factor(Boston[,4])

Boston[,'rad'] = as.factor(Boston[,'rad'])

str(Boston)

qplot(lstat, data = Boston, geom = "density", fill= rad, main="pdf of lstat", xlab="lstat", ylab="Density")

g = ggpairs(Boston,columns = c("lstat","medv"), lower = list(continuous = vis_fun))
g

ggpairs(Boston, columns = c("lstat","medv"))


