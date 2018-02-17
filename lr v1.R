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

# let's look at some plots to help us

library("GGally")

ggcorr(Boston, label = TRUE)


v = ggplot(Boston, aes(lstat,medv))

v + geom_point()

v + geom_point() + aes(color = age)

v + geom_point() + aes(color = age, size = dis)

v + geom_point() + aes(color = age, size = dis) + facet_grid(.~rad) 

v + geom_point() + aes(color = age, size = dis) + facet_grid(chas~rad) 

v + geom_point() + aes(color = age, size = dis) + facet_wrap(chas~rad) + theme_grey()


boxplot(Boston[,-c(7,10,12)])

qplot(lstat, data = Boston, geom = "density", fill= rad, main="pdf of lstat", xlab="lstat", ylab="Density")

Boston[,4] = as.factor(Boston[,4])

Boston[,'rad'] = as.factor(Boston[,'rad'])

str(Boston)

qplot(lstat, data = Boston, geom = "density", fill= rad, main="pdf of lstat", xlab="lstat", ylab="Density")

## ggpairs

vis_fun <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="purple", color="purple", ...)
  p
}


g = ggpairs(Boston,columns = c("lstat","medv"), lower = list(continuous = vis_fun))
g

ggpairs(Boston, columns = c("lstat","medv"))

# some more modeling
lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

# Interaction Terms
summary(lm(medv~lstat*age,data=Boston))

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))


# Advertising and Sales relationship

Advertising=read.csv("Advertising.csv", header=TRUE); 
newdata=Advertising[,-1]
attach(newdata)
names(newdata)
pairs(newdata)


lm.fit=lm(Sales~TV,data=Advertising) 
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit) 


lm.fit=lm(Sales~TV+Radio+Newspaper,data=Advertising)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

lm.fit1=lm(Sales~Newspaper,data=Advertising)
summary(lm.fit1)

lm.fit2=lm(Sales~Newspaper+TV,data=Advertising)
summary(lm.fit2)

lm.fit3=lm(Sales~Newspaper+TV+Radio,data=Advertising)
summary(lm.fit3)

lm.fit4=lm(Sales~TV+Radio,data=Advertising)
summary(lm.fit4)

lm.fit=lm(Sales~TV*Radio,data=Advertising)
summary(lm.fit)

lm.fit=lm(Sales~TV*Radio,data=Advertising)
summary(lm.fit)

#################
Credit=read.csv("Credit.csv", header=TRUE); 
head(Credit); 
newdata=Credit [,-1]
fix(newdata)
names(newdata)
pairs(newdata[,c(1, 2, 4, 5, 6, 7)])

lm.fit=lm(Balance~Gender,data=Credit)
summary(lm.fit); contrasts(Credit$Gender)

lm.fit=lm(Balance~Ethnicity,data=Credit)
summary(lm.fit)

lm.fit=lm(Balance~Gender,data=Credit)
summary(lm.fit); contrasts(Credit$Gender)

lm.fit=lm(Balance~Ethnicity,data=Credit)
summary(lm.fit)

# Try splitting data into training and testData
# do model
# Pred <- predict(lmMod, testData)

#actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=Pred))  # make actuals_predicteds dataframe.
#correlation_accuracy <- cor(actuals_preds) 
#head(actuals_preds)

#min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  




