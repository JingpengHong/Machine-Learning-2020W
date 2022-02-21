## Machine Learning 2022W
## Problem Set 3
## Instructor: Guillaume Pouliot
## Author: Jingpeng Hong
## Date: Feb 18, 2022

## Housekeeping ##
library(leaps)
library(ggplot2)
library(glmnet)
library(ISLR)
library(tree)
library(gbm)
library(randomForest)
library(glmnet)
rm(list=ls())
setwd("/Users/hongjingpeng/Desktop/Machine\ Learning/Machine-Learning-2022W/PSet3")

##################
## Chapter 6 Q8 ##
##################

## a. Generate a predictor X and a noise vector e.
set.seed(1)
x = rnorm(100)
e = rnorm(100)

## b. Generate a response vector Y.
## Y = 1 + 2X + 3X^2 + 4X^3 + e
beta0 = 1
beta1 = 2
beta2 = 3
beta3 = 4
y = beta0 + beta1*x + beta2*x**2 + beta3*x**3 +e

## c. Best subset selection
data.full = data.frame(y, x)
regfit.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
regfit.summary = summary(regfit.full)

# the best model obtained according to Cp, BIC, and adjusted R2
cp = regfit.summary$cp
which.min(cp)
bic = regfit.summary$bic
which.min(bic)
adjr2 = regfit.summary$adjr2
which.max(adjr2)

# display the plots
png(file="output/ch6q8_C.png", width=1600, height=1600, res=300)
par(mfcol=c(3,1), mai=c(.25,.6,.4,.25))
plot(cp, type = "b", col="red", lwd=2, ylab="Cp")
plot(bic, type = "b", col="green", lwd=2, ylab="BIC")
plot(adjr2, type = "b", col="blue", lwd=2, xlab="Degree", ylab="Adj-R2")
dev.off()

# report the coefficients of the best model obtained
coef(regfit.full, 3)

## d. Forward/Backward Stepwise Selection
regfit.fwd=regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, method="forward")
regfit.fwd.summary = summary(regfit.fwd)
regfit.bwd=regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, method="backward")
regfit.bwd.summary = summary(regfit.bwd)

# the best model obtained according to Cp, BIC, and adjusted R2
# Forward stepwise selection
cp.fwd = regfit.fwd.summary$cp
which.min(cp.fwd)
bic.fwd = regfit.fwd.summary$bic
which.min(bic.fwd)
adjr2.fwd = regfit.fwd.summary$adjr2
which.max(adjr2.fwd)

# Backward stepwise selection
cp.bwd = regfit.bwd.summary$cp
which.min(cp.bwd)
bic.bwd = regfit.bwd.summary$bic
which.min(bic.fwd)
adjr2.bwd = regfit.bwd.summary$adjr2
which.max(adjr2.bwd)

# display the plots
png(file="output/ch6q8_d.png", width=1600, height=800, res=128)
par(mfcol=c(3,2), mai=c(.3,.3,.3,.3))
plot(cp.fwd, type = "b", lwd=2, col="red")
plot(bic.fwd, type = "b", lwd=2, col="green")
plot(adjr2.fwd, type = "b", lwd=2, col="blue")
plot(cp.bwd, type = "b", lwd=2, col="red")
plot(bic.bwd, type = "b", lwd=2, col="green")
plot(adjr2.bwd, type = "b", lwd=2, col="blue")
dev.off()

# report the coefficients of the best model obtained
coef(regfit.fwd, 3)
coef(regfit.bwd, 3)

## e. Lasso
X = poly(x, 10, raw = T)

# cross-validation
set.seed(1)
cv.out = cv.glmnet(X, y, alpha=1)
png(file="output/ch6q8_e.png", width=1600, height=800, res=128)
plot(cv.out)
dev.off()

# model estimation
bestlam = cv.out$lambda.min
lasso.mod = glmnet(X, y, alpha=1, lambda=bestlam)
coef(lasso.mod)

## f.
## Y = 1 + 8X^7 + e
beta7 = 8
yf = beta0 + beta7*x**7 +e

# best subset selection
data.f = data.frame(yf, x)
regfit.f = regsubsets(yf ~ poly(x, 10, raw = T), data = data.f, nvmax = 10)
regfit.summary.f = summary(regfit.f)

cp = regfit.summary.f$cp
which.min(cp)
bic = regfit.summary.f$bic
which.min(bic)
adjr2 = regfit.summary.f$adjr2
which.max(adjr2)

coef(regfit.f, 1)
# Lasso
# cross-validation
cv.out.f = cv.glmnet(X, yf, alpha=1)

# model estimation
bestlam.f = cv.out.f$lambda.min
lasso.mod.f = glmnet(X, yf, alpha=1, lambda=bestlam.f)
coef(lasso.mod.f)

##################
## Chapter 8 Q3 ##
##################

x = seq(0, 1, .01)
G =  2*x*(1-x)
D = -x*log(x)-(1-x)*log(1-x)
E = 1-(x*(x > 0.5)+(1-x)*(x <= 0.5))

data = data.frame(x, G, D, E)
ggplot(data=data, aes(x=x))+
  geom_line(aes(y=G, col='G'))+
  geom_line(aes(y=D, col='D'))+
  geom_line(aes(y=E, col='E'))+
  xlab("p")+
  ylab("Criterion")+
  scale_color_hue("", labels = c(G="Gini index", D="Entropy", E="Classification error"))+ 
  theme(legend.position = "bottom", legend.box = "horizontal")
ggsave("output/ch8q3.png", width = 6, height = 4, dpi = 300)

##################
## Chapter 8 Q8 ##
##################

# a. Split the data set into a training set and a test set.
set.seed(1)
train=sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]

# b
# Fit a regression tree to the training set.
tree.carseats = tree(Sales~., Carseats.train)
summary(tree.carseats)
# Plot the tree
png(file="output/ch8q8_b.png", width=2000, height=1000, res=128)
plot(tree.carseats)
text(tree.carseats,pretty=0)
dev.off()
# Estimate the test error
tree.pred = predict(tree.carseats, newdata=Carseats.test)
mean((tree.pred-Carseats.test$Sales)^2)

# c. cross-validation
cv.carseats = cv.tree(tree.carseats)
cv.carseats
png(file="output/ch8q8_c.png", width=2000, height=1000, res=128)
plot(cv.carseats$size, cv.carseats$dev, type='b', col='red', xlab="Tree size", ylab="CV Classification Error")
dev.off()

# d. bagging
bag.carseats = randomForest(Sales~., data = Carseats.train, 
                           mtry = ncol(Carseats.train)-1, importance = TRUE)
yhat.bag.carseats = predict(bag.carseats, newdata = Carseats.test)
mse.bag.carseats = mean((yhat.bag.carseats - Carseats.test$Sales)^2)
importance(bag.carseats)

# e. random forest
m = seq(1, 10, by=1)
mse.test = rep(0, length(m))

# random forest with different m.
for (i in 1:length(m)){
  
  rf.carseats = randomForest(Sales~., data = Carseats.train, 
                             mtry = m[i], importance = TRUE)
  yhat.test.rf = predict(rf.carseats, newdata = Carseats.test)
  mse.test[i] = mean((yhat.test.rf-Carseats.test$Sales)^2)
  
}

# plot m and test set MSE
mse.plot = data.frame(m, mse.test)
ggplot(data = mse.plot, aes(x=m))+
  geom_line(aes(y=mse.test), col = 'red')+
  geom_point(aes(y=mse.test), col = 'red', shape=1)+
  ylab('MSE')+
  xlab('Number of variables considered at each split')
ggsave("output/ch8q8_e.png", width = 6, height = 4, dpi = 300)

# consider the case m=3
rf.carseats = randomForest(Sales~., data = Carseats.train, 
                           mtry = 3, importance = TRUE)
yhat.test.rf = predict(rf.carseats, newdata = Carseats.test)
mse.test = mean((yhat.test.rf-Carseats.test$Sales)^2)
importance(rf.carseats)

##################
## Chapter 8 Q9 ##
##################

# a. Split the data set into a training set and a test set.
set.seed(1)
train=sample(1:nrow(OJ), 800)
OJ.train=OJ[train,]
OJ.test=OJ[-train,]

# b. Fit a tree to the training data.
tree.OJ = tree(Purchase~., OJ.train)
summary(tree.OJ)

# c. Type in the name of the tree object in order to get a detailed text output.
tree.OJ

# d. Create a plot of the tree.
png(file="output/ch8q9_d.png", width=2000, height=800, res=128)
plot(tree.OJ)
text(tree.OJ, pretty=0)
dev.off()

# e. Predict on the test data
OJ.pred = predict(tree.OJ, newdata=OJ.test, type='class')
table(OJ.pred, OJ.test$Purchase) # confusion matrix
error.test = mean(OJ.pred != OJ.test$Purchase) # test error rate

# f. determine the optimal tree size
cv.OJ = cv.tree(tree.OJ, FUN=prune.misclass)
cv.OJ

# g. Produce a plot with tree size on the x and cross-validated classification error rate on the y
png(file="output/ch8q9_g.png", width=2000, height=1000, res=128)
plot(cv.OJ$size, cv.OJ$dev, type="b", col='red', xlab="Tree size", ylab="CV Classification Error")
dev.off()

# i. produce a pruned tree
prune.OJ=prune.misclass(tree.OJ, best=7) 
summary(prune.OJ)
png(file="output/ch8q9_i.png", width=2000, height=800, res=128)
plot(prune.OJ)
text(prune.OJ, pretty=0)
dev.off()

# k. Compare the test error rates
OJ.prune.pred=predict(prune.OJ, newdata=OJ.test,type="class") 
table(OJ.prune.pred, OJ.test$Purchase)
error.prune.test = mean(OJ.prune.pred != OJ.test$Purchase) # test error rate

###################
## Chapter 8 Q10 ##
###################

# a.
Hitters = na.omit(Hitters) # Remove the observations for whom the salary is NA
Hitters$Salary = log(Hitters$Salary) # log-transform the salaries

# b.
train = 1:200
Hitters.train = Hitters[train,] # training set with first 200 observations
Hitters.test = Hitters[-train,] # test set with the remaining observations.

# c-d. 
set.seed(1)
lambda = 10^seq(-4, 0, by=0.2)
mse.train = rep(0, length(lambda))
mse.test = rep(0, length(lambda))

# boosting with different shrinkage parameter.
for (i in 1:length(lambda)){
  
  boost.Hitters = gbm(Salary~., data = Hitters.train, distribution = 'gaussian',
                    n.trees = 1000, interaction.depth = 1, shrinkage = lambda[i])
  yhat.train = predict(boost.Hitters, Hitters.train, n.trees = 1000)
  mse.train[i] = mean((yhat.train-Hitters.train$Salary)^2)
  yhat.test = predict(boost.Hitters, Hitters.test, n.trees = 1000)
  mse.test[i] = mean((yhat.test-Hitters.test$Salary)^2)
  
}

# plot shrinkage values and training/test set MSE
mse.plot = data.frame(lambda, mse.train, mse.test)
ggplot(data = mse.plot, aes(x=lambda))+
  geom_line(aes(y=mse.train, color='train'))+
  geom_point(aes(y=mse.train, color='train'), shape=1)+
  geom_line(aes(y=mse.test, color='test'))+
  geom_point(aes(y=mse.test, color='test'), shape=1)+
  ylab('MSE')+
  xlab('Shrinkage values')+
  scale_color_hue("", labels = c(train="training set MSE", test="test set MSE"))+
  scale_x_continuous(trans='log10')+
  theme(legend.position = "bottom", legend.box = "horizontal")
ggsave("output/ch8q10_cd.png", width = 6, height = 4, dpi = 300)

# e. apply two of the regression approaches seen in Chapters 3 and 6
# linear regression
lm.Hitters = lm(Salary~., data = Hitters.train)
yhat.lm = predict(lm.Hitters, newdata = Hitters.test)
mse.test.lm = mean((yhat.lm - Hitters.test$Salary)^2)

# Lasso
x = model.matrix(Salary~., data = Hitters.train)
x.test = model.matrix(Salary~., data = Hitters.test)
lasso.Hitters = glmnet(x, Hitters.train$Salary, alpha = 1)
yhat.lasso = predict(lasso.Hitters, newx = x.test)
mse.test.lasso = mean((yhat.lasso - Hitters.test$Salary)^2)

# f. the most important predictors in the boosted model
lambda[which.min(mse.test)]
boost.Hitters = gbm(Salary~., data = Hitters.train, distribution = 'gaussian',
                    n.trees = 1000, interaction.depth = 1, shrinkage = lambda[which.min(mse.test)])
yhat.boost = predict(boost.Hitters, newdata = Hitters.test)
mse.test.boost = mean((yhat.boost - Hitters.test$Salary)^2)
png(file="output/ch8q10_f.png", width=1600, height=1600, res=300)
summary(boost.Hitters)
dev.off()

# g. Apply bagging to the training set
set.seed(1)
bag.Hitters = randomForest(Salary~., data = Hitters.train, 
                           mtry = ncol(Hitters.train)-1, importance = TRUE)

# test MSE
yhat.bag = predict(bag.Hitters, newdata = Hitters.test)
mse.test.bag = mean((yhat.bag - Hitters.test$Salary)^2)
