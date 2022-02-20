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
train=sample(1:nrow(Carseats), 200)
Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]

tree.carseats=tree(Sales~., Carseats.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)



