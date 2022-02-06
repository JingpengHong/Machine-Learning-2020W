## Machine Learning 2022W
## Problem Set 2
## Instructor: Guillaume Pouliot
## Author: Jingpeng Hong
## Date: Feb 4, 2022

## Housekeeping ##
library(wooldridge)
library(plyr)
library(stargazer)
library(ggplot2)
library(boot)
library(MASS)
library(ggfortify)
setwd("/Users/hongjingpeng/Desktop/Machine\ Learning/Machine-Learning-2022W/PSet2")

#####################
## PSet Question 1 ##
#####################

x1=rnorm(1000)
f1=-x1**2-x1
x2=rnorm(1000)
f2=x2**2-x2

y=f1+2*f2+rnorm(1000)
model.true=lm(y~f1+f2)
model.linear=lm(y~x1+x2)

stargazer(model.true, model.linear, type='latex', 
          omit.stat=c("ser", "f"), summary=FALSE, no.space=TRUE)

## regression diagnostics
png(file="output/q1.png", width=1000, height=600, res=96)
plot(model.linear, 1)
dev.off()
#####################
## PSet Question 2 ##
#####################

## Data Preparing ##
data(catholic)
attach(catholic)
female = as.factor(female)
gender = revalue(female, c("1" = "female", "0" = "male"))
hsgrad = as.factor(hsgrad)
hs = revalue(hsgrad, c("1" = "highschool", "0" = "no_highschool"))

## Data Analysis ##
model1 = lm(read12 ~ gender)
model2 = lm(read12 ~ gender - 1)
model3 = lm(read12 ~ gender*hs)
model4 = lm(read12 ~ gender*lfaminc)

stargazer(model1, model2, model3, model4, type='latex', 
          omit.stat=c("ser", "f"), summary=FALSE, no.space=TRUE)

##########################
## Chapter 5 Exercise 2 ##
##########################

## Question(g) ##
n = seq(1, 100000, 1)
p = 1 - (1-1/n)**n
png(file="output/q2-g.png", width=1000, height=600, res=96)
plot(n, p, type='l', col='red', scipen=5)
dev.off()

## Question(h) ##
store=rep(NA, 10000)
for(i in 1:10000){
  store[i]=sum(sample(1:100, rep=TRUE)==4)>0 
  }
mean(store)

##########################
## Chapter 5 Exercise 8 ##
##########################

## (a) Generate a simulated data set

set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

## (b) Scatter plot of X against Y 
png(file="output/q8-b.png", width=1000, height=600, res=96)
plot(x, y, xlab="X", ylab="Y", col='red')
dev.off()

## (c) LOOCV
set.seed(1)
df=data.frame(x,y)
loocv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(y~poly(x,i), data=df)
  loocv.error[i]=cv.glm(df, glm.fit)$delta[1]
  }
loocv.error

## (d) LOOCV using another random seed
set.seed(100)
df=data.frame(x,y)
loocv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(y~poly(x,i), data=df)
  loocv.error[i]=cv.glm(df, glm.fit)$delta[1]
}
loocv.error

## (f) Statistical significance of the coefficient estimates 
df=data.frame(x,y)
glm.fit1=glm(y~poly(x,1), data=df)
glm.fit2=glm(y~poly(x,2), data=df)
glm.fit3=glm(y~poly(x,3), data=df)
glm.fit4=glm(y~poly(x,4), data=df)

stargazer(glm.fit1, glm.fit2, glm.fit3, glm.fit4, type='latex', summary=FALSE, no.space=TRUE)

##########################
## Chapter 5 Exercise 9 ##
##########################
## Load Data ##
rm(list=ls())
data(Boston)

## (a) an estimate for the population mean of medv ##
mean(Boston$medv)

## (b) an estimate for of the standard error of medv ##
sd.medv=sd(Boston$medv)
se.medv=sd.medv/sqrt(nrow(Boston))

## (c) estimate the standard error of \mu using the bootstrap ##
## create the function to output the estimate for the mean based on the selected observations.
mu.fn=function(data, index){
  return(mean(data$medv[index]))
}
## Bootstrap
set.seed(1)
boot(Boston, mu.fn, R=1000)

## (d) ##
t.test(Boston$medv)

## (e) Provide an estimate for the median of medv in the population.##
median(Boston$medv)

## (f) Estimate the standard error of median using the bootstrap##
median.fn=function(data, index){
  return(median(data$medv[index]))
}
## Bootstrap
set.seed(1)
boot(Boston, median.fn, R=1000)

## (g) Provide an estimate for the tenth percentile of medv in the population.##
quantile(Boston$medv, probs = c(0.1))

## (h) Estimate the standard error of median using the bootstrap##
quantile10.fn=function(data, index){
  return(quantile(data$medv[index], probs = c(0.1)))
}
## Bootstrap
set.seed(1)
boot(Boston, quantile10.fn, R=1000)
