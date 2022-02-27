## Machine Learning Lab 3
## Author: Jingpeng Hong
## Date: Feb 25, 2022


## Housekeeping ##
# install.packages("fastDummies")
library(fastDummies)
# install.packages("readxl")
library(readxl)
library(dplyr)
library(stargazer)
library(glmnet)
rm(list=ls())
setwd("/Users/hongjingpeng/Desktop/Machine\ Learning/Machine-Learning-2022W/Lab3")


## Preparing Data ##
CovidData = read.csv("CovidData.csv")
vardes = read_excel("VariableDescription.xlsx")

## Data Analysis ##

## 1. Filter variables
oi = pull(vardes[vardes$Source == "Opportunity Insights", ], Variable) ## Opportunity Insights
oi_label = pull(vardes[vardes$Source == "Opportunity Insights", ], Description)
pmcovid = pull(vardes[vardes$Source == "PM_COVID", ], Variable) ## PM COVID
pmcovid_label = pull(vardes[vardes$Source == "PM_COVID", ], Description)
data = select(CovidData, c(county, state, deathspc, oi, pmcovid))

## 2. Descriptive statistics
stargazer(select(CovidData, c(oi)), digits = 2, 
          covariate.labels = c(oi_label), omit.summary.stat = c("p25", "p75"))
stargazer(select(CovidData, c(pmcovid)), digits = 2, 
          covariate.labels = c(pmcovid_label), omit.summary.stat = c("p25", "p75"))

## 3. Drop all observations with missing values.
data = na.omit(data)

## 4. Create dummies for states and the District of Columbia
data = dummy_cols(data, select_columns = "state")

# Note: Observations from New Jersey (21) and District of Columbia (1) have NAs,
# so we only have 47 dummy variables here.

## 5. Split the sample into training (80% of the data) and test (20% of the data) sets.
set.seed(1)
train = sample(seq_len(nrow(data)), size = 4*nrow(data)/5)
data.train = data[train,]
data.test = data[-train,]

## 6. OLS estimation
data.train.fit = select(data.train, -c("county", "state", "state_Wisconsin"))
ols = lm(deathspc ~ .-1 , data = data.train.fit)
summary(ols)
alias(ols)
# MSE and R^2 in the training set
r2.train = summary(ols)$r.sq
mse.train = mean(ols$residuals^2)

# MSE in the test set
data.test.fit = select(data.test, -c("county", "state", "state_Wisconsin"))
pred.test = predict(ols, data.test.fit)
y.test = data.test.fit$deathspc
mse.test = mean((y.test - pred.test)^2)

# R^2 in the test set
rss = sum((y.test - pred.test)^2)
tss = sum((y.test - mean(y.test))^2)
r2.test = 1 - rss/tss

## 7. Ridge and Lasso
grid = 10^seq(2, -2, length=100)
y = data.train$deathspc
x = model.matrix(deathspc~., data.train.fit)[,-1]

## Ridge
# a. model estimation
ridge.mod=glmnet(x, y, alpha=0, lambda=grid) # glmnet() standardizes the variables by default
# b. 10-fold cross-validation
ridge.cv.out = cv.glmnet(x, y, alpha=0, lambda=grid)
# c.plot
plot(ridge.cv.out)
# d. choosing the optimal value
ridge.bestlam = ridge.cv.out$lambda.min
# e. re-estimate using the optimal lambda
ridge.bestmod = glmnet(x, y, alpha=0, lambda=ridge.bestlam) 

## Lasso
# a. model estimation
lasso.mod=glmnet(x, y, alpha=1, lambda=grid) 
# b. 10-fold cross-validation
lasso.cv.out = cv.glmnet(x, y, alpha=1, lambda=grid)
# c.plot
plot(lasso.cv.out)
# d. choosing the optimal value
lasso.bestlam = lasso.cv.out$lambda.min
# e. re-estimate using the optimal lambda
lasso.bestmod = glmnet(x, y, alpha=1, lambda=lasso.bestlam) 

## 8. training set and test set prediction errors (MSE & R^2) for Ridge and Lasso.

x.test = model.matrix(deathspc~., data.test.fit)[,-1]

## Ridge
ridge.pred = predict(ridge.bestmod, s = ridge.bestlam, newx =x.test) 
mse.ridge = mean((ridge.pred - y.test)^2)
rss.ridge = sum((y.test - ridge.pred)^2)
r2.ridge = 1 - rss.ridge/tss

## Lasso
lasso.pred = predict(lasso.bestmod, s = lasso.bestlam, newx =x.test) 
mse.lasso = mean((lasso.pred - y.test)^2)
rss.lasso = sum((y.test - lasso.pred)^2)
r2.lasso = 1 - rss.lasso/tss
