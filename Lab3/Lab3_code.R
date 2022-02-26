## Machine Learning Lab 3
## Author: Jingpeng Hong
## Date: Feb 25, 2022


## Housekeeping ##
# install.packages("fastDummies")
library("fastDummies")
# install.packages("readxl")
library("readxl")
library("dplyr")
library("stargazer")
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

# Note: Observations from Delaware (3) and District of Columbia (1) have NAs,
# so we only have 47 dummy variables here

# 5. Split the sample into training (80% of the data) and test (20% of the data) sets.
set.seed(1)
train = sample(seq_len(nrow(data)), size = 4*nrow(data)/5)
data.train = data[train,]
data.test = data[-train,]















