## Machine Learning Lab 1
## Author: Jingpeng Hong
## Date: Jan 29, 2022


## Housekeeping ##
#install.packages("stargazer")
#install.packages("ggplot2")
#install.packages("car")
library(ggplot2)
library(stargazer)
library(car)

rm(list=ls())
setwd("/Users/hongjingpeng/Desktop/Machine Learning/Machine-Learning-2022W/Lab1/")

########################
## Preparing the Data ##
########################

data=read.csv("IPUMS_data.csv")
crosswalk=read.csv("Crosswalk.csv", sep=";")

## Create continuous measurement of education
data = merge(x=data, y=crosswalk, by.x="EDUCD", by.y="educd", all.x=TRUE)

## Create dummy variables
data$hsdip = ifelse(data$EDUCD>=62 & data$EDUCD<=100, 1, 0) # dummy for high school diploma
data$coldip = ifelse(data$EDUCD>=101 & data$EDUCD<999, 1, 0) # dummy for four-year college diploma
data$white = ifelse(data$RACE==1, 1, 0) # dummy for white
data$black = ifelse(data$RACE==2, 1, 0) # dummy for black
data$hispanic = ifelse(data$HISPAN>=1 & data$HISPAN<=4, 1, 0) # dummy for Hispanic origin
data$married = ifelse(data$MARST>=1 & data$MARST<=3, 1, 0) # dummy for married
data$female = ifelse(data$SEX==2, 1, 0) # dummy for female
data$vet = ifelse(data$VETSTAT==2, 1, 0) # dummy for veteran

## Create interaction terms
data$hsdip_educdc = data$hsdip*data$educdc # interaction, high school diploma and years of education
data$coldip_educdc = data$coldip*data$educdc # interaction, college diploma and years of education

## Create other variables
data$age2 = data$AGE*data$AGE # age squared
data$lnincwage = log(data$INCWAGE+1) # natural log of incwage

###################
## Data Analysis ##
###################

## descriptive statistics
stargazer(data[c('YEAR', 'INCWAGE', 'lnincwage', 'educdc', 'female', 'AGE', 'age2', 
                 'white', 'black', 'hispanic', 'married', 'NCHILD', 'vet', 'hsdip', 
                 'coldip', 'hsdip_educdc', 'coldip_educdc')], 
          omit.summary.stat=c("p25", "p75"), digits=2, out="output/summary.tex")

## Scatter plot ln(incwage) and education with a linear fit line.
ggplot(data, aes(x=educdc,y=lnincwage)) +
  geom_point(alpha=0.5, size=1.5, shape=1, fill = NA) +
  labs(x= "Number of Education Years", y="log Wages")+
  ggtitle("Wages by Number of Education Years") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method='lm', color='red', size=0.5)
ggsave("output/scatter_line.png", scale=1.5)

## Regression model estimation
model1 = lm(lnincwage~educdc+female+AGE+age2+white+black+hispanic+married+NCHILD+vet,
            data=data) # Output combined with results in question 5

## Hypothesis Testing -3.b
linearHypothesis(model1, c('educdc', 'female', 'AGE', 'age2', 'white', 'black', 
                           'hispanic', 'married', 'NCHILD', 'vet'))

## Hypothesis Testing -3.g
linearHypothesis(model1, c("white=0", "black=0", 'hispanic=0'))

## With no high school diploma/ High school diploma/ College degree
data$degree=0
data$degree[data$hsdip==1] = 1
data$degree[data$coldip==1] = 2
data$degree = factor(data$degree)

ggplot(data, aes(x=educdc,y=lnincwage, color=degree, shape=degree)) +
  geom_point(alpha=0.5, size=1.5, shape=1, fill = NA) +
  labs(x= "Number of Education Years", y="log Wages")+
  ggtitle("Wages by Number of Education Years") +
  scale_color_discrete(labels = c("With no high school diploma", 
                                  "High school diploma", 
                                  "College degree"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.15, .2), legend.title = element_blank())+
  geom_smooth(method='lm')
ggsave("output/scatter_line_degree.png", scale=1.5)

## Model that will allow the returns to education to vary by degree acquired
model2 = lm(lnincwage~educdc+female+AGE+age2+white+black+hispanic+married+NCHILD+vet
            +hsdip_educdc+coldip_educdc,
            data=data)
stargazer(model1, model2, title="OLS Regression", align=TRUE, out="output/linear_reg.tex",
          omit.stat=c("LL","ser"), no.space=TRUE)

## Predict the wages of an 22 year old, female individual 
## (who is neither white, black, nor Hispanic, not married, no children, not a veteran) 
## with a high school diploma and an all else equal individual with a college diploma. 
## 12 years to graduate high school and 16 years to graduate college.
data_pred=data.frame(educdc=c(12,16), female=c(1,1), AGE=c(22,22), age2=c(484,484), 
                white=c(0,0), black=c(0,0), hispanic=c(0,0), married=c(0,0), NCHILD=c(0,0), 
                vet=c(0,0), hsdip_educdc=c(12,0), coldip_educdc=c(0,16))
pred=predict(model2,data_pred)







