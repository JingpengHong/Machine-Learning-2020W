## Machine Learning 2022W
## Problem Set 4
## Instructor: Guillaume Pouliot
## Author: Jingpeng Hong
## Date: Mar 5, 2022

## Housekeeping ##
# install.packages("tm")
library(tm)
library(dplyr)
# install.packages("SnowballC")
library(SnowballC)
# install.packages("wordcloud")
library(wordcloud)
# install.packages("topicmodels")
library(topicmodels)
# install.packages("tidytext")
library(tidytext)
# install.packages("reshape2")
library(reshape2)
library(ggplot2)
library(e1071)
library(ISLR)
# install.packages("caret")
library(caret)
# install.packages("eList")
library(eList)

rm(list=ls())
setwd("/Users/hongjingpeng/Desktop/Machine\ Learning/Machine-Learning-2022W/PSet4")

################
## Question 1 ##
################

## Load the data ##

texts = file.path("SimpleText_auto") 
docs_raw = VCorpus(DirSource(texts))

## Clean the data ##

docs = docs_raw %>%
  tm_map(content_transformer(tolower)) %>% # transform all characters to lowercase
  tm_map(removeWords, stopwords("english")) %>% # remove stop words
  tm_map(removePunctuation) %>% # remove punctuation
  tm_map(removeNumbers, ucp = TRUE) %>%
  tm_map(stripWhitespace) %>% # remove excess whitespace
  tm_map(stemDocument) %>% # get to words' roots
  tm_map(removeWords, c('figure', 'result', 'use', 'can', 'also', 
                        'one', 'two', 'fig', 'case', 'tabl', 'show', 'studi')) # remove commonly-occurring words in academic papers

# Justify the answers
docs_raw[[1]]$content[4]
docs[[1]]$content[4]

## Word Cloud ##

png(file="output/wordcloud.png", width=500, height=500, res=128)
wordcloud(docs, max.words = 50, scale=c(3, .2))
dev.off()

## Fit the topic model on the corpus setting k equal to 2, 3, 5, 8, and 10. 
set.seed(123)
dtm = DocumentTermMatrix(docs)

# define the function of topic models with k.
topic = function(k){
  lda = LDA(dtm, k = k, method = "Gibbs", control = list(burnin = 100, iter = 1000))
  topics = tidy(lda, matrix = "beta") 
  topwords = topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) # print the words with the highest beta from each topic
  for (i in 1:k) {
    print(topwords %>% filter(topic==i))
  }
}

# k = 2, 3, 5, 8, 10
topic(2)
topic(3)
topic(5)
topic(8)
topic(10)

## 10-fold cross-validation ##
k_vals = c(2, 5, 8, 10)
alpha_vals = c(0.01, 0.1, 0.5, 1)

n_folds = 10
fold_indices = createFolds(1:dtm$nrow, k = n_folds)

perplexity = data.frame(matrix(NA, nrow = n_folds + 1, # initialize empty df to hold perplexity estimates
                               ncol = length(k_vals) * length(alpha_vals)),
                        row.names = c(..[for (i in 1:n_folds) paste0('fold_', i)], 'avg'))
colnames(perplexity) = ..[for (k in k_vals) for (alpha in alpha_vals) paste0('k_', k, '_alpha_', alpha)]

pb = txtProgressBar(min = 0, max = n_folds, initial = 0) 
for (k in k_vals) {
  for (alpha in alpha_vals) {
    curr_col = paste0('k_', k, '_alpha_', alpha)
    for(i in 1:n_folds){
      train_data <- dtm[!((1:length(dtm$dimnames$Docs)) %in% fold_indices[[i]]),]
      val_data <- dtm[(1:length(dtm$dimnames$Docs)) %in% fold_indices[[i]],]
      
      model <- LDA(train_data, k = k, method = "Gibbs",
                   control = list(alpha = alpha, burnin = 100, iter = 1000))
      perplexity[i, curr_col] <- perplexity(model, newdata = val_data)
      setTxtProgressBar(pb,i)
    }
    perplexity['avg', curr_col] = mean(perplexity[1:n_folds, curr_col]) # compute avg perplexity across folds
  }
}

# Print the k/alpha combo that results in the min perplexity
names(perplexity)[apply(perplexity['avg', ], MARGIN = 1, FUN = which.min)]

# best model
bestmodel = LDA(dtm, k = 10, method = "Gibbs", 
                control = list(alpha = 0.5, burnin = 100, iter = 1000))
topics = tidy(bestmodel, matrix = "beta") 
topwords = topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) # print the words with the highest beta from each topic
for (i in 1:10) {
  print(topwords %>% filter(topic==i))
  }

################
## Question 2 ##
################

## create the circle function
circleFun = function(center = c(-1, 2), r = 2, npoints = 100){
  tt <- seq(0, 2*pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

## sketch the curve
data = circleFun(c(-1, 2), 2, npoints = 100)
ggplot(data, aes(x, y)) + 
  geom_path() +
  xlim(-4, 4) + 
  xlab("X1") +
  ylim(-2, 10) + 
  ylab("X2") +
  annotate("text", x = -1, y = 2, label = "<4") + 
  annotate("text", x = 0, y = 6, label= ">4") + 
  geom_point(aes(x = 0, y = 0), colour = 'blue', fill = NA, size = 2, shape = 21)+
  geom_point(aes(x = -1, y = 1), colour = 'red', fill = NA, size = 2, shape = 21)+ 
  geom_point(aes(x = 2, y = 2), colour = 'blue', fill = NA, size = 2, shape = 21)+ 
  geom_point(aes(x = 3, y = 8), colour = 'blue', fill = NA, size = 2, shape = 21)+ 
  coord_fixed()
ggsave("output/Q2.png", width = 3, height = 4)

################
## Question 3 ##
################

set.seed(123)

## initial data
x = rnorm(100)
y = 3*x**2 + 4

# translate along the y-axis to separate into two classes
y[1:50] = y[1:50] + 2
y[51:100] = y[51:100] - 2
z = c(rep(1, 50), rep(-1, 50))
data = data.frame(x, y, z=as.factor(z))

# create training and test data frames
# ensure in training set and test set, both classes have the equal size
data.positive = data[z==1, ]
train1 = sample(50, 25)
data.positive.train = data.positive[train1, ]
data.positive.test = data.positive[-train1, ]

data.negative = data[z==-1, ]
train2 = sample(50, 25)
data.negative.train = data.negative[train2, ]
data.negative.test = data.negative[-train2, ]

data.train = rbind(data.positive.train, data.negative.train)
data.test = rbind(data.positive.test, data.negative.test)

## SVM ##

## linear
svm.linear = svm(z~., data = data.train, kernel = 'linear', scale = FALSE)
summary(svm.linear)
png(file="output/Q3_linear.png", width=2000, height=1200, res=200)
plot(svm.linear, data.train, col = c("orange", "white"))
dev.off()
# training error rates
table(predict = svm.linear$fitted, truth = data.train$z)
# test error rates
table(predict = predict(svm.linear, data.test), truth = data.test$z)

## polynomial kernel
svm.poly = svm(z~., data = data.train, kernel = 'polynomial', scale = FALSE)
summary(svm.poly)
png(file="output/Q3_poly.png", width=2000, height=1200, res=200)
plot(svm.poly, data.train, col = c("orange", "white"))
dev.off()
# training error rates
table(predict = svm.poly$fitted, truth = data.train$z)
# test error rates
table(predict = predict(svm.poly, data.test), truth = data.test$z)

## radial kernel
svm.rad = svm(z~., data = data.train, kernel = 'radial', scale = FALSE)
summary(svm.rad)
png(file="output/Q3_rad.png", width=2000, height=1200, res=200)
plot(svm.rad, data.train, col = c("orange", "white"))
dev.off()
# training error rates
table(predict = svm.rad$fitted, truth = data.train$z)
# test error rates
table(predict = predict(svm.rad, data.test), truth = data.test$z)

################
## Question 4 ##
################

Auto = Auto

# Create a median-split binary variable for gas mileage
Auto$mpg2 = ifelse(Auto$mpg>median(Auto$mpg), 1, 0) %>%
  as.factor()

## cross validation with different values of cost
set.seed(1)
tune.out = tune(svm, mpg2~., data=Auto, kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

# best cost value
bestmod = tune.out$best.model
ypred = predict(bestmod, Auto)
table(predict = ypred, truth = Auto$mpg2)

## radial kernels
tune.radial = tune(svm, mpg2~., data=Auto, kernel="radial",
                   ranges = list(cost = c(0.1, 1, 10), 
                                 gamma = c(0.01, 0.1, 1)))
summary(tune.radial)

## polynomial kernels
tune.poly = tune(svm, mpg2~., data=Auto, kernel="polynomial",
                   ranges = list(cost = c(0.1, 1, 10), 
                                 degree = c(2, 3, 4)))
summary(tune.poly)

## plots ##
plotvar = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpg2", "name"))]) {
    png(file=paste("output/Q4", name, ".png", sep = ""), width=2000, height=1200, res=200)
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")), col = c("orange", "white"))
    dev.off()
  }
}
## linear
svmfit.linear = svm(mpg2~., data = Auto, kernel = "linear", cost = 1)
plotvar(svmfit.linear)

## radial
svmfit.radial = svm(mpg2~., data = Auto, kernel = "radial", cost = 10, gamma = 0.01)
plotvar(svmfit.radial)

## polynomial
svmfit.poly = svm(mpg2~., data = Auto, kernel = "polynomial", cost = 10, degree = 2)
plotvar(svmfit.poly)