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

rm(list=ls())
setwd("/Users/hongjingpeng/Desktop/Machine\ Learning/Machine-Learning-2022W/PSet4")

## Load the data ##

texts = file.path("SimpleText_auto") 
docs_raw = VCorpus(DirSource(texts))

####################
## Clean the data ##
####################

docs = docs_raw %>%
  tm_map(content_transformer(tolower)) %>% # transform all characters to lowercase
  tm_map(removeWords, stopwords("english")) %>% # remove stop words
  tm_map(removeWords, c('table', 'figure', 'results', 'use', 'can', 'also')) %>% # remove commonly-occurring words in academic papers
  tm_map(removePunctuation) %>% # remove punctuation
  tm_map(removeNumbers, ucp = TRUE) %>%
  tm_map(stripWhitespace) %>% # remove excess whitespace
  tm_map(stemDocument) # get to words' roots

# Justify the answers
docs_raw[[1]]$content[4]
docs[[1]]$content[4]

################
## Word Cloud ##
################
png(file="output/wordcloud.png", width=500, height=500, res=128)
wordcloud(docs, max.words = 50, scale=c(3, .2))
dev.off()

##
set.seed(123)

dtm <- DocumentTermMatrix(docs)

inspect(dtm) # peek at document term matrix

# Model with k = 2
k = 2
lda2 <- LDA(dtm, k = k, method = "Gibbs", control = list(burnin = 100, iter = 1000))

topics2 <- tidy(lda2, matrix = "beta") 
topwords2 = topics2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) # print the words with the highest beta from each topic
for (i in 1:k) {
  print(topwords2 %>% filter(topic==i))
}


set.seed(1)
x=matrix(rnorm(20*2), ncol=2) 
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1

plot(x, col=(3-y))

dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=.1,
           scale=FALSE)
plot(svmfit, dat)
svmfit$index


circleFun = function(center = c(-1, 2), r = 2, npoints = 100){
  tt <- seq(0, 2*pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

dat <- circleFun
ggplot(dat,aes(x,y)) + 
  geom_path()
