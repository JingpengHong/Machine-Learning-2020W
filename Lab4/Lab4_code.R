## Machine Learning Lab 4
## Author: Jingpeng Hong
## Date: Mar 10, 2022


## Housekeeping ##
library(e1071)
rm(list=ls())
setwd("/Users/hongjingpeng/Desktop/Machine\ Learning/Machine-Learning-2022W/Lab4")

##################
## Exercise 1.1 ##
##################
help(svm)
help(plot.svm)

###########################
## Empirical Application ##
###########################

vote_df = read.csv("vote.csv")
work_df = read.csv("work.csv")
vote_demo = vote_df
work_demo = work_df

#################################
## Exercise 2.1 Clean the data ##
#################################

## prcitshp: U.S. citizenship status
prcitshp_unique = unique(c(vote_df$prcitshp, work_df$prcitshp))
vote_df$prcitshp = factor(vote_df$prcitshp, levels = prcitshp_unique)
work_df$prcitshp = factor(work_df$prcitshp, levels = prcitshp_unique)

## pesex: sex
vote_df$pesex = as.factor(vote_df$pesex)
work_df$pesex = as.factor(work_df$pesex)

## ptdtrace: race (vote_df has 15 levels whereas word_df has 13, 16 levels in total.)
ptdtrace_unique = unique(c(vote_df$ptdtrace, work_df$ptdtrace))
vote_df$ptdtrace = factor(vote_df$ptdtrace, levels = ptdtrace_unique)
work_df$ptdtrace = factor(work_df$ptdtrace, levels = ptdtrace_unique)

## pehspnon: Hispanic or Non-Hispanic status
vote_df$pehspnon = as.factor(vote_df$pehspnon)
work_df$pehspnon = as.factor(work_df$pehspnon)

## peeduca: highest level of schooling
vote_df$peeduca = as.factor(vote_df$peeduca)
work_df$peeduca = as.factor(work_df$peeduca)

## prtage: age
vote_df$prtage = as.integer(vote_df$prtage)
work_df$prtage = as.integer(work_df$prtage)

# synthetic binary variable
vote_df$vote = as.factor(vote_df$vote)
work_df$work = as.factor(work_df$work)

###############################################################
## Exercise 2.2 Cross-validation to pick the cost and kernel ##
###############################################################

cv_svm = function(k, data, ...) {
  # randomly assign each observation to a fold --- 
  initialization = rep(seq_len(k), nrow(data)) 
  shuffle = sample(seq_len(nrow(data)), nrow(data)) 
  fold_label = initialization[shuffle]
  # compute the error for each validation set --- 
  error = vector("double", k)
  for (i in seq_len(k)) {
    hold_out = fold_label == i
    train = data[!hold_out, ] # create training set 
    test = data[hold_out, ] # create validation set 
    # fit the candidate SVM on the training set 
    svm_kfold = svm(work ~ .,
    data = train,
    ...)
predict_kfold = predict(svm_kfold,
                         newdata = test[, !(names(test) %in% c("work"))]) # compute classification error
error[i] = sum(predict_kfold != test[, "work"]) / length(predict_kfold)
  }
  # compute the mean error across the validation sets ---
  mean(error)
}

cost_values = c(1, 5, 10)
kernel_values = c("linear", "sigmoid")
models = expand.grid(cost = cost_values, kernel = kernel_values) 
models$error = NA_real_
for (cost_candidate in cost_values) {
  for (kernel_candidate in kernel_values) {
    # run 5-fold cross-validation for each model 
    fold_error = cv_svm(k = 5,
                        data = work_df,
                        scale = FALSE,
                        cost = cost_candidate,
                        kernel = kernel_candidate)
# store the cross-validation error in a data frame
models[models$cost == cost_candidate &
         models$kernel == kernel_candidate, "error"] <- fold_error
  } }
print(models)



