## Machine Learning Lab 4
## Author: Jingpeng Hong
## Date: Mar 10, 2022


## Housekeeping ##
library(e1071)
library(ggplot2)
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

set.seed(1)
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

(cv_cost = models[which.min(models$error), "cost"])
(cv_kernel = models[which.min(models$error), "kernel"])
(cv_error = models[which.min(models$error), "error"])

##################################
## Exercise 2.3 Fit SVM on work ##
##################################

svmfit = svm(work ~ ., data = work_df, scale = FALSE,
             cost = cv_cost, kernel = cv_kernel)
predict_svmfit = predict(svmfit,
                         newdata = work_df[, !(names(work_df) %in% c("work"))])
(error <- sum(predict_svmfit != work_df[, "work"]) / length(predict_svmfit)) 

#######################################
## Exercise 2.4 Impute Work Schedule ##
#######################################

impute_work = predict(svmfit,
                      newdata = vote_df[, !(names(vote_df) %in% c("vote"))])

##################################################################
## Exercise 2.5 Regress Voting Status on Imputed Work Schedules ##
##################################################################
work_numeric = as.numeric(impute_work == "flexible")
vote_numeric = as.numeric(vote_df$vote == "vote")
reg = lm(vote_numeric ~ work_numeric + poly(prtage, 2) + pesex, data = vote_df)
stargazer::stargazer(reg, single.row = TRUE, header = FALSE)
work_vote_relationship = coef(reg)["work_numeric"]

##################################
## Exercise 2.6 Bias Correction ##
##################################

compute_M = function(a, b) {
  1 / (1 - 2 * b) * (1 - (1 - b) * b / a - (1 - b) * b / (1 - a))
}

(a <- sum(impute_work == "flexible") / length(impute_work)) 
(b <- cv_error)
(M <- compute_M(a, b)) 
(work_vote_bias_correction <- work_vote_relationship / M)

########################################################## 
## Exercise 2.7 Creating Bootstrap Confidence Intervals ##
##########################################################

num_bootstrap <- 50
bootstrap_work_vote <- vector("double", num_bootstrap) 
bootstrap_work_vote_corrected <- vector("double", num_bootstrap) 
for (i in seq_len(num_bootstrap)) {
  bootstrap_index <- sample(nrow(work_df), nrow(work_df), replace = TRUE)
  work_df_bootstrap <- work_df[bootstrap_index, ]
  svm_bootstrap <- svm(work ~ .,
                       data = work_df_bootstrap,
                       scale = FALSE,
                       cost = cv_cost,
                       kernel = cv_kernel)
  impute_bootstrap <- predict(svm_bootstrap,
                              newdata = vote_df[!(names(vote_df) %in% c("vote"))])
  a_bootstrap <- sum(impute_bootstrap == "flexible") / length(impute_bootstrap)
  b_bootstrap <- cv_svm(k = 5,
                        data = work_df_bootstrap,
                        scale = FALSE,
                        cost = cv_cost,
                        kernel = cv_kernel)
  impute_bootstrap_numeric <- as.numeric(impute_bootstrap == "flexible")
  bootstrap_index <- sample(nrow(vote_df), nrow(vote_df), replace = TRUE)
  vote_df_bootstrap <- vote_df[bootstrap_index, ]
  vote_bootstrap <- vote_numeric[bootstrap_index]
  work_bootstrap <- impute_bootstrap_numeric[bootstrap_index]
  reg_bootstrap <- lm(vote_bootstrap ~ work_bootstrap + poly(prtage, 2) + pesex,
                      data = vote_df_bootstrap)
  bootstrap_work_vote[i] <- coef(reg_bootstrap)["work_bootstrap"]
  M_bootstrap <- compute_M(a_bootstrap, b_bootstrap)
  bootstrap_work_vote_corrected[i] <- bootstrap_work_vote[i] / M_bootstrap
}
ggplot() +
  geom_histogram(aes(x = bootstrap_work_vote, fill = "0"), color = "white") +
  geom_histogram(aes(x = bootstrap_work_vote_corrected, fill = "1"), color = "white") + 
  scale_fill_manual(labels = c("naive", "bias-corrected"),
                  values = c("coral", "cornflowerblue"),
                  name = "") +
  geom_vline(xintercept = work_vote_relationship) +
  geom_vline(xintercept = work_vote_bias_correction) +
  xlab("bootstrapped results") +
  ylab("")
ggsave("eshist.png", width = 6, height = 3)

# lower/upper bound of 95% confidence interval
naive_ci <- quantile(bootstrap_work_vote, prob = c(0.025, 0.975))
corrected_ci <- quantile(bootstrap_work_vote_corrected, prob = c(0.025, 0.975))
ggplot() +
  geom_segment(aes(x = naive_ci["2.5%"], xend = naive_ci["97.5%"],
                   y = "naive", yend = "naive", color = "0")) +
  geom_point(aes(x = work_vote_relationship, y = "naive", color = "0")) +
  geom_segment(aes(x = corrected_ci["2.5%"], xend = corrected_ci["97.5%"],
                   y = "bias-corrected", yend = "bias-corrected", color = "1")) +
  geom_point(aes(x = work_vote_bias_correction, y = "bias-corrected", color = "1")) +
  scale_color_manual(labels = c("naive", "bias-corrected"),
                     values = c("coral", "cornflowerblue")) +
  xlab("bootstrapped results") +
  ylab("") +
  theme(legend.position = "none")
ggsave("esinterval.png", width = 6, height = 3)
# standard errors
sd(bootstrap_work_vote)
sd(bootstrap_work_vote_corrected) 
