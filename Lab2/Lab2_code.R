## Machine Learning Lab 2
## Author: Jingpeng Hong
## Date: Feb 14, 2022


## Housekeeping ##
# install.packages("devtools")
# devtools::install_github("omkarakatta/diftrans")
library(diftrans)
library(dplyr) # for manipulating data 
library(tidyr) # for cleaning data 
library(ggplot2) # for visualization
rm(list=ls())
setwd("/Users/hongjingpeng/Desktop/Machine\ Learning/Machine-Learning-2022W/Lab2")

## Clean Data of Beijing and Tianjin Car Sales ##

#######################################
## Beijing pre and post (Exercise 4.1) ##
#######################################
# keep 2010 and 2011 only
Beijing <- Beijing_sample %>%
  filter(year >= 2010 & year < 2012)

# collect unique MSRP values
uniqueMSRP_Beijing <- data.frame(MSRP = unique(Beijing$MSRP)) 

# aggregate sales at each price for 2010 (pre-lottery) 
Beijing10_sales <- Beijing %>%
  filter(year == 2010) %>%
  group_by(MSRP) %>%
  summarize(count = sum(sales))

# merge the MSRP and sales
Beijing_pre <- left_join(uniqueMSRP_Beijing,
                         Beijing10_sales,
                         by = "MSRP") %>%
               replace_na(list(count = 0)) %>%
               arrange(MSRP)

# aggregate sales at each price for 2011 (post-lottery) 
Beijing11_sales <- Beijing %>%
  filter(year == 2011) %>%
  group_by(MSRP) %>%
  summarize(count = sum(sales))

# merge the MSRP and sales
Beijing_post <- left_join(uniqueMSRP_Beijing,
                          Beijing11_sales,
                         by = "MSRP") %>%
  replace_na(list(count = 0)) %>%
  arrange(MSRP)

#########################################
## Tianjin pre and post (Exercise 4.1) ##
#########################################
# keep 2010 and 2011 only
Tianjin <- Tianjin_sample %>%
  filter(year >= 2010 & year < 2012)

# collect unique MSRP values
uniqueMSRP_Tianjin <- data.frame(MSRP = unique(Tianjin$MSRP)) 

# aggregate sales at each price for 2010 (pre-lottery) 
Tianjin10_sales <- Tianjin %>%
  filter(year == 2010) %>%
  group_by(MSRP) %>%
  summarize(count = sum(sales))

# merge the MSRP and sales
Tianjin_pre <- left_join(uniqueMSRP_Tianjin,
                         Tianjin10_sales,
                         by = "MSRP") %>%
  replace_na(list(count = 0)) %>%
  arrange(MSRP)

# aggregate sales at each price for 2011 (post-lottery) 
Tianjin11_sales <- Tianjin %>%
  filter(year == 2011) %>%
  group_by(MSRP) %>%
  summarize(count = sum(sales))

# merge the MSRP and sales
Tianjin_post <- left_join(uniqueMSRP_Tianjin,
                          Tianjin11_sales,
                          by = "MSRP") %>%
  replace_na(list(count = 0)) %>%
  arrange(MSRP)

##################################
## Visualize Beijing Car Sales  ##
##################################
Beijing_distribution_pre <- Beijing_pre %>% uncount(count) # consult `help(uncount)` 
Beijing_distribution_post <- Beijing_post %>% uncount(count)
bdist <- ggplot() +
  geom_histogram(data = Beijing_distribution_pre,
                 aes(x = MSRP / 1000,   # Let price be in terms of 1000 RMB
                     y = ..density..),  # Normalize bars so their area sum to 1
                 binwidth = 20,         # Each bin has width of 2000 RMB
                 fill = "orange", color = "orange", alpha = 0.35) +
  geom_histogram(data = Beijing_distribution_post,
                 aes(x = MSRP / 1000,   # Let price be in terms of 1000 RMB
                     y = ..density..),  # Normalize bars so their area sum to 1
                 binwidth = 20,         # Each bin has width of 2000 RMB
                 fill = "steelblue", color = "steelblue", alpha = 0.35) +
  xlab("MSRP (1000 RMB)") +
  ylab("Density")
ggsave("output/Beijing_dist.png", width = 6, height = 3)
################################################
## Visualize Tianjin Car Sales (Exercise 4.2) ##
################################################

Tianjin_distribution_pre <- Tianjin_pre %>% uncount(count) 
Tianjin_distribution_post <- Tianjin_post %>% uncount(count)
bdist <- ggplot() +
  geom_histogram(data = Tianjin_distribution_pre,
                 aes(x = MSRP / 1000,
                     y = ..density..),
                 binwidth = 20,
                 fill = "orange", color = "orange", alpha = 0.35) +
  geom_histogram(data = Tianjin_distribution_post,
                 aes(x = MSRP / 1000,
                     y = ..density..),
                 binwidth = 20,
                 fill = "steelblue", color = "steelblue", alpha = 0.35) +
  xlab("MSRP (1000 RMB)") +
  ylab("Density")
ggsave("output/Tianjin_dist.png", width = 6, height = 3)

##################
## Exercise 4.3 ##
##################
# set the seed for reproducibility
set.seed(1)
# We will use the `rmultinom` function to construct our placebo.
# Imagine the same number of cars as in 2010. (see `size` argument)
# For each MSRP value, we will decide how many of these imaginary cars will 
# be sold at this price. The number of of these imaginary cars to be sold at 
# the particular MSRP value will be proportional to the actual number of cars 
# sold in the pre-lottery distribution. (see `prob` argument)
# We only want one placebo distribution. (see `n` argument)
placebo_1 <- data.frame(MSRP = Beijing_pre$MSRP,
                        count = rmultinom(n = 1,
                                          size = sum(Beijing_pre$count),
                                          prob = Beijing_pre$count))
placebo_2 <- data.frame(MSRP = Beijing_pre$MSRP,
                        count = rmultinom(n = 1,
                                          size = sum(Beijing_post$count),
                                          prob = Beijing_pre$count))
ggplot() +
  geom_histogram(data = placebo_1,
                 aes(x = MSRP / 1000,
                     y = ..density..), # Normalize bars so their area sum to 1 
                 binwidth = 20,
                 fill = "orange", color = "orange", alpha = 0.35) +
  geom_histogram(data = placebo_2,
                 aes(x = MSRP / 1000,
                     y = ..density..), # Normalize bars so their area sum to 1
                 binwidth = 20,
                 fill = "steelblue", color = "steelblue", alpha = 0.35) +
                   xlab("Support") +
                   ylab("Density") +
                   theme(axis.text = element_text(size = 12),
                         axis.title = element_text(size = 14))
ggsave("output/placebo_dist.png", width = 6, height = 3)

#########################################################
## Compute their optimal transport cost (Exercise 4.4) ##
#########################################################

bandwidths <- seq(0, 100000, by = 1000) # store all the bandwidths you want in this vector 
placebo <- diftrans(pre_main = placebo_1,
                         post_main = placebo_2,
                         var = MSRP,
                         bandwidth_seq = bandwidths)

empirical <- diftrans(pre_main = Beijing_pre,
                    post_main = Beijing_post,
                    var = MSRP,
                    bandwidth_seq = bandwidths)

ggplot() +
  geom_line(data = placebo,
                 aes(x = bandwidth,
                     y = main), 
                 color = "orange") +
  geom_line(data = empirical,
                 aes(x = bandwidth,
                     y = main), 
                 color = "steelblue") +
  xlab("Bandwidth") +
  ylab("Transport Cost") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
ggsave("output/cost_bandwidth.png", width = 6, height = 3)

## For which values of d is the placebo cost less than 0.05%?
max(which(placebo$main > 0.0005))

## Before-and-after estimate
empirical[which(empirical$bandwidth==17000), ]

################################################################
## Compute Differences-in-Transports Estimator (Exercise 4.5) ##
################################################################

dit <- diftrans(pre_main = Beijing_pre,
                     post_main = Beijing_post,
                     pre_control = Tianjin_pre,
                     post_control = Tianjin_post,
                     var = MSRP,
                     bandwidth_seq = seq(0, 50000, by = 1000),
                     conservative = TRUE)

set.seed(1)
placebo_Beijing_1 <- data.frame(MSRP = Beijing_pre$MSRP,
                            count = rmultinom(n = 1,
                                              size = sum(Beijing_pre$count),
                                              prob = Beijing_pre$count))
placebo_Beijing_2 <- data.frame(MSRP = Beijing_pre$MSRP,
                                count = rmultinom(n = 1,
                                                  size = sum(Beijing_post$count),
                                                  prob = Beijing_pre$count))
placebo_Tianjin_1 <- data.frame(MSRP = Tianjin_pre$MSRP,
                                count = rmultinom(n = 1,
                                                  size = sum(Tianjin_pre$count),
                                                  prob = Tianjin_pre$count))
placebo_Tianjin_2 <- data.frame(MSRP = Tianjin_pre$MSRP,
                                count = rmultinom(n = 1,
                                                  size = sum(Tianjin_post$count),
                                                  prob = Tianjin_pre$count))

dit_placebo <- diftrans(pre_main = placebo_Beijing_1,
                post_main = placebo_Beijing_2,
                pre_control = placebo_Tianjin_1,
                post_control = placebo_Tianjin_2,
                var = MSRP,
                bandwidth_seq = seq(0, 50000, by = 1000),
                conservative = TRUE)

dit_placebo$diff2d_abs <- abs(dit_placebo$diff2d)
ggplot() +
  geom_line(data = dit_placebo,
            aes(x = bandwidth,
                y = diff2d_abs), 
            color = "orange") +
  xlab("Bandwidth") +
  ylab("Absolute value of the placebo differences-in-transports estimator") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
ggsave("output/dit_placebo.png", width = 6, height = 3)

# For which values of d does the absolute value of 
#the placebo differences-in-transports estimator stay below 0.05%?
which(dit_placebo$diff2d_abs > 0.0005) # ignore temporary increases

# the largest value of diff2d_abs
dit_b <- dit %>%
  filter(bandwidth>10000)
max(dit_b$diff2d)


