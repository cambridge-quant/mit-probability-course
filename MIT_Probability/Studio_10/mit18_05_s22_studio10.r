#---------------------------------------------------------
# File:   mit18_05_s22_studio10.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 10 ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# **** matrixStats package *****
# You should used the Package tab in R Studio to install matrixStats.
library('matrixStats')

#--------------------------------------
# Problem 1: Simulated type 1 bootstap CI error rate for normal data
# See instructions for this studio

studio10_problem_1 = function(true_mean, true_sd, n_data, n_boot, n_trials, confidence) {
  cat("\n----------------------------------\n")
  cat("Problem 1: Simulated type 1 empirical bootstrap CI error rate: normal distribution\n")

  # Arguments:
  # true_mean = the mean of the normal distribution used to generate the data
  # true_sd = the standard deviation of the normal distribution used to generate the data
  # n_data = number of values in one sample. (Original generated using a normal distribution.)
  # n_boot = number of bootstrap samples to use in each trial
  # n_trials = number of trials to run in simulation
  # confidence = the bootstrap confidence level


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}

#--------------------------------------
# Problem 2: Simulated type 1 bootstap CI error rate for log normal data
# See instructions for this studio

studio10_problem_2a = function(meanlog, sdlog) {
  cat("\n----------------------------------\n")
  cat("Problem 2a: Print true mean and true std of the log normal distribution\n")

  # Arguments:
  # meanlog = value of meanlog parameter in rlnorm
  # sdlog = value of sdlog parameter in rlnorm

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}

studio10_problem_2b = function(meanlog, sdlog, n_data, n_boot, n_trials, confidence) {
  cat("-----\n")
  cat("Problem 2b: Simulated type 1 empirical bootstrap CI error rate: log normal distribution\n")

  # Arguments:
  # meanlog = value of meanlog parameter in rlnorm
  # sdlog = value of sdlog parameter in rlnorm
  # n_data = number of values in one sample. (Original generated using a log normal distribution.)
  # n_boot = number of bootstrap samples to use in each trial
  # n_trials = number of trials to run in simulation
  # confidence = the bootstrap confidence level


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}
