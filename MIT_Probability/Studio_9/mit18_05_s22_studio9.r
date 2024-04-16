#---------------------------------------------------------
# File:   mit18_05_s22_studio9.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 9 ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Problem 1: Simulated confidence intervals for normal data
# See instructions for this studio

# Problem 1a: Simulated type 1 CI error rate for z-confidence intervals
studio9_problem_1a = function(theta_vals, theta_prior, sigma, n_data, confidence, n_trials) { 
  cat("\n----------------------------------\n")
  cat("Problem 1a: Simulated type 1 CI error rate for z-confidence intervals \n")

  # Arguments:
  #  theta_vals = possible values for the mean of the normal distribution
  #  theta_prior = probabilities for choosing a theta from theta_vals
  #  sigma = standard deviation of the normal distribution
  #  n_data = the number of data values in each trial
  #  confidence = the confidence level, e.g. 0.95, 0.9 etc
  #  n_trials = number of trials in the simulation


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}


# Problem 1b: Simulated type 1 CI error rate for t-confidence intervals
studio9_problem_1b = function(theta_vals, theta_prior, sigma, n_data, confidence, n_trials) { 
  cat("\n----------------------------------\n")
  cat("Problem 1b: Simulated type 1 CI error rate for t-confidence intervals \n")

  # Arguments:
  #  theta_vals = possible values for the mean of the normal distribution
  #  theta_prior = probabilities for choosing a theta from theta_vals
  #  sigma = standard deviation of the normal distribution
  #  n_data = the number of data values in each trial
  #  confidence = the confidence level, e.g. 0.95, 0.9 etc
  #  n_trials = number of trials in the simulation

  # Remember: in this problem, you use sigma to generate the data, but not in computing the confidence interval.
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}


# Problem 1c: Bayesian updating and probability of hypotheses
studio9_problem_1c = function(theta_vals, theta_prior, sigma, n_data, confidence, xbar) { 
  cat("\n----------------------------------\n")
  cat("Problem 1c: Bayesian updating and probability of hypotheses\n")

  # Arguments:
  #  theta_vals = possible values for the mean of the normal distribution
  #  theta_prior = probabilities for choosing a theta from theta_vals
  #  sigma = standard deviation of the normal distribution
  #  n_data = the number of data values in each trial
  #  confidence = the confidence level, e.g. 0.95, 0.9 etc
  #  xbar = the mean of the data

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}


#---------------------------
# OPTIONAL Problem 2: Simulated polling confidence intervals
# See instructions for this studio

studio9_problem_2 = function(true_theta, n) { 
  cat("\n----------------------------------\n")
  cat("Problem 2: Simulated polling confidence interval\n")

  # Arguments
  # true_theta = true proportion who prefer Lincoln. Use this to generate your simulated polling data
  # n = size of the sample

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
    
}
