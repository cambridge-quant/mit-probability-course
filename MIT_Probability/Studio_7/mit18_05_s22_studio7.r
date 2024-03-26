#---------------------------------------------------------
# File:   mit18_05_s22_studio7.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------
# Studio 7  ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Problem 1: Significance and power/type 1 and type 2 errors
# See instructions for this studio

studio7_problem_1 = function(theta_HA, alpha, n_tosses) {
  cat("----------------------------------\n")
  cat("Problem 1: Rejection region, actual significance, power\n")

  # **** Be careful to avoid an 'off by 1 error' here.

  # Arguments:
  #   theta_HA = the probability of heads in the alternate hypothesis
  #   alpha = significance level for our significance test
  #   n_tosess = the number of tosses in one trial

  theta_H0 = 0.5 # probability of heads in the null hypothesis

  # Enforce theta_HA > theta_H0
  if (theta_HA < theta_H0) {
    warning("Problem 1: We require theta_HA > theta_H0", immediate.=TRUE)
    return()
  }

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********


  cat("  Rejection region: ", "PRINT THE REJECTION REGION","\n")
  cat("  True significance=", "PRINT THE TRUE SIGNIFICANCE",'\n')
  cat("  Power=", "PRINT THE POWER",'\n')
}


#--------------------
# Problem 2: Simulation with a known prior.
# See instructions for this studio

studio7_problem_2 = function(theta_HA, alpha, n_tosses, n_trials, secret_prior) {
  cat("----------------------------------\n")
  cat("Problem 2: Simulation with a mixture of coins\n")

  # Arguments:
  #   theta_HA = the probability of heads in the alternate hypothesis
  #   alpha = significance level for our significance test
  #   n_tosses = the number of tosses in one trial
  #   n_trials = the number of trials in the simulation
  #   secret_prior = the secret prior used to pick the type of coin for each trial = c(prob. of H0, prob of HA)

  theta_H0 = 0.5

  # Enforce theta_HA > theta_H0
  if (theta_HA < theta_H0) {
    warning("Problem 1: We require theta_HA > theta_H0", immediate.=TRUE)
    return()
  }


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********



  cat("  theta_HA=", theta_HA, ", alpha=", alpha, '\n', sep='')
  cat("  n_tosses=", n_tosses, ", n_trials=", n_trials, '\n', sep='')
  cat("  secret_prior=", secret_prior, '\n')
  cat("  Number of rejections: ", "PRINT NUMBER OF REJECTIONS", '\n')
  cat("  Number of type 1: ", "PRINT NUMBER OF TYPE 1 ERRORS", '\n')
  cat("  Number of type 2: ", "PRINT NUMBER OF TYPE 2 ERRORS", '\n')
  cat("  P(rejection | H0): ", "PRINT THE PROBABILITY", '\n')
  cat("  P(H0 | rejection): ", "PRINT THE PROBABILITY", '\n')
  cat("  P(rejection | HA): ", "PRINT THE PROBABILITY", '\n')
  cat("  P(HA | rejection): ", "PRINT THE PROBABILITY", '\n')
  cat("  P(rejection): ", "PRINT THE PROBABILITY", '\n')
}

#--------------------
# Problem 3: Simulation with only fair coins
# See instructions for this studio

studio7_problem_3a = function(theta_HA, alpha, n_tosses, n_trials) {
  cat("----------------------------------\n")
  cat("3a: Simulation with all fair coins: explain results\n")

  secret_prior = c(1.0, 0) # Prob of H0, prob of HA
  cat("----\n")
  cat("# Problem 2 called from problem 3a\n")
  studio7_problem_2(theta_HA, alpha, n_tosses, n_trials, secret_prior)
  cat("----\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("  EXPLAIN THE RESULTS\n")
}

studio7_problem_3b = function(theta_HA, alpha, n_tosses, n_trials) {
  cat("-----\n")
  cat("3b: Simulation with all unfair coins: explain results\n")

  secret_prior = c(0.0, 1.0) # Prob of H0, prob of HA
  cat("----\n")
  cat("# Problem 2 called from problem 3b\n")
  studio7_problem_2(theta_HA, alpha, n_tosses, n_trials, secret_prior)
  cat("----\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("  EXPLAIN THE RESULTS\n")
}

studio7_problem_3c = function() {
  cat("-----\n")
  cat("3c: Explain difference between P(H0 | rejection) and significance\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("  EXPLAIN THE DIFFERENCE\n")
}

studio7_problem_3d = function() {
  cat("-----\n")
  cat("3d: Shout it out!\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("  SHOUT\n")
  cat("  speak\n")
}


#--------------------
# OPTIONAL Problem 4: Use Bayes theorem
# See instructions for this studio

studio7_problem_4 = function(theta_HA, alpha, n_tosses, prior) {
  cat("----------------------------------\n")
  cat("OPTIONAL 4: Use Bayes theorem\n")

  theta_H0 = 0.5

  # Enforce theta_HA > theta_H0
  if (theta_HA < theta_H0) {
    warning("Problem 1: We require theta_HA > theta_H0", immediate.=TRUE)
    return()
  }

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********


  cat("  true P(H0 | rejection): ", "PRINT THE VALUE" , '\n')
  cat("  true P(HA | rejection): ", "PRINT THE VALUE", '\n')
}
