#---------------------------------------------------------
# File:   mit18_05_s22_studio4.r
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 4 ----

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio4-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#---------------------------------
# Problem 1: Barto and Axel simulated covariance and correlation. ----
# See the instructions for this studio.

# 1a: Simulated covariance and correlation.
studio4_problem_1a = function(n_together, n_Barto_alone, ntrials) {
  cat("\n----------------------------------\n")
  cat("Problem 1a. Barto and Axel simulated covariance and correlation.\n")

  # Arguments:
  #   n_together = number of games Axel and Barto play together
  #   n_Barto_alone = number of games Barto plays after playing with Axel
  #   ntrials = number of trials to run in one simulation.

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  one_bet_values = c(1, -1)
  one_bet_probs = c(18/38, 20/38)

  x = sample(one_bet_values, ntrials*n_together,
             replace=TRUE, prob=one_bet_probs)
  together_data = matrix(x, ncol=ntrials)

  x = sample(one_bet_values, ntrials*n_Barto_alone,
             replace=TRUE, prob=one_bet_probs)
  alone_data = matrix(x, ncol=ntrials)

  # We use colSums to total Axel and Barto's winnings for each trial
  axel_outcomes = colSums(together_data)
  barto_outcomes = axel_outcomes + colSums(alone_data)

  axel_mean = mean(axel_outcomes)
  barto_mean = mean(barto_outcomes)
  axel_var = var(axel_outcomes)
  barto_var = var(barto_outcomes)
  ab_cov = cov(axel_outcomes, barto_outcomes)
  ab_cor = cor(axel_outcomes, barto_outcomes)

  #------
  # Use the cat statements below to print your results. You will need to insert your variables names where indicated.
  cat("For n_together =", n_together, " and n_Barto_alone =", n_Barto_alone, "we have\n")
  cat("Axel's sample mean is", axel_mean, '\n')
  cat("Barto's sample mean is", barto_mean, '\n')
  cat("Axel's sample variance is", axel_var, '\n')
  cat("Barto's sample variance is", barto_var, '\n')
  cat("Their sample covariance is", ab_cov, '\n')
  cat("Their sample correlation is", ab_cor, '\n')
}

# 1b. Print your description of what happens as n_Barto_alone increases
studio4_problem_1b = function() {
  cat("-----\n")
  cat("1b. Describe behavior as n_Barto_alone increases\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  cat('As n_Barto_alone increases, the covariance, decreases. \n')
  cat('As n_Barto_alone increases, the correlation, decreases. \n')
}

#-------------------------------------
# Problem 2: Simulated central limit theorem. ----
# This is cool and shouldn't take a lot of code.
studio4_problem_2 = function(n_bets_per_trial, ntrials) {
  cat("\n----------------------------------\n")
  cat("Problem 2. Simulated central limit theorem.\n")

  # Arguments:
  #   n_bets_per_trial = the number of bets in each trial
  #   n_trials = number of trials

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  one_bet_values = c(1, -1)
  one_bet_probs = c(18/38, 20/38)

  x = sample(one_bet_values, ntrials*n_bets_per_trial,
             replace=TRUE, prob=one_bet_probs)
  data = matrix(x, ncol=ntrials)
  outcomes = colSums(data)
  hist(outcomes, col='orange', freq=FALSE)
  x = seq(min(outcomes)-1, max(outcomes)+1, 0.01)
  lines(x, dnorm(x, -2*n_bets_per_trial/38, sqrt(n_bets_per_trial*360/361)), col='blue', lwd=2)

  #------
  cat('See plot\n')
}
