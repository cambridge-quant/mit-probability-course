#---------------------------------------------------------
# File:   mit18_05_s22_studio5.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 5 ----

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio5-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Setup ----
# We have five types of dice: 4, 6, 8, 12, 20 sided.
# There is a prior distribution of the quantity of each die.
# One die is chosen at random and rolled repeatedly.
# Our job is to figure out which type of die was chosen.

DICE = c(4, 6, 8, 12, 20) # Fixed for the entire studio
DICE_TYPES = c('D4', 'D6', 'D8', 'D12', 'D20')

#-------------------------------
# Problem 0: List hypotheses and outcomes. ----
# See the instructions for this studio.

# 0a: List hypotheses.
studio5_problem_0a = function() {
  cat("\n----------------------------------\n")
  cat("Problem 0a: List hypotheses.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

}


# 0b: List outcomes.
studio5_problem_0b = function() {
  cat("-----\n")
  cat("0b. List outcomes.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

}


# 0c: Print likelihood table for one roll. ----
studio5_problem_0c = function() {
  cat("-----\n")
  cat("0c. Print likelihood table for one roll.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

}

#---------------------------
# Problem 1: Updating ----

# 1a: Run studio5_sample_code_example_2.
studio5_problem_1a = function() {
  cat("\n----------------------------------\n")
  cat("Problem 1a. Run example 2.\n")

  # Nothing more to do.
  cat("Read the code and looked at the plots.\n")
}


# Problem 1b: Updates, bar plots, stacked bar plot.
studio5_problem_1b = function(prior,
                              nrolls,
                              plot_individual_posteriors=FALSE) {
  cat("-----\n")
  cat("1b. Updates, bar plots, stacked bar plot.\n")

  # Arguments:
  #  prior = prior probilities for the type of die use to generate data
  #  nrolls = number of rolls to simulate
  #  plot_individual_posteriors = whether or not to make individual bar charts

  # For this problem force the chosen die to be 8-sided
  random_die = 8
  data_rolls = sample(1:random_die, size=nrolls, replace=TRUE)

  cat('The initial prior is\n')
  print(prior, digits=4)
  cat('nrolls =', nrolls, '\n')
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********


  cat('See plots\n')
}

# Problem 1c: Compare and contrast. ----
studio5_problem_1c = function() {
  cat("-----\n")
  cat("1c. Compare and contrast.\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("You need to put your answer here.\n")
}

# Problem 1d: Too certain a prior. ----
studio5_problem_1d = function() {
  cat("-----\n")
  cat("1d. Too certain a prior.\n")

  # Prior probability of D8 = 0
  prior = c(0.25, 0.25, 0, 0.25, 0.25)
  cat('Running studio5_problem_1b\n')
  studio5_problem_1b(prior, 20, FALSE)

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("You should put your answer here\n")
}


#---------------------------
# Problem 2 (OPTIONAL): Censored data. ----

# Problem 2a (OPTIONAL): List hypotheses, make likelihood table
studio5_problem_2a = function() {
  cat("\n----------------------------------\n")
  cat("OPTIONAL 2a. List hypotheses, outcomes, make likelihood table.\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}

# Problem 2b (OPTIONAL): Censored data. ----
studio5_problem_2b = function(prior, nrolls) {
  cat("-----\n")
  cat("OPTIONAL 2b: Censored data.\n")

  # Arguments:
  #  prior = prior probilities for the type of die use to generate data
  #  nrolls = number of rolls to simulate

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********



  cat('See plots\n')
}
