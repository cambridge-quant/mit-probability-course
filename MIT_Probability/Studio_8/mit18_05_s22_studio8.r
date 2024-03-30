#---------------------------------------------------------
# File:   mit18_05_s22_studio8.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 8  ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Summary of questions
# 1. Simulation of the f-statistic: draw histogram and graph
# 2. z-test. Code by hand a z-test
# 3. Chi-square test for independence
# 4. ANOVA. using aov()

#--------------------------------------
# Problem 1: Simulate F statistic
# See instructions for this studio

studio8_problem_1 = function(n, m, mu, sigma, n_trials) {
  cat("----------------------------------\n")
  cat("Problem 1: Simulate F statistic\n")

  # Arguments
  # n = number of groups
  # m = number of data points in each group (same for all groups)
  # mu = common mean: same for each group
  # sigma = common standard deviation (same for each group)
  # n_trials = number of trials to run in the simulation
  

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  
  cat("See plots\n")
}

#--------------------------------------
# Problem 2: Code z-test by hand
# See instructions for this studio

studio8_problem_2 = function(data, mu0, known_sigma, alpha) {
  cat("----------------------------------\n")
  cat("Problem 2: Code z-test by hand\n")

  # Arguments
  # data = list of sample values
  # mu0 = hypothesized mean in the null hypothesis
  # known_sigma = known value of the standard deviation
  # alpha = significance for the test

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}

#--------------------------------------
# Problem 3: Chi-square test for independence
# See instructions for this studio

studio8_problem_3 = function(data_table_file, alpha) {
  cat("----------------------------------\n")
  cat("Problem 3: Chi-square test for independence\n")

  # Arguments
  # data_table_file = data file containing a two-by-two contingency table
  # alpha = significance for the test
  
  contingency_tbl = read.table(data_table_file)
  print(contingency_tbl)

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
}

#--------------------------------------
# Problem 4: ANOVA using aov()
# See instructions for this studio

studio8_problem_4 = function(T1, T2, T3, alpha) {
  cat("----------------------------------\n")
  cat("Problem 4: ANOVA using aov()\n")

  # Arguments
  # T1, T2, T3 = 3 sets of data. All the same length
  # alpha = significance of the test
  
  # Put the data into a data.frame.
  # Data frames are tables. We make columns for procedure and pain
  procedure = c(rep('T1',length(T1)),rep('T2',length(T2)),rep('T3',length(T3)))
  pain = c(T1,T2,T3)
  data_pain = data.frame(procedure, pain)

  #Look at the printout. Notice how the frame has columns procedure and pain. The ANOVA test will be to see if there is a difference between the average pain from each procedure.
  print(data_pain)

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}
