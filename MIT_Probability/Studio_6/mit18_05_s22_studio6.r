#---------------------------------------------------------
# File:   mit18_05_s22_studio6.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 6 ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Problem 0: Averaging normal distributions.
# See instructions for this studio
studio6_problem_0 = function() { 
  cat("\n----------------------------------\n")
  cat("Problem 0: Averaging normal distributions \n")

  # This will draw the pair of histograms needed for this problem
  source('mit18_05_s22_studio6_problem0_draw_plot.r')
  studio6_problem0_draw_plot(10, 6, 9, 10000)

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat('0. PUT YOUR COMPARISON OF THE HISTOGRAMS HERE.\n')
}

#---------------------------
# Problem 1: Cauchy distribution

# 1a. Formula for Cauchy pdf
studio6_problem_1a = function() {
  cat("\n----------------------------------\n")
  cat("Problem 1a. Formula for Cauchy pdf\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("1a. PUT THE FORMULA f(x | theta) FOR THE CAUCHY DISTRIBUTION HERE.\n")
}


# Problem 1b: Plot pdfs
studio6_problem_1b = function() {
  cat("-----\n")
  cat("1b. Plot pdf of Cauchy and standard normal\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********


  cat("1b. See plots\n")
}

# Problem 1c: Explain fat tails
studio6_problem_1c = function() {
  cat("-----\n")
  cat("1c. Explain fat tails\n")

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("1c. Answer: PUT YOUR EXPLANATION FOR FAT TAILS HERE.\n")
}

# Problem 1d: Average of Cauchy
studio6_problem_1d = function() {
  cat("-----\n")
  cat("1d. Average of Cauchy distributions\n")

  # Be sure the working directory is set to source file location
  # No code to write. Just run this code
  source('mit18_05_s22_studio6_problem_1d.r')

 
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("Notice that averaging the Cauchy distribution does not change the spread of the histogram. Averaging does not help us estimate the location of the pdf!\n")
}

#--------------------------------------
# Problem 2: Cauchy's lighthouse ----

# 2a: Load and plot data
studio6_problem_2a = function(data_frame_csv) {
  cat("\n----------------------------------\n")
  cat("Problem 2a. Load and plot data.\n")

  # Be sure the working directory is set to source file location

  # Arguments:
  #  data_frame_csv = name of the data file. (The grader will use a different data file than the test scripts.)

  # Load the data
  studio6_data_frame = read.csv(data_frame_csv)
  print(head(studio6_data_frame))
  position_data = studio6_data_frame[,'position']

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********


  cat("2a. See plot\n")
}

# Problem 2b:. Discretized Bayesian updates----
studio6_problem_2b = function(data_frame_csv) {
  cat("-----\n")
  cat("2b. Discretized Bayesian updates and MAP estimates.\n")

  # Arguments:
  #  data_frame_csv = name of the data file. (The grader will use a different data file than the test scripts.)

  # We give the unknown parameter theta a prior that follows a Uniform(theta_min, theta_max) distribution.
  # Use these values
  theta_min = -10
  theta_max = 10
  dtheta = 0.02 # Stepsize for discretizing the prior.

  # Load the data
  studio6_data_frame = read.csv(data_frame_csv)
  position_data = studio6_data_frame[,'position']

  # Do not change the above code.
  # ********* YOUR CODE FOR HERE***********



  # 2b(vi) Where to look
  cat("2b(vi). GIVE YOUR CONCLUSION ON WHERE TO LOOK.\n")
}

# Problem 2c:. OPTIONAL Explanation ----
studio6_problem_2c = function() {
  cat("-----\n")
  cat("Problem 2c (OPTIONAL). Explanation\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE***********

  cat("2c (OPTIONAL). PUT YOUR EXPLANATION HERE.\n")
}
