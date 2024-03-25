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

  cat('The variance decreases when averages from multiple draws are used.\n')
}

#---------------------------
# Problem 1: Cauchy distribution

# 1a. Formula for Cauchy pdf
studio6_problem_1a = function() {
  cat("\n----------------------------------\n")
  cat("Problem 1a. Formula for Cauchy pdf\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("1a. f(x | theta) = (pi * (1 + (x - theta)^2))^-1.\n")
}


# Problem 1b: Plot pdfs
studio6_problem_1b = function() {
  cat("-----\n")
  cat("1b. Plot pdf of Cauchy and standard normal\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  xmin = -3
  xmax = 3
  x = seq(xmin, xmax, 0.01)
  y_norm = dnorm(x, 0, 1)
  y_cauchy = dcauchy(x, 0, 1)
  plot(x, y_norm, type='l', xlab='theta', ylab='pdf', col='orange', lwd=2)
  lines(x, y_cauchy, col='blue', lwd=2)

  cat("1b. See plots\n")
}

# Problem 1c: Explain fat tails
studio6_problem_1c = function() {
  cat("-----\n")
  cat("1c. Explain fat tails\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("1c. Answer: The peak of the Cauchy distribution is less than the Normal distribution.\n")
  cat("1c. But they must have equal area since the total area = total probability = 1.\n")
  cat("1c. Therefore the Cauchy distribution must have fatter tails.\n")
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
  plot(position_data, type='p', pch=19, col='blue', main='Plot of data')

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
  ndata = length(position_data)
  discrete_theta_range = seq(theta_min, theta_max, dtheta)
  unnormalized_prior = dunif(discrete_theta_range, theta_min, theta_max)
  discrete_prior = unnormalized_prior/(sum(unnormalized_prior))
  posterior_mat = matrix(NA, nrow=length(discrete_theta_range), ncol=ndata)
  scale = 1.0
  prior = discrete_prior
  for (j in 1:ndata) {
    x = position_data[j]
    likelihood = dcauchy(x, location = discrete_theta_range, scale = scale)
    bayes_numerator = likelihood*prior
    posterior = bayes_numerator/(sum(bayes_numerator))
    posterior_mat[,j] = posterior
    prior = posterior
  }

  ymin = 0
  ymax = max(posterior_mat, discrete_prior)
  plot(c(theta_min, theta_max), c(ymin, ymax), type='n', xlab="theta", ylab="pdf")
  lines(discrete_theta_range, discrete_prior, col='red')
  for (j in 1:ndata) {
    lines(discrete_theta_range, posterior_mat[,j], col=j)
  }

  MAP_estimates = rep(0, ndata)
  for (j in 1:ndata) {
    k = which.max(posterior_mat[,j])
    MAP_estimates[j] = discrete_theta_range[k]
  }
  plot(MAP_estimates, type='p', col='blue', pch=19, xlab="index")

  plot(discrete_theta_range, posterior_mat[,ndata], type='l',col='orange', lwd=2,  xlab='theta', ylab='posterior')
  abline(v=MAP_estimates[ndata], col='blue', lty='dashed')

  # 2b(vi) Where to look
  cat("2b(vi). You should look at =", MAP_estimates[ndata], "\n")
}

# Problem 2c:. OPTIONAL Explanation ----
studio6_problem_2c = function() {
  cat("-----\n")
  cat("Problem 2c (OPTIONAL). Explanation\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE***********

  cat("2c (OPTIONAL). PUT YOUR EXPLANATION HERE.\n")
}
