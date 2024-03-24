#---------------------------------------------------------
# File:   mit18_05_s22_studio6-samplecode.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Sample code for 18.05 Studio 6
#--------------------------------
# 1. Discretizing continuous distributions
# 2. Bayesian updating
# 3. Maximum a posterior (MAP) estimates of the unknown parameter.

#---------------------------------------
# Suppose we are doing Bayesian updating with the following setup
# Unknown parameter: theta
# Range of theta:   [0, 1]
# Prior on theta:   f(theta) ~ beta(2, 2). (f(theta) = 6*theta*(1-theta))
# Likelihood:       f(x|theta) ~ Normal(mean=theta, variance=9)
# Data:             Given below

# Put the data in an array; find the number of data points.
data = c(2.1, 2, 2.1, 1.7, -0.1, 5, 2.7)
ndata = length(data)

#---------------------------
# 0. Plot the data. It is usually a good idea to simply plot data
plot(data, type='p', pch=19, col='blue', main='Plot of data')
     
#---------------------------
# 1. Discretizing the prior.
# We need a discretization stepsize. We'll use dtheta = 0.01
# First we discretize the range of theta
a = 2
b = 2
theta_min = 0.0
theta_max = 1.0
dtheta = 0.01
discrete_theta_range = seq(theta_min, theta_max, dtheta)

# Discretize the prior.
# Remember: this is a distribution of theta.
# The first array is 'unnormalized' because it doesn't sum to 1
unnormalized_prior = dbeta(discrete_theta_range, a, b)
# To normalize we want sum(prior) = 1
# (Note, for Bayesian updating, is is not necessary to normalize.)
discrete_prior = unnormalized_prior/(sum(unnormalized_prior))

#---------------------------
# 2. Update the prior one data value at a time. Save each posterior.

# Initialize matrix whose jth column will store the posterior distribution after the jth update We will use this to make plots at the end
posterior_mat = matrix(NA, nrow=length(discrete_theta_range), ncol=ndata )

# Update by looping over the data
sigma = 3.0  # This was given to us.
prior = discrete_prior
for (j in 1:ndata) {
  x = data[j]
  # dnorm can take an array of theta and will return an array of density values.
  likelihood = dnorm(x, mean = discrete_theta_range, sd = sigma)
  bayes_numerator = likelihood*prior
  posterior = bayes_numerator/(sum(bayes_numerator))
  posterior_mat[,j] = posterior

  # Make the current posterior the next prior
  prior = posterior
}

# Graph posteriors on one plot
# We start by finding minima and maxima to make sure all the graphs fit on the plot
ymin = 0
ymax = max(posterior_mat, discrete_prior)
# Create an empty plot with the correct ranges
plot_title = "Plot of all posteriors (and prior) for theta"
# Make an empty plot (type='n') setting ranges and labels
plot(c(theta_min, theta_max), c(ymin, ymax), type='n', xlab="theta", ylab="pdf", main = plot_title)
lines(discrete_theta_range, discrete_prior, col='red')
for (j in 1:ndata) {
  #R lets you cycle through its list of colors by using col=j
  lines(discrete_theta_range, posterior_mat[,j], col=j)
}

#---------------------------
# 3.  MAP Estimates
# If we have a posterior distribution for a parameter theta. The maximum a posteriori estimate (MAP estimate) of theta is the value of theta which has the maximum probability.

# Embedded in this section are two other lessons
# (a) How to divide the screen into two plots
# (b) How different choices of axis limits give different impressions of scale.

# Divide the screen into 1 row, 2 columns using parameter mfrow
# (Save the old parameter setting in opar, so we can restore them later)
# While we're changing parameters, we change the margins and place where axis labels are printed using parameter mar and mgp
opar = par(mfrow=c(1,2), mar=c(3.5,3.5,1.5,1), mgp=c(2.3,1,0))

# Find and plot all the MAP estimates
MAP_estimates = rep(0,ndata)
for (j in 1:ndata) {
  # Index of maximum probability
  k = which.max(posterior_mat[,j])
  # Pick theta with that max. probability
  MAP_estimates[j] = discrete_theta_range[k] 
}
plot_title = "Plot of all MAP estimates"
plot(MAP_estimates, type='p', col='blue', pch=19, xlab="index", main=plot_title)

# The plot just made of the map estimates gives a distorted impression of scale. We now make a better plot of all map estimates. This one uses the full y-range 0 to maxy

plot_title = "Better plot of MAP estimates"
maxy = max(MAP_estimates)
# Create an empty plot that uses the full range
plot(c(0,ndata), c(0,maxy), type='n', xlab="index", ylab='MAP_estimates', main=plot_title)
points(MAP_estimates, type='p', col='blue', pch=19)

# Restore the old parameter settings
par(opar)

# Print the final MAP Estimate
print(paste("Final map estimate: ", MAP_estimates[ndata]))

#---------------------------
# 4. Plot the last posterior put a vertical line at its MAP estimate
plot_title = paste("Plot of final posterior. MAP = ", MAP_estimates[ndata])
plot(discrete_theta_range, posterior_mat[,ndata], type='l',col='orange', lwd=2,  xlab='theta', ylab='posterior', main=plot_title)
abline(v=MAP_estimates[ndata], col='blue', lty='dashed')

