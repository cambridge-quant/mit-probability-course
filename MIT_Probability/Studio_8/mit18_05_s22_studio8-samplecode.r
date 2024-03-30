#---------------------------------------------------------
# File:   mit18_05_s22_studio8-samplecode.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Sample code for 18.05 Studio 8
#--------------------------------
# BE SURE TO SET THE WORKING DIRECTORY TO SOURCE FILE LOCATION

# 1. Simulation of t-statistics:
# 2. Run one-sample t tests (two-sided, right-sided, left-sided)
# 3. Run a two-sample t-test for equal means of two data sets
# 4. Use aov() to run ANOVA for equal means among more than 2 data sets

# ---------------------------- #
# Simulation: The t statistic of a normal sample should follow a t distribution with the correct degrees of freedom.

# Each simulation does the following
# 1. Generate n_samples values from Normal(mu0, sigma^2) distribution.
# 2. Compute the t statistic for H0: mu = mu0.
# Then we run 10000 simulations; plot a density histogram of the t statitisc and superimpose the graph of the pdf of t with n_samples-1 degrees of freedom.

# Set parameters
mu0 = 4
sigma = 3
n_samples = 8
n_simulations = 10000

# Run simulations, save the results in tstats
t_stats = rep(0, n_simulations)
for (j in 1:n_simulations) {
  x = rnorm(n_samples, mu0, sigma)
  t = mean(x-mu0)/sqrt(var(x)/n_samples)
  t_stats[j] = t
}

# Get x, y for the graph of the pdf
df = n_samples-1
x = seq(-4, 4, 0.001)
y = dt(x, df)

# Draw the density histogram (freq=FALSE) and graph
y_max = max(y)  # Used to set plot range
bin_width = 0.3  # For histogram. 0.3 bin_width determined by trial and error
breaks = seq(min(t_stats), max(t_stats)+bin_width, bin_width)
hist(t_stats, breaks=breaks, xlim=c(-5,5), ylim=c(0,1.1*y_max), freq=FALSE)
lines(x, y, col='orange', lwd=2)

# ----------------------- #
# One-sample t-test

# Read the data in from a file
mu0 = 0  # Null hypothesis
x = read.table('mit18_05_s22_studio8-onesampledata.txt', )
results_two_side = t.test(x, mu=mu0) # It's this easy: two-sided test

# Printing the results gives a lot of information
print(results_two_side)

# We can extract the p-value as follows
p_value = results_two_side$p.value
cat('Two-sided p-value =', p_value, '\n')

# We can run right and left-sided tests also
results_right = t.test(x,mu=mu0, alternative='greater') 
print(results_right)
p_value = results_right$p.value
cat('Right-sided p-value =', p_value, '\n')

results_left = t.test(x,mu=mu0, alternative='less')
print(results_left)
p_value = results_left$p.value
cat('Left-sided p-value =', p_value, '\n')

#--------
# Check results by hand
data = x[,1]
n_samples = length(data)
df = n_samples - 1
t = (mean(data)-mu0)/sqrt(var(data)/n_samples)
p2 = 2*(1-pt(abs(t), df))
cat('Two-sided test: t:', t, 'p', p2, '\n')
pR = 1-pt(abs(t), df)
cat('Right-sided test: t:', t, 'p', pR, '\n')
pL = pt(abs(t), df)
cat('Left-sided test: t:', t, 'p', pL, '\n')
#----------

#----------------------#
# Two-sample t-test
x = c(10,12,13,12,9,8,16,12,22,13,8)
y = c(6,7,6,4,5,0,11,5,9,5,5,3,5,5,3,3,7,8,4,7,3,3)

# This is similar to the one-sample t-test

# Two-sided test. Note unequal lengths give decimal for df.
results_two_side = t.test(x, y)  
print(results_two_side)

p_value = results_two_side$p.value
cat('Two-sided p-value =', p_value, '\n')

# Right-sided test: first mean > second mean
results_right = t.test(x, y, alternative="greater")  
print(results_right)
p_value = results_right$p.value
cat('Right-sided p-value =', p_value, '\n')

# Left sided test: first mean < second mean
results_left = t.test(x, y, alternative="less")  
print(results_left)
p_value = results_left$p.value
cat('Left-sided p-value =', p_value, '\n')

#----------------------#
# ANOVA (F test for equal means) using aov
# First we construct some sets of data 
shepherd = c(1.3, 1.5, 1.7, 1.3)
collie = c(2.3, 1.1, 2.1, 1.8)
poodle = c(0.9, 1.1, 1.1, 0.7)

# Then we pack it into a data.frame (We could build this easier with a little code. We'll do it in this way just to avoid distractions)
dog = c('shepherd', 'shepherd', 'shepherd', 'shepherd', 
         'collie', 'collie', 'collie', 'collie',
         'poodle', 'poodle', 'poodle', 'poodle')
allergy = c(shepherd, collie, poodle)
allergy_data = data.frame(dog, allergy)
print(allergy_data)

# We use aov to run the analysis of variance test
# allergy~dog is a 'linear formula'
# allergy is the 'response' variable --this goes on the left
# dog is the 'explanatory' variable this goes on the right.
# aov() tests for whether some of the differences in the allergic response can be attributed to the breed of dog. (Our Null hypothesis is that all the dogs are equal, so none of the difference in allergies is explained by the breed.

aov_results = aov(allergy~dog, data=allergy_data)

# The summary gives information including the p-value
cat("\nSummary:\n")
print(summary(aov_results))

# To actually get the p-value is a bit of an ordeal. Here are two ways to do this.
# 1. Capture the summary and get the p-value from it.
s = summary(aov_results)
a = s[[1]]$'Pr(>F)'  # This is potentially a list
p_value = a[1]
cat('\np-value using summary():', p_value,'\n')

# 2. Use the anova function on aov.data and get the p-value from the result.
s2 = anova(aov_results)
a = s2$'Pr(>F)'
p_value2 = a[1]
cat('p-value using anova():', p_value2,'\n')

#----------------------#
cat("\nThe file class19.r posted in our usual place has examples of\n", "chisq.test   --chi-square tests\n", "aov  -- ANOVA\n")
