#---------------------------------------------------------
# File:   mit18_05_s22_studio9-test.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 9 test script

# Before running: clean your environment and source('mit18_05_s22_studio9.r')

#-----------------------
theta_vals =  c(0, 0.2, 0.4, 0.6, 0.8, 1)
theta_prior = c(0.02, 0.02, 0.02, 0.7, 0.2, 0.04)
n_trials = 10000
sigma = 2
n_data = 256
confidence = 0.95

studio9_problem_1a(theta_vals, theta_prior, sigma, n_data, confidence, n_trials) 
studio9_problem_1b(theta_vals, theta_prior, sigma, n_data, confidence, n_trials)

xbar = 0.2
studio9_problem_1c(theta_vals, theta_prior, sigma, n_data, confidence, xbar)


#-----------------------
studio9_problem_2(0.55, 400)
