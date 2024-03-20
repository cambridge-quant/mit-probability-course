#---------------------------------------------------------
# File:   mit18_05_s22_studio5-test.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 5 test script

# Before running: clean your environment and source('mit18_05_s22_studio5.r')

#-----------------------
studio5_problem_0a()
studio5_problem_0b()
studio5_problem_0c()

#-----------------------
studio5_problem_1a()

prior = c(0.2, 0.2, 0.2, 0.2, 0.2)
studio5_problem_1b(prior, 8, TRUE)

# The following can be used to answer problem 1c.
prior = c(0.2, 0.2, 0.2, 0.2, 0.2)
studio5_problem_1b(prior, 20, FALSE)

prior = c(0.001, 0.001, 0.001, 0.001, 0.996)
studio5_problem_1b(prior, 20, FALSE)

studio5_problem_1c()
studio5_problem_1d()

#-----------------------
# Problem 2 is optional
studio5_problem_2a()

prior=c(0.2, 0.2, 0.2, 0.2, 0.2)
studio5_problem_2b(prior, 30)
studio5_problem_2b(prior, 200)

#-----------------------
