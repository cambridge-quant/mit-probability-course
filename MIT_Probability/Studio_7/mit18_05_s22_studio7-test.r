#---------------------------------------------------------
# File:   mit18_05_s22_studio7-test.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------
# Studio 7 test script

# Before running: clean your environment and source('mit18_05_s22_studio7.r')

#-----------------------
studio7_problem_1(0.7, 0.05, 18)

#-----------------------
studio7_problem_2(0.7, 0.05, 18, 1000, c(0.3,0.7))

#-----------------------
studio7_problem_3a(0.7, 0.05, 18, 1000)
studio7_problem_3b(0.7, 0.05, 18, 1000)
studio7_problem_3c()
studio7_problem_3d()

#-----------------------
# OPTIONAL 
studio7_problem_4(0.7, 0.05, 18, c(0.1, 0.9))
# Compare the problem 4 results with the simulated results in problem 2.
studio7_problem_2(0.7, 0.05, 18, 10000, c(0.1, 0.9))
