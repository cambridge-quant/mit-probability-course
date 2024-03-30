#---------------------------------------------------------
# File:   mit18_05_s22_studio8-test.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 8 test script

# Before running: clean your environment and source('mit18_05_s22_studio8.r')

#-----------------------
studio8_problem_1(6, 30, 3, 2, 10000)

#-----------------------
test_data_problem_2 = c(-1.387,  0.611,  0.688,  3.155,  1.476,
                        1.120,  2.529,  2.021,  1.797,  2.041,
                        -0.104,  2.501, -0.341,  1.094, -0.257,
                        3.322,  0.390,  1.292, -1.127,  4.253,
                        2.750,  1.411,  2.940,  2.236,  1.726,
                        0.741,  3.978, -0.269,  0.018, -1.984,
                        2.881, -0.190,  0.763,  2.911,  1.836,
                        0.511, -2.289,  3.038,  0.944,  2.179,
                        -5.863,  0.609, -3.401,  1.470,  2.048,
                        0.463,  2.170, -2.437, -0.586,  1.119)

studio8_problem_2(test_data_problem_2, 0.3, 2, 0.05)
studio8_problem_2(test_data_problem_2, 1, 2, 0.05)

#-----------------------
studio8_problem_3("mit18_05_s22_studio8Problem3_test.tbl", 0.05)

#-----------------------
T1 = c(1.089, -0.167, 0.915, 2.444, 1.332, 2.805, 1.560, 3.067, 1.881)
T2 = c(1.041, 2.146, 0.652, 1.893, 1.134, 3.558, 0.896, 2.169, 2.924)
T3 = c(4.246, 0.489, 4.229, 4.850, 0.498, 1.750, 4.433, 3.728, 1.447)

studio8_problem_4(T1, T2, T3, 0.05)

