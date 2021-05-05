!! Linear Least Squares Fit
!!
!! Develop a subroutine that will calculate slope m and intercept b of the least-squares line 
!! that best fits an input data set. The input data points will be passed to the subroutine in
!! two input arrays, X and Y

!! The equations describing the slope and intercept of the least-squares lines are:

!! y = mx + b
!!
!! m = (SUM(x*y) - SUM(x)*y_bar)/(SUM(x**2) - SUM(x)*x_bar)
!!
!! b = y_bar - m * x_bar
!!


program linear_least_squares_test

use linear_least_squares
implicit none

    real(real32), dimension(20) :: X_, Y_
    ! real(real32), dimension(19) :: test_
    real(real32) :: m_, b_

    X_ = [-4.91, -3.84, -2.41, -2.62, -3.78, -0.52, -1.83, -2.01, 0.28, &
           1.08, -0.94, 0.59, 0.69, 3.04, 1.01, 3.60, 4.53, 5.13, 4.43, 4.12]

    Y_ = [-8.18, -7.49, -7.11, -6.15, -5.62, -3.30, -2.05, -2.83, -1.16, &
        0.52, 0.21, 1.73, 3.96, 4.26, 5.75, 6.67, 7.70, 7.31, 9.05, 10.95]

    call lls_fit(X_, Y_, m_, b_)


    1 format("y = ", F7.3, "x + ", F7.3)
    print 1, m_, b_

end program