!! Guassian Normal Distribution

!!
!! We can generate a standard normaL distribution from a uniform distribution in the 
!! range [-1, 1) as follows:
!!
!! 1. Select two uniform random variables, x1 and x2 from the range [-1, 1) such that x1^2 + x2^2 < 1.
!!2. Then, each of the values y1 and y2 in the following equations below will be a normally distributed
!! random variable:

!!
!! y1 = sqrt(-2*ln(r)/r) * x1
!! y2 = sqrt(-2*ln(r)/r) * x2
!!
!! where r = x1^2 + x2^2

program gaussian_distribution

use foreng_math_stats
use foreng_math_prob

implicit none

    integer, parameter :: SIZE = 10000000
    real(real64), dimension(SIZE) :: test_normal
    real(real64) :: sigma, mu
    integer :: i

    do i = 1,SIZE

        test_normal(i) = standard_normal_rand()

    end do

    mu = mean(test_normal)
    sigma = std_dev(test_normal)

    print *, "Average = ", mu
    print *, "std_dev = ", sigma

    ! print *, "test_normal = ", test_normal


contains 

    function standard_normal_rand() result (x)

        real(real64) :: x 

        real(real64) :: x_1, x_2, r

        test_xs: do

            x_1 = urand_r64(-1._real64, 1._real64)
            x_2 = urand_r64(-1._real64, 1._real64)

            r = x_1*x_1 + x_2*x_2

            if (r >= 1) then
                cycle test_xs
            else
                exit test_xs
            end if
        end do test_xs

        x = sqrt( (-2._real64 * log(r) ) / r ) * x_1        

    end function


end program
