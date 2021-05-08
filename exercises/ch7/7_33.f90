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



end program
