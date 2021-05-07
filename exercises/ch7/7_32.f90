!! Generate an array containing 10,000 random numbers between 0.0 and 1.0. 
!! Then, use the statistics subroutines to calculate the average and standard deviation 
!! of the values in the array.
!! The theoretical stddev is 1/sqrt(2)
!! the theoretical average is 0.5

program seven_thirty_two

use foreng_math

implicit none

    real(real64), dimension(100) :: x
    real(real64) :: sigma, mu !! standard deviation and average

    call fill_urand(x, 0._real64, 1._real64)

    mu = mean(x)
    sigma = std_dev(x)

    print *, "average = ", mu
    print *, "sigma = ", sigma


end program