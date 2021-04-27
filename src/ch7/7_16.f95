! Road Traffic Density
!
! We don't always want to generate uniform probability density. Take for example
! the Poisson distribution. Write a function to evaluate the Poisson distribution for any
! k, t, and lamda. est your function by calculating the probability of 0, 1, 2, ..., 5 cars
! passing a particular point on a highway in 1 minute, given that lamda is 1.6 per minutes for
! that highway.

program poisson_test
    implicit NONE

    integer :: k
    real, parameter :: t = 1, lamda = 1.6
    real :: prob
    real :: poisson

    do k = 0,5
        prob = poisson(k, t, lamda)
        print 100, k, prob
        100 format("P(", I0, ") = ", F7.4)
    end do

end program

function poisson(k, t, lamda) result (P)
    ! Purpose:
    !
    !   To return the probability that the number of calls, k, is observed after a given
    !   time, t.
    !
    ! Data dictionary : Declare values used inside the function
    integer, intent(in) :: k         ! The number of observations in a given time
    real, intent(in) :: t            ! The amount of time passed
    real, intent(in) :: lamda        ! The paramater of a poisson distribution
    real :: P                        ! The probability that k observations occured in time t
    integer :: k_fact
    integer :: fact

    k_fact = fact(k)
    P = exp(-lamda * t) * (((lamda * t) ** k)/ k_fact)

end function

recursive function fact(x) result (x_fact)
    integer, intent(in) :: x
    integer :: x_fact

    if (x == 0) then
        x_fact = 1
        return
    end if
    x_fact = x * fact(x - 1)

end function


