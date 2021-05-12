! Derivative of a Function. Write a subroutine to calculate the
! derivative of a double precision real function f(x) at position x = x_0.

! The calling arguments to the subroutine should be the 
! function f(x), the location x_0 at which to evaluate the function, and the step 
! size dx to use in the evaluation.

program deriv_sub

use iso_fortran_env
use foreng_numeric_calculus


implicit none

real(real64) :: dydx, x_0 = 0, dx = 1D-315
real(real64), external :: poly,  test_fun

dydx = calc_derivative(test_fun, x_0, dx)

print *, "derivative of x**2 at x_0 = ", x_0, " is ", dydx
! print *, "200cos(20x) at x_0", 200.0 * cos(20.0 * x_0)


end program

real(8) function poly(x)
    real(8), intent(in) :: x
    ! print *, "poly called"
    poly = x**2 

    ! print *, x, "^2 = ", x**2

end function

real(8) function test_fun(x)
    real(8), intent(in) :: x
    test_fun = 10.* sin(20.0*x)
end function