! Derivative of a Function. Write a subroutine to calculate the
! derivative of a double precision real function f(x) at position x = x_0.

! The calling arguments to the subroutine should be the 
! function f(x), the location x_0 at which to evaluate the function, and the step 
! size dx to use in the evaluation.

program deriv_sub

use iso_fortran_env


implicit none

real(real64) :: dydx, x_0 = 0, dx = 1D-315
real(real64), external :: poly,  test_fun

call calc_derivative(test_fun, x_0, dx, dydx)

print *, "derivative of x**2 at x_0 = ", x_0, " is ", dydx
! print *, "200cos(20x) at x_0", 200.0 * cos(20.0 * x_0)



contains 

    subroutine calc_derivative(func, x_0, dx, dydx)

    
        real(real64), external :: func
        real(real64), intent(in) :: x_0, dx
        real(real64), intent(out) :: dydx
        real(real64) :: test = 3.1415926

        real(real64) :: f_x0, f_x1

        f_x0 = func(x_0)
        f_x1 = func(x_0 + dx)
        dydx = (f_x1 - f_x0)/dx

    end subroutine

    
    
    
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