! Derivative of a Function. Write a subroutine to calculate the
! derivative of a double precision real function f(x) at position x = x_0.

! The calling arguments to the subroutine should be the 
! function f(x), the location x_0 at which to evaluate the function, and the step 
! size dx to use in the evaluation.

program deriv_sub
implicit none

integer, parameter :: DBL = selected_real_kind(13)
real(DBL) :: dydx, x_0 = 0, dx = 0.0000001
real(DBL), external :: poly,  test_fun

call calc_derivative(test_fun, x_0, dx, dydx)

print *, "derivative of 10sin20x at x_0 = ", x_0, " is ", dydx
print *, "200cos(20x) at x_0", 200.0 * cos(20.0 * x_0)


contains 

    subroutine calc_derivative(func, x_0, dx, dydx)

        integer, parameter :: DBL = selected_real_kind(13)
        real(DBL), external :: func
        real(DBL), intent(in) :: x_0, dx
        real(DBL), intent(out) :: dydx
        real(DBL) :: test = 3.1415926

        real(DBL) :: f_x0, f_x1

        f_x0 = func(x_0)
        f_x1 = func(x_0 + dx)
        dydx = (f_x1 - f_x0)/dx

        print *, "f_x0 = ", f_x0
        print *, "f_x1 = ", f_x1
        print *, "f(2) = ", func(test)

        THIS FUNCTION IS NOT WORKING AND I DONT KNOW WHY

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
    test_fun = 10.* sin(10.0*x)
end function