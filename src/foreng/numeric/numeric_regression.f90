module foreng_numeric_regression

use foreng_env
use foreng_math

implicit none

contains

    subroutine lls_fit(X, Y, m, b, stat)
    !! Linear Least Squares Fit

        real(real32), dimension(:), intent(in) :: X, Y
        real(real32), intent(out) :: m, b
        integer, optional :: stat

        if (size(X) /= size(Y)) then
            if (present(stat)) then
                stat = -1
                return
            else
                error stop "X and Y are not the same size"
            end if
        end if

        m = calc_m_lls(X, Y)
        b = calc_b_lls(mean(X), mean(Y), m)

    end subroutine

    subroutine lls_coefficient_fit(X, Y, m, b, r, stat)
        !! Linear Least Squares Fit with correlation coefficient
    
            real(real32), dimension(:), intent(in) :: X, Y
            real(real32), intent(out) :: m, b, r
            integer, optional :: stat
    
            if (present(stat)) then 
                call lls_fit(X, Y, m, b, stat)
            else 
                call lls_fit(X, Y, m, b)
            end if

            r = calc_correlation_coefficient_lls(X, Y)
    
    end subroutine

    subroutine generate_sys_eqns(X, Y, order, A, u)
        ! Data dictionary
            real(real64), dimension(:), intent(in) :: X, Y                   ! The x and y coordinates to be fit
            integer, intent(in) :: order                                     ! The order of the polynomial to fit
            real(real64), dimension(:,:), allocatable, intent(out) :: A      ! The system of equations we will solve
            real(real64), dimension(:), allocatable, intent(out) :: u        ! The right hand side of Ac = u
    
            integer :: i,j,ndim
    
            ndim = order + 1
    
            allocate(A(ndim, ndim), u(ndim))    
    
            do i = 1,ndim
                do j = 1,ndim
                    A(i,j) = sum(X**(i + j - 2))
                end do
            end do
    
            A(1,1) = size(X)
    
            do i = 1,ndim
                u(i) = sum(Y * (X ** (i-1)))
            end do
    end subroutine

    !====================================================================!
    !=                   Utility Functions                              =!
    !====================================================================!

    real(real32) function calc_m_lls(X, Y) result(m)

        real(real32), dimension(:), intent(in) :: X, Y

        real(real32) :: x_bar
        real(real32) :: y_bar

        real(real32), dimension(size(X)) :: x_squared

        x_squared = X * X
        x_bar = mean(X)
        y_bar = mean(Y)

        m = (sum(X * Y) - sum(X) * y_bar)/(sum(x_squared) - sum(x)*x_bar)        

    end function

    real(real32) function calc_b_lls(y_bar, x_bar, m) result(b)

        real(real32), intent(in) :: y_bar, x_bar, m

        b = y_bar - m * x_bar

    end function

    real function calc_correlation_coefficient_lls(X, Y) result(r)

        real(real32), dimension(:), intent(in) :: X, Y
        real(real32) :: denom, num !! Numerator and denominator
        integer :: n 

        n = size(X)

        num = n * sum(X*Y) - sum(X)*sum(Y)
        denom = sqrt( (n * sum(X*X) -  sum(X)**2) * (n * sum(Y*Y) - sum(Y)**2) )

        r = num/denom

    end function




end module