! Extrapolation. Once a least-squares fit has been calculated, the resulting polynomial
! can also be used to estimate the values of the function beyond the limits of the original
! input data set. This process is called sctrapolation. Write a program that calculates a 
! linear least-squares fit to the data set given below. Then, use that fit to estimate the 
! expected values y_0 at x_0 = 14.0

program interpolation_lin
    use prototype
    ! Purpose
    !   To interpolate using a quadratic fit for the data stored in X and Y
    implicit none
    
    real(8), dimension(11) :: X,Y       ! The data to fit
    real(8), dimension(11,2) :: XY
    integer :: i
    real(8), dimension(:), allocatable :: c
    integer, parameter :: ORDER = 1  ! Order of the polynomial to fit
    real(8), parameter :: X_0 = 14.0
    real(8) :: y_0
    
    do i = 1,11
        X(i) = 1.0 * (i - 1)
    end do
    
    Y = [-14.22, -10.54, -5.09, -3.12, 0.92, 3.79, 6.99, 8.95, 11.33, 14.71, 18.75]
    
    XY(:,1) = X
    XY(:,2) = Y
    
    print *, "Calling least_squares_fit"
    
    call least_squares_fit(X, Y, ORDER, c)
    
    print 101
    print 99, c(2), c(1)
    print 101
    99 format("f(x) = ",  F7.2, "*x + ", F7.2)
    101 format(50("#"))
    
    y_0 = c(3) * (X_0**2) + c(2) * (X_0) + c(1)
    
    print 111, y_0
    111 format("f(14.0) = ", F10.3)
    
    print 101
    call print_mat2(XY)

    print 101
    print *, Y
    
    
    
    end program