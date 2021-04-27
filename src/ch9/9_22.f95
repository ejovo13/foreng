! Interpolation

! Once a least-squares fit of order n is calculated, we can estimate the expected value 
! y_0 with any location x_0 within the data set. This process is called interpolation.
!
! Write a program that calculates a quadratic least-squares fit to the data set given below
! and then uses that fit to estimate the expected value y_0 at x_0 = 3.5

program interpolation
use prototype
! Purpose
!   To interpolate using a quadratic fit for the data stored in X and Y
implicit none

real(8), dimension(11) :: X,Y       ! The data to fit
real(8), dimension(11,2) :: XY
integer :: i
real(8), dimension(:), allocatable :: c
integer, parameter :: ORDER = 2    ! Order of the polynomial to fit
real(8), parameter :: X_0 = 3.5
real(8) :: y_0

do i = 1,11
    X(i) = 1.0 * (i - 1)
end do

Y = [-23.22, -13.54, -4.14, -0.04, 3.92, 4.97, 3.96, -0.07, -5.67, -12.29, -20.25]

XY(:,1) = X
XY(:,2) = Y

print *, "Calling least_squares_fit"

call least_squares_fit(X, Y, ORDER, c)

print 101
print 99, c(3), c(2), c(1)
print 101
99 format("f(x) = ", F7.2, "*x^2 + " F7.2, "*x + ", F7.2)
101 format(50("#"))

y_0 = c(3) * (X_0**2) + c(2) * (X_0) + c(1)

print 111, y_0
111 format("f(3.5) = ", F10.3)

! print 101
! call print_mat2(XY)



end program

