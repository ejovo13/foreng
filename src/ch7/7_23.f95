! Derivative of a Function
!
! Using the limit definition of a function calculate the derivative of a function
!
! Assume that a vector VECT contains NSAMP samples of a function taken at a spacing 
! of DX per sample. Write a subroutine that will calculate the derivative of this vector
! The subroutine should check to make sure that DX is greater than zero to prevent 
! divide by zero errors in the subroutine.

program deriv
implicit none

real, parameter :: DX = 0.001, X_0 = 0
integer, parameter :: NSAMP = 30
real, dimension(NSAMP) :: X
real, dimension(NSAMP) :: Y
real, dimension(NSAMP) :: cosx
real, dimension(NSAMP) :: x_squared
real, dimension(NSAMP) :: two_x
integer :: i
real, dimension(:), allocatable :: dydx

do i = 1, NSAMP
    X(i) = X_0 + (i - 1) * DX    
end do

Y = sin(X)
cosx = cos(X)

call find_deriv(Y, DX, dydx)

print 99
print *, "sin(x) = "
print *, Y

print 99
print *, "d/dx sin(x) = "
print *, dydx

print 99
print *, "cos(x) = "
print *, cos(x)

x_squared = X ** 2
two_x = 2 * X

call find_deriv(x_squared, DX, dydx)

print 99
print 99
print 99
print *, "x^2 = "
print *, x_squared

print 99
print *, "d/dx x^2 = "
print *, dydx

print 99
print *, "2x = "
print *, two_x

99 format(50("#"))


contains

    subroutine find_deriv(vect, dx, vect_prime)

        real, dimension(:), intent(in) :: vect
        real, intent(in) :: dx
        real, dimension(:), allocatable, intent(out) :: vect_prime

        integer :: nsamp, i
        real, parameter :: EPS = 1E-14
        real :: dy

        nsamp = size(vect)
        ! The vector containing the derivates should have 1 less element 
        allocate(vect_prime(nsamp - 1))

        if (abs(dx) < EPS) then 
            error stop "Value of dx is too small"
        end if

        do i = 1,nsamp-1
            dy = vect(i+1) - vect(i)
            vect_prime(i) = dy/dx
        end do

    end subroutine

end program