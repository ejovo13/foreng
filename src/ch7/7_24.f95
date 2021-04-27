! Derivative in the presence of Noise

! Generate an input vector containing 100 values of the function sin x starting at
! x = 0, and using a step size of 0.05. Generate random noise with a maximum amplitude
! of +- 0.02.

program deriv_with_noise
    use prototype
    implicit none
    
    real, parameter :: DX = 0.05, X_0 = 0
    integer, parameter :: NSAMP = 100
    real, dimension(NSAMP) :: X
    real, dimension(NSAMP) :: Y, Y_noise, noise
    real, dimension(NSAMP) :: cosx
    integer :: i
    real, dimension(:), allocatable :: dydx
    
    do i = 1, NSAMP
        X(i) = X_0 + (i - 1) * DX    
        noise(i) = urand(-0.02, 0.02)
    end do
    
    Y = sin(X)
    Y_noise = Y + noise
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

    print 99
    print 99
    print 99

    print *, "Now finding the derivative with noise!"

    call find_deriv(Y_noise, DX, dydx)

    print 99
    print *, "d/dx sin(x) + noise = "
    print *, dydx
    
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