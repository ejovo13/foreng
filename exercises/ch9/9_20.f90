! 1. Create a test data set by calculating points along the curve y(x) = x^2 - 4x + 3
! for x = 0, 0.1, 0.2, ..., 5.0

! Next add random noise to each of the yi values. Then, try to estimate the coefficients
! of the original function that generated the data set. Try this when the added random noise has
! the range:

! (a) 0.0
! (b) [-0.1, 0.1)
! (c) [-0.5, 0.5)
! (d) [-1.0, 1.0)

program nine_20

    use prototype
    ! Purpose
    !   To test how the least_squares algorithm works for generated sets with added noise
    implicit none
    
    ! Data dictionary
    real(8), dimension(51) :: X, Y, noise, Y_noise
    real(8), dimension(:), allocatable :: u
    integer :: i
    
    
    
    do i = 1,51
        X(i) = 0.1 * (i - 1)     
    end do
    
    Y = curve(X)
    
    ! (a) 
    noise = 0
    
    print 99, 0.0,0.0
    call least_squares_fit(X, Y, 4, u)
    
    
    ! ! (b)
    print 99, -0.1, 0.1
    call add_noise(Y, noise, Y_noise, -0.1, 0.1)
    call least_squares_fit(X, Y_noise, 4, u)
    
    
    ! ! (c)
    print 99, -0.5, 0.5
    call add_noise(Y, noise, Y_noise, -0.5, 0.5)
    call least_squares_fit(X, Y_noise, 4, u)
    
    
    ! ! (d)
    print 99, -1.0, 1.0
    call add_noise(Y, noise, Y_noise, -1.0, 1.0)
    call least_squares_fit(X, Y_noise, 4, u)
    
    
    99 format("Solving least square for random noise: [", F5.2, ",", F5.2, ")")
    
    
    contains 
    
        elemental real(8) function curve(x)
            real(8), intent(in) :: x
            curve = x**4 - 3*(x**3) + 4*(x**2) + 2*x + 3
        end function
    
    end program