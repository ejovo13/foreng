! Is the following program correct or incorrect? If it is incorrect, what is wrong with it?
! If it is correct, what values will be printed out by the following program?

module my_constants
    implicit NONE
    real, parameter :: PI = 3.141593 ! Pi 
    real, parameter :: G = 9.81
end module my_constants

program main 
    use my_constants
    implicit NONE
    write (*,*) 'SIN(2*PI) = ', sin(2.*PI)
    ! G = 17. ! obviously you can't modify a parameter.
end program main