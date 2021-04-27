program five_three
! What is printed out by the following Fortran statements?

    real :: a = 1.602E-19, b = 57.2957795, c = -1
    write (*, '(ES14.7, 2(1X, E13.7))') a, b, c

end program