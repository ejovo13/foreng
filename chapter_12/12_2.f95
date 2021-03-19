! Create a derived data type called "polar" to hold a complex number 
! expressed in polar (z, theta) format.

! Write two functions that convert an ordinary complex number into a polar
! number, and that convert a polar number into an ordinary complex number

program test_polar
use polar_type

implicit none

    complex :: c = (0, 1)
    type (polar) :: my_polar

    my_polar = comp2pol(c)

    print *, "my_polar = ", my_polar

    my_polar%theta = 45.0
    my_polar%r = sqrt(2.0)

    print *, "my_complex = ", pol2comp(my_polar)



end program