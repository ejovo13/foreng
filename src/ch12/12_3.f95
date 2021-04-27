! If two complex numbers are expressed in polar form, the two numbers may
! be multiplied by multiplying their magnitudes and adding their angles.
! Write a function that multiplies two variables of type "polar" together 
! using this expression and returns a result in polar form. Note that the 
! resulting angle theta should be in the range -180 <= theta <= 180

program polar_multiplication

use ejovo_types

implicit none

type(polar) :: p1, p2, p3

p1 = polar(10.0, 90.0)
p2 = polar(5, 360)
 
p3 = p1%mult(p2)

print *, "p3 = ", p3

end program
