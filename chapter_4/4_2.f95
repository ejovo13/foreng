program test_statements
! This program tests to see if certain equalities are valid fortran statements, and if they are, to evaluate them
implicit none


logical :: a, b
character(5) :: c, d
a = '123' > 'abc'
! b = '9478' == 9478
c = ACHAR(65) // ACHAR(95) // ACHAR(72)
d = ACHAR(IACHAR('j') + 5)

print *, "a: ", a, "b: ", b,  "c: ", c, "d: ", d

end program