program misc_test

use foreng_math_misc
use iso_fortran_env

implicit none

    ! integer(int16) :: x
    ! integer(int32) :: x
    ! integer(int64) :: x
    ! integer(16) :: x

   

    print *, "factorial(7) = ", factorial(7_int16)
    print *, "factorial(8) = ", factorial(8_int16)

    print *, "factorial(16) = ", factorial(16_int32)
    print *, "factorial(17) = ", factorial(17_int32)

    print *, "factorial(20) = ", factorial(20_int64)
    print *, "factorial(21) = ", factorial(21_int64)

    print *, "factorial(33) = ", factorial(33_int128)
    print *, "factorial(34) = ", factorial(34_int128)

    print *, "8**-3 = ", 8.0**(1.0/3)
    print *, "nth_root(8.0, 3.0) = ", nth_root(8.0, 3)
    



end program