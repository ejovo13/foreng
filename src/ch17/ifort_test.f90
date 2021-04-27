program ifort_test

! We have learned that ifort does not support the types integer 128

use iso_fortran_env

    real(real128) :: A = 10.0
    integer, parameter :: MY_INT_128 = selected_int_kind(38)
    integer(MY_INT_128) :: K = 1512352138

    print *, "Real types supported by ifort:", real_kinds

    print *, "real32: ", real32
    print *, "real64: ", real64
    print *, "real128: ", real128

    print *, "A := ", A
    print *, "MY_INT_64 = ", MY_INT_64
    print *, "range(K) = ", range(K)
    print *, "K = ", K
    print *, "K * big number = ", K * 11235213054_MY_INT_128


end program