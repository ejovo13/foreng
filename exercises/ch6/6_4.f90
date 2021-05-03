! Determine the shape and size of the arrays specified by the following declaration statements,
! and the valid subscript range for each dimension of each array

program size_of_arrays
    implicit none
    ! ! (a)
    ! character(len=80), dimension(60) :: line
    ! ! An array that is 1 dimensionsal, has 60 entries, and each entry is a charactar of length 80.

    ! ! (b)
    ! integer, parameter :: ISTART = 32
    ! INTEGER, PARAMETER :: ISTOP = 256
    ! INTEGER, DIMENSION(ISTART:ISTOP) :: char
    ! ! char is an array of size (256-32 + 1) = 225

    ! ! (c)
    ! INTEGER, PARAMETER :: NUM_CLASS = 3
    ! INTEGER, PARAMETER :: NUM_STUDENT = 35
    ! LOGICAL, DIMENSION(NUM_STUDENT, NUM_CLASS) :: passfail
    ! ! passfail is an array of size 33.

end program