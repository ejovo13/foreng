! Determine the shape and size of the arrays specified by the following declaration statements.
! Also note the valid subscript range for each dimension of each array.

program main

! (a)
    ! CHARACTER(len=80), DIMENSION(3, 60) :: line
! This array has rank 2, with valid subscripts 1:3 and 1:60. The size of the array is 180, and each element
! is a string with 80 characters.

! (b)
    ! INTEGER, DIMENSION(-10:10,0:20) :: char
! This arrays has rank two, valid subscripts from -10:10, and 0:20. The array has size 21 * 21 = 441
! The element of this array is a integer that represents a character 

! (c)
    ! REAL, DIMENSION(-5:5, -5:5, -5:5, -5:5, -5:5) :: range
! This array has rank 5, with valid subscripts -5:5 in every dimension. The size of the array is 11**5.
! The shape of the array is 11 x 11 x 11 x 11 x 11

end program main