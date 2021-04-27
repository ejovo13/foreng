! Determine which of the following Fortran program fragments are valid. For each valid statement,
! specify what will happen in the program. (Assume default typing for any variables that are not 
! explicitly typed within the program fragments.)

program valid_fragments
    ! ! (a)
    ! INTEGER, DIMENSION(100) :: icount, jcount
    ! ! . . .
    ! icount = [ (i, i=1, 100)] ! implied do loop
    ! jcount = icount + 1
    ! ! (a) has valid syntax, this will create two arrays, icount which is a size 100 array that
    ! ! contains the sequence of natural numbers from 1 to 100, and jcount contains the sequence of 
    ! ! natural numbers from 2 to 101.

    ! print *, icount
    ! print *, jcount

    ! (b)
    ! REAL, DIMENSION(10) :: value 
    ! value(1:10:2) = [5., 4., 3., 2., 1. ]
    ! value(2:11:2) = [10., 9., 8., 7., 6. ]
    ! write (*, 100) value
    ! 100 format ('Value = ',/,(F10.2))

    ! ! (c)
    ! INTEGER, DIMENSION(6) :: a
    ! INTEGER, DIMENSION(6) :: b
    ! a = [1, -3, 0, -5, -9, 3]
    ! b = [-6, 6, 0, 5, 2, -1]
    ! WRITE (*, *) a > b
    
    ! All the fragments are valid!
    print *, "All code fragments are valid"


end program