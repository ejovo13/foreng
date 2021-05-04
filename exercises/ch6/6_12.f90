!! Assume that values is a 101-element array containing a list of measurements from a scientific
!! experiment. Values is declared as: REAL, DIMENSION(-50:50) :: values

!! Write Fortran statements that would count the number of positive values
!! negative values, and zero values in the array, and write out a message summarizing 
!! how many values of each type were found.

program counting 
use prototype

implicit none

    real, dimension(-50:50) :: values
    real, parameter :: EPS = 1E-5
    integer :: i

    integer :: nzero = 0, npos = 0, nneg = 0



    do i = -50,50
    !! Initialize values
        values(i) = urand(-10.0, 10.0)
    end do

    values(13) = 0.0
    values(-5) = 0.0
    values(30) = 0.0

    do i = -50,50

        if (abs(values(i)) < EPS) then 
            nzero = nzero + 1
        else if (values(i) < 0) then 
            nneg = nneg + 1
        else
            npos = npos + 1
        end if
    end do

    print *, "Number positive = ", npos
    print *, "Number negative = ", nneg
    print *, "Number zero     = ", nzero



end program