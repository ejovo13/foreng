!! Write a Fortran program that would print out every fifth value in the array
!! values from 6_12.

program every_fifth

use prototype

implicit none

    real, dimension(-50:50) :: values
    integer :: i



    do i = -50,50
    !! Initialize values
        values(i) = urand(-100.0, 100.0)
    end do

    111 format("values(", I3, ") = ", F8.4)

    do i = -50, 50, 5

        print 111, i, values(i)

    end do

end program