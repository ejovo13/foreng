! Write a set of Fortran statements that would search a rank 3 array arr and limit the max
! value of any array element to be less than or equal to 1000. If any value exceeds 1000, set the 
! element to 1000. Assume that the array is 1000 x 10 x 30

! Write one statement using do loops, and one statement with the where statement. Which is easier?

program max_value
implicit none

integer, dimension(1000,10,30) :: arr = 0
integer :: i, j, k

do i = 1,1000
    do j = 1,10
        do k = 1,30
            if (arr(i,j,k) > 1000) arr(i,j,k) = 1000
        end do
    end do
end do

where (arr > 1000)
    arr = 1000
end where

! The second way is faster to write

end program
