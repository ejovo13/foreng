! Assume that values is a 10,201 element array containing a list
! of measurements from a scientific experiment, which has been declared by the statement
! real, dimension(-50:50, 0,100) :: values

! (a) Create a set of Fortran statements that would count the number
! of positive values, negative values, and zero values in the array
! and write out a message summarizing how many values of each type were found. Do
! not use any intrinsic functions in your code.

! (b) Use the intrinsic function Count to achieve the same thing as in (a)

program counting 
!   Purpose:
!       To learn how to use the where statement.
implicit none

integer, dimension(3,3) :: test 
integer :: pcount = 0, ncount = 0, zcount = 0
integer :: i, j


test(1,:) = [ 1, 2, 3]
test(2,:) = [ -1, -3, 0]
test(3,:) = [ 0, 0, 0]

! (a)
do i = 1,3
    do j = 1,3
        if (test(i, j) > 0) pcount = pcount + 1
        if (test(i, j) < 0) ncount = ncount + 1
        if (test(i,j) == 0) zcount = zcount + 1
    end do
end do

print 100, (test(i,:), i = 1,3)
100 format (3I3)

print 110, pcount, ncount, zcount
110 format("pcount: ", I4, /, "ncount: ", I4, /, "zcount: ", I4)

! (b)

pcount = count(test > 0)
ncount = count(test < 0)
zcount = count(test == 0)
print 110, pcount, ncount, zcount

! Part b is way more elegant. This is what I wanted, a function to count the number of true values. 


end program

 

