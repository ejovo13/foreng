program four_eight
! Examine the following DO statements and determine how many times each loop will
! be exectued.

implicit NONE
integer :: irange, j, kount, index, i
integer :: count = 0

a: do irange = -32768, 32767
    count = count + 1
end do a

print *, "(a) = ", count
count = 0

b: do j = 100, 1, -10
    count = count + 1
end do b

print *, "(b) = ", count

count = 0
c: do kount = 2, 3, 4
    count = count + 1
end do c

print *, "(c) = ", count
count = 0

d: do index = -4,-7
    count = count + 1
end do d

print *, "(d) = ", count
count = 0
 
e: do i = -10, 10, 10
    count = count + 1
end do e

print *, "(e) = ", count
count = 0

! f: do i = 10, -2, 0
!     count = count + 1
! end do f

!!!!! This for loop shows an error, since the increment is 0 !!!!!!!!!!

! print *, "(f) = ", count
! count = 0

end program
