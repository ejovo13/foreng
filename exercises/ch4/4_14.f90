! Write a Fortran program to evaluate the function 
! y(x) = ln(1/1-x)
! for any user-specified value of x, where ln is the natural logarithm.
! Write the program with a while loop, so that the program repeats the calculation 
! for each legal value of x entered into the program. When an illegal value of x is entered,
! terminate the program. 

PROGRAM nat_log
!   Purpose:
!       This program calculates the value of ln(1/(1-x)) for all valid
!       values of x entered by the user
!
!
IMPLICIT NONE

! Data dictionary: declare variable types, definitions, & units.
real :: x                   ! The x value to be evaluated
integer :: ierr = 0         ! The error status when reading in the value
character(100) :: errmsg    ! The error message generated when reading in a value


do while(ierr == 0)
    print *, "Please enter a real number greater than 1"
    read (*,*, iostat = ierr, iomsg = errmsg) x
    if (x <= 1 .or. ierr /= 0) exit
    print 100, x, log((1)/(x - 1))
end do

100 Format("f(", f10.2, ") = ", f7.3)

end program