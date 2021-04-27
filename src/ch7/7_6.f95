! Suppose that a real value is passed to a subroutine in an argument that is declared to be an 
! integer in the subroutine. Is there any way for the subroutine to tell that the type
! is mismatched? What happens on your computer when the following code is executed?

! Surely since we declared the type in the subroutine definition, there will be a complile/execution error.

program main 
    implicit none
    real :: x
    x = -5.
    call sub1 (x) 
end program main

subroutine sub1 (i)
    implicit none
    integer, intent(in) :: i
    write(*,*) ' I = ', i
end subroutine sub1

! This program gives a warning when compiling but doesn't fail when executing --- damn that's kinda rough.
! The output is a garbage value because the subroutine expects an integer.