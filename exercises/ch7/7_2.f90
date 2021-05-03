! When a subroutine is called, how is data passed from the calling program to the subroutine,
! and how are the results of the subroutine returned to the calling program?

! Parameters are passed around inside subroutines as references, that is to say that there is not
! a block of memory that is copied to the subroutine, instead the argument is passed with a pointer 
! to the block of memory that stores the value.