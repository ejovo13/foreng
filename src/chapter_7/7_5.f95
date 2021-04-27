! Suppose that a 15-element array A is passed to a subroutine as a calling argument. What will 
! happen if the subroutine attempts to write to element a(16)

! If the array is explicit-shaped, then there will be an error in the program. If the array is assumed-size, 
! then there will be no error and the program will write to a certain block of memory that comes right after
! array a.