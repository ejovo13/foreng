! 1. State the problem:
! Matrix Multiplication. Write a subroutine to calculate the product of two matrices if they
! are of compatible sizes and if the output array is large enough to hold the result. If the matrices are not
! of compatible sizes or if the array is too small, set an error flag and return to the calling proggram. 
! The dimensions of all three arrays a,b, and c should be passed to the subroutines from the calling program 
! so that explicit-shape dummy arrays can be used and size checking can be done.

subroutine my_matmul(A, B, C, dimA, dimB, dimC, ierr)
! Purpose:
!   To multiply two matrices, A and B.
implicit none

! Data dictionary
integer, dimension(2), intent (in) :: dimA, dimB, dimC
integer, intent(inout) :: ierr
real, dimension(dimA(1), dimA(2)), intent(in) :: A
real, dimension(dimB(1), dimB(2)), intent(in) :: B
real, dimension(dimC(1), dimC(2)), intent(out) :: C

! Error flags dictionary
! 0        Successful matrix multiplication 
! 1        Matrices A and B are incompatible
! 2        Matrix C is not large enough to contain the result
! 3        Matrix C is not the appropriate size

! See if A and B are compatible
if (dimA(2) == dimB(1)) then
    print *, "The arrays are compatible"
    ierr = 0
end if 

end subroutine

program test_mult
implicit none

real, dimension(2,2) :: A, B, C
integer :: ierr

call my_matmul(A, B, C, [2,2], [2,2], [2,2], ierr)


end program