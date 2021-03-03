! 1. State the problem:
! Matrix Multiplication. Write a subroutine to calculate the product of two matrices if they
! are of compatible sizes and if the output array is large enough to hold the result. If the matrices are not
! of compatible sizes or if the array is too small, set an error flag and return to the calling proggram. 
! The dimensions of all three arrays a,b, and c should be passed to the subroutines from the calling program 
! so that explicit-shape dummy arrays can be used and size checking can be done.

subroutine my_matmul(A, B, C, dimA, dimB, dimC)