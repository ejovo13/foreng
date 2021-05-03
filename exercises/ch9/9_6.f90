! 1. State the problem:
! Matrix Multiplication. Write a subroutine to calculate the product of two matrices if they
! are of compatible sizes and if the output array is large enough to hold the result. If the matrices are not
! of compatible sizes or if the array is too small, set an error flag and return to the calling proggram. 
! The dimensions of all three arrays a,b, and c should be passed to the subroutines from the calling program 
! so that explicit-shape dummy arrays can be used and size checking can be done.

program test_mult

use my_matmul_mod

implicit none

integer, parameter :: NROWA = 4, NCOLA = 3, NROWB = 3, NCOLB = 1, NROWC = 4, NCOLC = 1
real, dimension(NROWA,NCOLA) :: A
real, dimension(NROWB,NCOLB) :: B
real, dimension(NROWC,NCOLC) :: C
real, dimension(NROWC,NCOLC) :: C_true
integer :: ierr, i

A(1,:) = [1, -1, -2]
A(2,:) = [2, 2, 0]
A(3,:) = [3, 3, 3]
A(4,:) = [5, 4, 4]

B(:,1) = [-2, 5, 2]




call my_matmul(A, B, C, [4,3], [3,1], [4,1], ierr)

if (ierr == 0) then
    print *, "Matrix A: "

    do i = 1,NROWA
        print *, A(i,:)
    end do

    print *, "Matrix B: "

    do i = 1,NROWB
        print *, B(i,:)
    end do

    print *, "A x B = "

    do i = 1,NROWC
        print *, C(i,:)
    end do
    
    print *, "The actual answer is :"
    C_true = matmul(A,B)
    
    do i = 1,NROWC
        print *, C_true(i,:)
    end do

end if


end program