! 1. State the problem:
! Matrix Multiplication. Write a subroutine to calculate the product of two matrices if they
! are of compatible sizes and if the output array is large enough to hold the result. If the matrices are not
! of compatible sizes or if the array is too small, set an error flag and return to the calling proggram. 
! The dimensions of all three arrays a,b, and c should be passed to the subroutines from the calling program 
! so that explicit-shape dummy arrays can be used and size checking can be done.

module my_matmul_mod

Contains

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
integer :: i,j,k
real :: sum


! Error flags dictionary
! 0        Successful matrix multiplication 
! 1        Matrices A and B are incompatible
! 2        Matrix C is not large enough to contain the result
! 3        Matrix C is not the appropriate size

! See if A and B are compatible


if (dimA(2) == dimB(1)) then
    print *, "A and B are compatible for matrix multiplication"
    
    if (size(C) >= dimA(1) * dimB(2)) then
        print *, "C is large enough to hold the matrix product"

        if (dimC(1) == dimA(1) .and. dimC(2) == dimB(2)) then
            print *, "C is the proper shape to hold the matrix product"
            ierr = 0

            ! Let's actually compute the matrix now

            ! The common value is the colA and rowB. This is the number of operations per element in C
            print *, "Computing the matrix multiplication"
            do i = 1,dimC(1)
                do j = 1,dimC(2)
                    sum = 0
                    do k = 1,dimA(2)
                        ! First column of C is 1st col of B times first row of A
                        ! Second column of C is the 2nd col of B times the first row of A
                        sum = sum + (B(k,j) * A(i,k))
                        print 100, k, j, i, k
                        100 format ("B(", I0, ",", I0, ") * A(", I0, ",", I0, ")")
                        111 format (20("*"))
                    end do
                    print 111
                    C(i,j) = sum
                end do
            end do

        else 
            print *, "C is not the proper shape to hold the matrix product"
            ierr = 3
        end if
    else
        print *, "C is not large enough to hold the matrix product"
        print *, "The size of C is: ", size(C)
        print *, "The shape of C is: ", shape(C)
        ierr = 2
    end if

else 
    ierr = 1
    print *, "Matrices are not compatible"
end if

! print *, "The shape of C inside the subroutine is: ", shape(C)

end subroutine

end module

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