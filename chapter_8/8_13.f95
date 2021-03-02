! 1. State the problem

! Matrix Multiplication. Write a program that can read two matrices of arbitrary size
! from two input disk files, and multiply them if they are of compatible sizes.
! If they are of incompatible sizes, an appropriate error message should be printed.

! 2. Define the input and output

! The number of rows and columns in each matrix will be specified by two integers on the first
! line in each file, and the elements in each row of the matrix will be found on a single
! line of the input file.
! So, we will have two input files, mat1.dat and mat2.dat.

! 3. Describe the algorithm.

! First off, we want to open mat1.dat and mat2.dat. If the open was succesful, we want to
! read the very first line of both files. We check to see if the number of columns of the 
! first matrix is compatible with the number of rows of mat2. If they are compatible, we will
! allocate the appropriate amount of space for each matrix.

program matrix_multiplication
! Purpose:
!   To multiply two arbitrary matrices together, if they are compatible.
implicit none

! Data dictionary
real, dimension(:,:), allocatable :: A, B
integer :: rowA, colA, rowB, colB
integer :: aerr, berr, i
character(100) :: amsg, bmsg


! Try and open mat1.dat and mat2.dat
open(unit=10, file='mat1.dat', status='old', action='read', iostat=aerr, iomsg=amsg)
open(unit=11, file='mat2.dat', status='old', action='read', iostat=berr, iomsg=bmsg)

if (aerr == 0 .and. berr == 0) then ! Open was succesful
    
    read(10, *) rowA, colA
    read(11, *) rowB, colB
    
    print *, "The row and col of A is: ", rowA, colA
    print *, "The row and col of B is: ", rowB, colB

    if (colA == rowB) then
        print *, "Arrays are compatible, continuing with computation"
    else 
        stop "Arrays are not compatible for multiplication"
    end if

    allocate(A(rowA, colA))
    allocate(B(rowB, colB))

    read(10, *) (A(i, :), i = 1,colA)
    read(11, *) (B(i, :), i = 1,colB)

    print *, A, B






    
else if (aerr /= 0) then
    print *, amsg
else 
    print *, bmsg
end if



    
    

close(10)
close(11)






end program