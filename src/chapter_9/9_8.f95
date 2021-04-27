! Write a new version of the matrix multiplication subroutine from Exercise 9-6 that uses submodules to 
! separate the explicit interface from the executable code.

submodule (my_matmul_mod_parent) my_matmul_mod_child

implicit none

contains

    module procedure my_matmul 
    integer, dimension(2) :: dimA, dimB, dimC
    integer :: i,j,k
    real :: sum
    dimA = shape(A)
    dimB = shape(B)
    dimC = shape(C)
    
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

    end procedure my_matmul

end submodule

program nine_eight
    
    use my_matmul_mod_parent


    implicit none
    

    real, dimension(3,2) :: A
    real, dimension(2,1) :: B
    real, dimension(3,1) :: C
    real, dimension(3,1) :: C_true
    integer :: ierr, i


    A(1,:) = [10, 2]
    A(2,:) = [3, -3]
    A(3,:) = [5, 6]

    B(:,1) = [10, 20]

    call my_matmul(A, B, C, ierr)

    if (ierr == 0) then
        print *, "Matrix A: "
    
        do i = 1,3
            print *, A(i,:)
        end do
    
        print *, "Matrix B: "
    
        do i = 1,2
            print *, B(i,:)
        end do
    
        print *, "A x B = "
    
        do i = 1,3
            print *, C(i,:)
        end do
        
        print *, "The actual answer is :"
        C_true = matmul(A,B)
        
        do i = 1,3
            print *, C_true(i,:)
        end do
    
    end if

end program

