! Use the program from 8_13 to calculate the C = A x B where A and B are stored in the files
! a.dat and b.dat.

! This program is a copy and paste of 8_13, except I change the name of the input files to a.dat and b.dat.

program matrix_multiplication_clone
    ! Purpose:
    !   To multiply two arbitrary matrices together, if they are compatible.
    implicit none
    
    ! Data dictionary
    real, dimension(:,:), allocatable :: A, B, C        ! A and B are the matrices to be multiplied, C is the product
    integer :: rowA, colA, rowB, colB
    integer :: aerr, berr, i
    
    character(100) :: amsg, bmsg
    
    print 111
    print 999
    ! Try and open mat1.dat and mat2.dat
    open(unit=10, file='a.dat', status='old', action='read', iostat=aerr, iomsg=amsg)
    open(unit=11, file='b.dat', status='old', action='read', iostat=berr, iomsg=bmsg)
    
    if (aerr == 0 .and. berr == 0) then ! Open was succesful
        
        read(10, *) rowA, colA
        read(11, *) rowB, colB
        
        print *, "The row and col of A is: ", rowA, colA
        print *, "The row and col of B is: ", rowB, colB
        print 999
    
        if (colA == rowB) then
            print *, "Arrays are compatible, continuing with computation"
            print 999
            print 111
        else 
            stop "Arrays are not compatible for multiplication"
            print 999
        end if
    
        allocate(A(rowA, colA))
        allocate(B(rowB, colB))
    
        read(10, *) (A(i, :), i = 1,rowA)
        read(11, *) (B(i, :), i = 1,rowB)
    
    
        print *, "Matrix A read in as: "
        print 999
    
        do i = 1,rowA
            print *, A(i,:)
        end do
    
        print 111
        print *, "Matrix B read in as: "
        print 999
    
        do i = 1, rowB
            print *, B(i,:)
        end do 
    
        C = matmul(A, B) ! C gets automatically allocated by Fortran
    
        print 111
        print *, "A x B = "
        print 999
        ! C should be of size rowA x colB
        do i = 1, rowA
            print *, C(i,:)
        end do
    
        999 FORMAT(50("-"))
        111 FORMAT(/)
    
        
    else if (aerr /= 0) then
        print *, amsg
    else 
        print *, bmsg
    end if
    
    
    
        
        
    
    close(10)
    close(11)
    
    
    
    
    
    
    end program