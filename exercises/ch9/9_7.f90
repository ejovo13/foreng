! Write a new version of the matrix multiplication subroutine from Exercise 9-6 that uses
! an explicit interface and assumed-shape arrays. Before multiplying the matrices.

! So the previous problem implemented explicit shape arrays, and this program I should use assumed-shape arrays.
! To get the bounds, I should use the inquiry functions in this chapter :
! LB

program nine_seven
    
    use my_matmul_mod_explicit


    implicit none
    

    real, dimension(3,2) :: A
    real, dimension(2,1) :: B
    real, dimension(3,1) :: C
    real, dimension(3,1) :: C_true
    integer :: ierr, i


    A(1,:) = [1, 2]
    A(2,:) = [3, 4]
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

