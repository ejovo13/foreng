module my_matmul_mod_parent

interface
    
    module subroutine my_matmul(A, B, C, ierr)
    ! Purpose:
    !   To multiply two matrices, A and B.
    implicit none
    
    ! Data dictionary
    integer, intent(inout) :: ierr
    real, dimension(:,:), intent(in) :: A, B
    real, dimension(:,:), intent(out) :: C

    
    end subroutine

end interface
    
end module