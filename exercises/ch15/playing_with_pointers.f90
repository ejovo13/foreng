!! Explore the functionality of pointers

program pointer_fun

implicit none

    integer, target :: int_targ
    integer, pointer :: int_ptr

    int_targ = 10
    int_ptr => int_targ

    print *, "int_ptr = ", int_ptr
    print *, "int_targ = ", int_targ

    deallocate(int_ptr)
    ! nullify(int_ptr)


    ! print *, "int_ptr = ", int_ptr
    ! print *, "int_targ = ", int_targ
    
    ! if (associated(int_ptr)) then
    !     print *, "int_ptr is associated"
    ! else
    !     print *, "int_prt is disassociated"
    ! end if





end program