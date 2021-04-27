program five_five
! For the fortran statements and 5_5.inp, state what the values of each variables 
!  will be when the READ statements have been completed.

    implicit none
    integer :: item1, item2, item3, item4, item5
    integer :: item6, item7, item8, item9, item10
    open(unit=10, file='5_5.inp', status='old', action='read')
    
    ! (a)
    ! read(10,*) item1, item2, item3, item4, item5, item6
    ! read(10,*) item7, item8, item9, item10

    ! (b)
    ! read (10, 8) item1, item2, item3, item4, item5, item6
    ! read (10, 8) item7, item8, item9, item10
    ! 8 FORMAT(4I10)

    close(unit=10)
    print *, item1, item2, item3, item4, item5, item6, item7, item8, item9, item10

end program
