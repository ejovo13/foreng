! Given an array my_array defined as shown and containing the values shown below, determine whether
! each of the following array sections is valid. Specify the shape and contents of each valid array section

program valid_array_sections
    implicit none
    REAL, DIMENSION(-2:7) :: my_array = [-3, -2, -1, 0, 1, 2, 3, 4, 5, 6]
    INTEGER, DIMENSION(5) :: list = [ -2, 1, 2, 4, 2 ]

    ! (a)
    ! print *, my_array(-3, 3)

    ! (b)
    print *, my_array(-2:2)

    ! (c) 
    print *, my_array(1:5:2)

    ! (d)
    print *, my_array(list)

end program