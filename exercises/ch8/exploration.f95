program exploration
    ! Purpose:
    !
    !       To understand who Fortran uses logical indexing, and how that differs from MATLAB
    !
    !       So we've learned that Fortran does NOT use logical indexing. However, just like MATLAB,
    !       using an inequality on a matrix will return a logical matrix
    implicit none
    integer :: i
    integer, dimension(10) :: my_array = [ (i, i = 1,10)]
    logical, dimension(10) :: my_logic
    integer, dimension(2,2) :: A
    
    A(1,:) = [ 1, 2 ]
    A(2,:) = [ 3, 4 ]

    my_logic = my_array > 5


    print *, "My logic = ", my_logic

    where (my_logic)
        my_array = 10
    end where

    where (my_array < 50)
        my_array = 100
    end where

    print *, "My new array = ", my_array

    print *, "---------------------------"
    print *, "Now testing rank 2 arrays"

    print *, "Default representation of A :: "
    print *, "A = ", A

    print 100, (A(i,:), i = 1,2)
    100 format(2(I0, 1X))

end program

