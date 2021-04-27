! What will be the output from each of the WRITE statements in the following program?
! Why is the output of the two statements different?

program eight_four
    implicit none
    integer, dimension(0:1, 0:3) :: my_data ! 2 x 4 array.
    integer :: i, j
    my_data(0,:) = [ 1, 2, 3, 4 ] ! First row of my_data
    my_data(1,:) = [ 5, 6, 7, 8 ] ! second row of my_data

    do i = 0,1
        write(*, 100) (my_data(i,j), j = 0, 3) ! Implied do loop, returns a list of length 4 for each row i
        100 format(6(1X, I4)) ! Each write statement expects 6 integers, but will go to the next line for each loop iteration.
    end do
    ! Output should be:
    ! 1 2 3 4 
    ! 5 6 7 8

    write(*, 100) ((my_data(i,j), j = 0, 3), i = 0,1) ! Implied do loop, returns list of 8 locations.
    ! This will write the first 6 elements, get full, then go to the next line.
    ! I expect the output to be
    ! 1 2 3 4 5 6
    ! 7 8
end program eight_four