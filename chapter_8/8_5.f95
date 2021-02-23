! An input file 8_5.dat is used, which is a 5 x 5 matrix of integers.

! What will be the contents of the variable array_values after each of the following   
! READ statements have been executed.?

program eight_five
    implicit none
    integer, dimension(4,4) :: values = 0
    integer :: i, j
    open(unit=8, status='old', action='read', file='8_5.dat')

    ! (a)
    do i = 1, 4
        read(8,*) (values(i, j), j = 1, 4) ! Returns a list of length 4
        ! The values that are read in will be the first (upper left) 4 x 4 block of 8_5.dat
    end do

    print 110
    print 100, (values(i,:), i = 1,4) ! Prints the matrix how it appears
    100 format(4I4)
    110 format(/, " (a) ", /)
    ! (b)
    rewind(8)
    read(8, *) ((values(i,j), j = 1,4), i = 1,4) ! Returns a list that is 16 elements long, row major order.
    ! This read statement will read across the matrix in 8_5.dat, and read in the first 16 elements, row major order
    120 format(/, " (b) ", /)
    print 120
    print 100, (values(i,:), i = 1,4)
    rewind(8)

    ! (c) 
    do i = 1,4 
        read (8,*) values(i,:)
    end do
    ! I think that this will be the same result as (a)

    130 format(/, " (c) ", /)
    print 130
    print 100, (values(i, :), i = 1,4)

    rewind(8)
    
    ! (d) 
    read (8, *) values 
    ! when dealing with an array like this, the elements are read in column major. So, we expect the first 4 elements (row 1)
    ! of 8_5.dat to be read into the first column of data. Basically, read(8,*) will read across the input, and will assign the entries
    ! column major.

    140 format(/, " (d) ", /)
    print 140
    print 100, (values(i, :), i = 1,4)



    close(unit=8)
end program