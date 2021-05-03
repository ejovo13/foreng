! Write a program that can read in a rank2 array from an input disk file, and calculate the 
! sums of all of the data in each row and each column in the array. The size of the array to be read in
! will be specified by two numbers on the first line of the input file, and the elements in each row of the 
! will be found on a single line of the input file. Size the program to handle arrays of up to 100 rows and 100 
! columns.  

program read_array
! Purpose:
!       To read in a variable size array from an input file,
!       and then print the sums of the columns and rows.
implicit none

! Data dictionary: Declare variables
integer, parameter:: MAX_ROWS = 100, MAX_COLS = 100
integer :: istat
integer :: nrows
integer :: ncols
character(50) :: msg
real, allocatable, dimension(:,:) :: my_array
real, allocatable, dimension(:) :: row_sums
real, allocatable, dimension(:) :: col_sums
character(30) :: file_name
integer :: i, j

file_name = '8_9.dat'
open(unit=10, file=file_name, iostat=istat, iomsg=msg, status='old', action='read')

if (istat == 0) then ! Opening the file was successful
    read(10,*) nrows, ncols ! Read the first lines

    if (nrows > MAX_ROWS .or. ncols > MAX_COLS) then
        error stop "Number of rows or columns exceeds the max value"
    end if

    ! Read the matrix
    allocate(my_array(nrows, ncols))
    allocate(row_sums(nrows))
    allocate(col_sums(ncols))

    if (allocated(my_array)) then
        do i = 1,nrows
            read(10,*) my_array(i,:)
        end do
    end if

    ! Sum the rows
    if (allocated(row_sums)) then
        do i = 1,nrows
            row_sums(i) = sum(my_array(i,:))
            print 100, i, row_sums(i)
            100 format ("Sum of row ", I3, "= ", F8.3)
        end do
    end if

    if (allocated(col_sums)) then
        do j = 1,ncols
            col_sums(j) = sum(my_array(:,j))
            print 110, j, col_sums(j)
            110 format ("Sum of col ", I3, " = ", F8.3)
        end do
    end if


    ! Sum the cols

    do j = 1,nrows
        print *, my_array(j,:)
        ! print 100
        ! 100 format (/)
    end do
    

else 
    print *, msg
end if    

close(10)


end program
