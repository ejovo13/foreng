! 1. State the problem

! Write a program to read a Matrix A from an input disk and to scan for all relative maxima within
! the matrix.

! 2. Define the input and output

! The first line in the disk file should contain the number of rows and the number
! of columns in the matrix and then the next lines should contain the values in the matrix,
! with all the values in a given row on a single line of the input disk file.

! 3. Describe the algorithm

! We should first read in the first line of a given input file. We will call this file 8_16.inp
! We will then read in the data of this matrix. Afterwards, find the valid boundaries that only test
! boundary points. For example, for a 3 x 3 matrix, there is only one interior point. Loop through the interior points
! and find whether or not the point is a maximum by comparing it to the surrounding points.

! 4. Write the program in Fortran

program local_maximum
! Purpose:
!   To find the local maximum in a matrix, that is a value that is larger than the 8 local values.
implicit none

! Data Dictionary
real, dimension(:,:), allocatable :: A
real, dimension(:), allocatable :: local_max_arr
logical, dimension(:,:), allocatable :: A_local_max_logical
integer, dimension(:), allocatable :: irow, icol
integer :: nrowA, ncolA, nlocal_max
integer :: ierr
character(100) :: msg
integer :: i, j
integer :: in_row_start, in_row_end, in_col_start, in_col_end, this_index


open(unit=10, file='8_16.inp', iostat=ierr, iomsg=msg, status='old', action='read')

if (ierr == 0) then ! Open was successful

    read(10,*) nrowA, ncolA

    in_row_start = 2
    in_col_start = 2
    in_row_end = nrowA - 1
    in_col_end = ncolA - 1

    allocate(A(nrowA,ncolA))
    allocate(A_local_max_logical(nrowA,ncolA))


    read(10,*) (A(i,:), i = 1,nrowA)


    print *, "Matrix A read in as: "
    do i = 1,nrowA
        print *, A(i,:)
    end do

    print *, "Matrix A_logical initialized as: "
    do i = 1,nrowA
        print *, A_local_max_logical(i,:)
    end do

    do i = in_row_start, in_row_end
        do j = in_col_start, in_col_end
            A_local_max_logical(i,j) = is_max(i, j, A)
        end do
    end do

    print *, "Matrix A_logical transformed to: "
    do i = 1,nrowA
        print *, A_local_max_logical(i,:)
    end do

    nlocal_max = count(A_local_max_logical)

    allocate(local_max_arr(nlocal_max))
    allocate(irow(nlocal_max))
    allocate(icol(nlocal_max))

    this_index = 1

    do i = in_row_start, in_row_end
        do j = in_col_start, in_col_end            
            if(A_local_max_logical(i,j)) then
                irow(this_index) = i
                icol(this_index) = j
                local_max_arr(this_index) = A(i,j)
                this_index = this_index + 1
            end if
        end do
    end do

    do i = 1,nlocal_max
        print 111, irow(i), icol(i), local_max_arr(i)
    end do

    111 format("(", I0, ",", I0, ")", 2X, "|", 2X, F7.3)


else 
    print *, msg
    stop
end if



contains 

pure logical function is_max(i, j, array)
! This function evaluates if the specified index is a local maximum
implicit none


integer, intent(in) :: i, j
real, intent(in), dimension(:,:) :: array
real, dimension(3,3) :: neighborhood
real :: this_value

neighborhood = array(i-1:i+1, j-1:j+1)
this_value = array(i,j)

is_max = all(neighborhood <= this_value)


end function


end program