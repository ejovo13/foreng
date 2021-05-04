!! RMS average 

!! rms average is the square root of the arithmetic mean of the squares of the numbers



program rms_average
implicit none

    real :: this_input
    real :: sum_of_squares
    integer :: ierr
    character(100) :: errmsg
    integer :: count

    print *, "Enter a list of numbers to compute the geometric mean"
    print *, "enter -1 to end"


    sum_of_squares = 0
    count = 0

    read: do 

        read (*,*, iostat=ierr, iomsg=errmsg) this_input

        if (ierr == 0) then
            if (this_input < 0) then
                exit read
            else 
                sum_of_squares = sum_of_squares + (this_input*this_input)
                count = count + 1
                cycle read
            end if
        else 
            print *, "ERR: ", errmsg
            print *, "Please enter a positive real number (or negative to quit)"
        end if

    end do read

    print *, "Sum of squares = ", sum_of_squares
    print *, "rms = ", sqrt(sum_of_squares/count)

end program