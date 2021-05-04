!! Harmonic mean 

!! Harmonic mean is calculated by : N / (1/x1 + 1/x2 + 1/x3 + 1/xN)

program harmonic_mean

implicit none

    real :: this_input
    real :: sum_of_inverses !! (1/x)
    integer :: ierr
    character(100) :: errmsg
    integer :: count

    print *, "Enter a list of numbers to compute the geometric mean"
    print *, "enter -1 to end"


    sum_of_inverses = 0
    count = 0

    read: do 

        read (*,*, iostat=ierr, iomsg=errmsg) this_input

        if (ierr == 0) then
            if (this_input < 0) then
                exit read
            else 
                sum_of_inverses = sum_of_inverses + (1/this_input)
                count = count + 1
                cycle read
            end if
        else 
            print *, "ERR: ", errmsg
            print *, "Please enter a positive real number (or negative to quit)"
        end if

    end do read

    print *, "Sum of inverses = ", sum_of_inverses
    print *, "Harmonic mean = ", count / sum_of_inverses


end program