!! Geometric Mean

!! Geometric mean is defined as the nth root of the product of the numbers
!! Use a while loop to take a list of numbers, terminating when negative

program geometric_mean

implicit none

    real :: this_input
    real :: product, sum
    integer :: ierr
    character(100) :: errmsg
    integer :: count

    print *, "Enter a list of numbers to compute the geometric mean"
    print *, "enter -1 to end"

    product = 1
    sum = 0
    count = 0

    read: do 

        read (*,*, iostat=ierr, iomsg=errmsg) this_input

        if (ierr == 0) then
            if (this_input < 0) then
                exit read
            else 
                product = product * this_input
                sum = sum + this_input
                count = count + 1
                cycle read
            end if
        else 
            print *, "ERR: ", errmsg
            print *, "Please enter a positive real number (or negative to quit)"
        end if

    end do read

    print *, "Product = ", product
    print *, "Mean = ", sum / count
    print *, "Geometric mean = ", nth_root(product, count)

    ! print *, "3rd root of 8 = ", nth_root(8.0, 3)
    ! print *, "4th root of 16 = ", nth_root(16.0, 4)

contains

    real function nth_root(x_, n_) result(root_)

        real, intent(in) :: x_
        integer, intent(in) :: n_
        real, parameter :: BASE = 10

        root_ = BASE**((1.0/n_)*log10(x_))


    end function



end program