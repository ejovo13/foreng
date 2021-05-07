!! Infinite series

!! sin(x) is usually calculated on computers using a truncated taylor series.

!! Write a program that reads in a value for x in degrees, and then calculate sinx using 
!! a taylor series with N = 1,10

program sine_taylor

use foreng_math

implicit none

    real :: x = 90
    integer :: i = 0
    integer :: ierr
    character(100) :: errmsg
    real :: sin_x_intrinsic, sin_x_series, err
    print *, "Enter a value in degrees to compute sine using a truncated taylor series"

    read: do

        read(*,*, iostat=ierr, iomsg=errmsg) x

        if (ierr == 0) then
            exit read
        else 
            print *, "Please enter a valid number"
            cycle read
        end if

    end do read

    111 format("| n terms |   series   | intrinsic |    error    |")
    122 format("|", 4X, I0, T11, "|", F10.7, T24, "|", F10.7, T36, "|", X, F10.7, T50, "|")
    123 format(50("="))

    print 111
    print 123

    do i = 1, 10

        sin_x_intrinsic = sind(x)
        sin_x_series = sind_series(x, i)
        err = sin_x_series - sin_x_intrinsic

        print 122, i, sin_x_series, sin_x_intrinsic, err

    end do

    print 123

contains 

    real function sind_series(x_, n_) result(sin_x)
    !! Compute sine using a truncated taylor series
        real, intent(in) :: x_ !! Angle in degrees
        integer, intent(in) :: n_ !! Number of terms to use
        integer :: i_
        real :: x_rad

        x_rad = deg_to_rad(x_)

        sin_x = 0

        do i_ = 1, n_

            sin_x = sin_x + ( (-1)**(i_ - 1) )*( (x_rad ** (2 * i_ - 1)) / (fact((2 * i_) - 1)))        

        end do


    end function

    recursive integer function fact(x) result (x_fact)
    integer, intent(in) :: x

    if (x == 0) then
        x_fact = 1
        return
    end if
    x_fact = x * fact(x - 1)

end function

 



end program
