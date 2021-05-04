!! Test the mean functions
program mean_tests

use ch4_means

    implicit none

    real :: this_input
    integer :: ierr
    character(100) :: errmsg
    real, dimension(:), allocatable :: inputs


    real :: harmonic_mean
    real :: arithmetic_mean
    real :: rms_mean
    real :: geom_mean


    print *, "Please enter a set of values to calculate the mean." 
    print *, "[-1] to quit"


    read: do 

        read (*,*, iostat=ierr, iomsg=errmsg) this_input

        if (ierr == 0) then
            if (this_input < 0) then
                exit read
            else 

                if (allocated(inputs)) then
                ! If allocated, extend the array
                    inputs = [inputs, this_input]
                else
                ! if not, allocate the array
                    inputs = [this_input]
                end if

                cycle read
            end if
        else 
            print *, "ERR: ", errmsg
            print *, "Please enter a positive real number (or negative to quit)"
        end if

    end do read


    arithmetic_mean = arithmetic(inputs)
    harmonic_mean = harmonic(inputs)
    rms_mean = rms(inputs)
    geom_mean = geometric(inputs)

    ! arithmetic_mean = mean(inputs)
    ! harmonic_mean = mean(inputs, "harmonic")
    ! rms_mean = mean(inputs, "rms")
    ! geom_mean = mean(inputs, "geometric")

    print *, "For the set: ", inputs
    print 1, "arithmetic_mean = ", arithmetic_mean
    print 1, "rms_mean = ", rms_mean
    print 1, "harmonic_mean", harmonic_mean
    print 1, "geometric_mean", geom_mean

    1 format(A, T20, F7.3)



    
end program

