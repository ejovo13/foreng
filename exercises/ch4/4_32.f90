!! Mean Time between failures

!! The reliability of a piece of electronic equipment is usually measured
! in terms of Mean Time between Failures.

!! If the system is structured like the one in figure 4-16, MTBF can be calculated
!! as the harmonic mean / number of components.

!! Write a program that reads in the number of series of components,
!! Then reads in the MTBFs for each component and then calculates the overall MTBF 
!! for the system.

program mean_time_between_failures
    
use ch4_means
implicit none 

    real :: this_input
    integer :: ierr, i
    integer :: num_series
    character(100) :: errmsg
    real, dimension(:), allocatable :: inputs

    real :: harmonic_mean
    real :: mtbf_all 

    print *, "Please enter the number of components in series"

    read_series: do 

        read (*,*, iostat=ierr, iomsg=errmsg) num_series

        if (ierr == 0) then
            if (num_series <= 0) then
                print *, "Please enter a number greater than 0"
                cycle read_series
            else 
                allocate(inputs(num_series))
                exit read_series
            end if
        else 
            print *, "ERR: ", errmsg
            print *, "Please enter a positive real number"
        end if

    end do read_series

    print *, "Please enter the mean time before failure for each component"

    i = 1

    read_mtbf: do 

        read (*,*, iostat=ierr, iomsg=errmsg) this_input

        if (ierr == 0) then
            inputs(i) = this_input
            i = i + 1
            if (i > num_series) then
                exit read_mtbf
            end if
            cycle read_mtbf
        else 
            print *, "ERR: ", errmsg
            print *, "Please enter a positive valid number"
        end if


    end do read_mtbf

    harmonic_mean = mean(inputs, type_="harmonic")
    mtbf_all = harmonic_mean/num_series

    print *, "Mean Time Before Failure for entire system: ", mtbf_all








end program