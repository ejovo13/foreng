!! The birthday problem

!!
!! Create an array of size n, and generate birthdays in the range 1 to 365.
!! If any of the arrays are identical, then there is a "success"
!!
!! Perform this experiment at least 10,000 times.
!! Calculate the probability that two or more people will have the same birthday for n = 2 ... 40

program birthday_problem

use ch6_sets
use prototype

implicit none

    integer, parameter :: N_EXPERIMENTS = 10000
    integer, dimension(:), allocatable :: birthdays
    integer :: i, j, n, k

    real, dimension(40) :: probabilities
    integer :: success_count

    probabilities(1) = 0

    do n = 2, 40

        success_count = 0
        allocate(birthdays(n))        
        
        run_experiment: do j = 1, N_EXPERIMENTS

            fill_birthdays: do i = 1, n

                birthdays(i) = int(urand(1.0, 366.0))

            end do fill_birthdays

            do k = 1, i - 1

                if(belongs_to(birthdays(k), birthdays(k+1:))) then
                !! If the birthdays shows up more than once
                    success_count = success_count + 1
                    cycle run_experiment
                end if

            end do

            ! print *, "Birthdays: ", birthdays

        end do run_experiment

        ! print *, "Success count = ", success_count
        probabilities(n) = real(success_count) / N_EXPERIMENTS

        deallocate(birthdays)

    end do

    3 format ("|  N  |   Prob   |")
    2 format (18("="))
    1 format("|", X, I0, T7, "|", X, F8.5, T18, "|")
    
    ! print *, "Probabilities: ", probabilities
    print 2
    print 3
    print 2

    do i = 1,40

        print 1, i, probabilities(i)

    end do

    print 2




end program