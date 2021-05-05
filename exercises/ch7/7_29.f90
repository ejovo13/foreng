! Subroutines written in "ch7_timer"

program timer_test

use ch7_timer

implicit none

    integer, parameter :: num_loops = 1E9
    integer :: sum = 0, i
    integer :: sec_elapsed

    call set_timer()

    do i = 1, num_loops

        sum = sum + 1
        sum = sum - 1

    end do

    call elapsed_time(sec_elapsed)

    print *, "sum = ", sum
    print *, "Elapsed time = ", sec_elapsed

    !! This timer implementation as outlined by exercise 7_29 is quite terrible

end program
