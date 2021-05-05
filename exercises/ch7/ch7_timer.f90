module ch7_timer
!! Elapsed time subroutines
    implicit none

    integer, dimension(8) :: TICK !! Store the previous time from the data_and_time values output
    

contains 

    subroutine set_timer

        call date_and_time(values=TICK)

    end subroutine 

    subroutine elapsed_time(sec_elapsed) 
    !! store the elapsed time in seconds

        integer, intent(out) :: sec_elapsed
        integer, dimension(8) :: TICK_2
        integer :: delta_hour, delta_minute, delta_second

        call date_and_time(values=TICK_2)

        delta_hour = TICK_2(5) - TICK(5)
        delta_minute = TICK_2(6) - TICK(6)
        delta_second = TICK_2(7) - TICK(7)

        sec_elapsed = delta_hour * 3600 + delta_minute * 60 + delta_second

    end subroutine


end module