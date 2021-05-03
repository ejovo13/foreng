! This module contains some test functions to explore chapter 7 - procedures

module ejovo

save
contains
    subroutine return_10()
        
    end subroutine

    subroutine three_args(x, y, error)
        integer, intent(in) :: x, y
        integer, intent(out) :: error

        if (x > y) then
            error = 1
            print *, "Not supposed to happen, exiting program"
            stop "underflow"
        else 
            error = 0
            print *, "All is well"
        end if



    end subroutine

end module ejovo