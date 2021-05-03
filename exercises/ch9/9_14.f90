! Simulating Dice Throws

! Assume that a programmer is writing a game program. As a part of the program, it is
! necessary to simulate the throw of a pair of dice. Write a subroutine called throw to 
! return two random values from 1 to 6 each time that it is called. 

module dice_util

    interface 
    module subroutine throw(result)
        integer, intent(inout) :: result

    end subroutine

    module integer function die()
        
    end function

    end interface

end module

submodule (dice_util) dice_exec

    contains
    module procedure throw
        result = die()
    end procedure

    module procedure die
        real :: temp
        call random_number(temp)
        die = int((temp*6) + 1)
    end procedure

end submodule

program dice_test
    use dice_util
    
    implicit none


    integer :: throw1, throw2

    call throw(throw1)
    call throw(throw2)

    print *, "Throw 1 :", throw1, " Throw 2:", throw2 


end program