! Dice simulation
!
! It is often useful to be able to simulate the throw of a fair die. Write a 
! Fortran function dice() that simulates the throw of a fair die by returning some
! random integer between 1 and 6 everytime that it is called.

program dice_simulation
    use ran001
    implicit NONE
    integer :: i, dice
    integer, parameter :: NDICE_ROLLS = 10000
    integer, dimension(NDICE_ROLLS) :: dice_rolls

    call seed(1)

    do i = 1,NDICE_ROLLS
        dice_rolls(i) = dice()
    end do 

    print *, sum(real(dice_rolls))/NDICE_ROLLS


end program

function dice() result(dice_outcome)
    use ran001
    implicit none
    real :: my_ran, lower_bound, upper_bound
    integer :: iran, dice_outcome
    
    call random0(my_ran)

    do iran = 1,6
        lower_bound = (iran - 1.)/6.
        upper_bound = (iran)/6.
        ! print *, "iran = ", iran, " lower_bound = ", lower_bound, " upper_bound = ", upper_bound
        if (my_ran > lower_bound .and. my_ran <= upper_bound) then
            ! print *, my_ran, " is between ", lower_bound, " and ", upper_bound
            dice_outcome = iran
        end if 
    end do

    ! print *, "The random value is: ", my_ran
    ! print *, "The dice outcome is: ", dice_outcome
end function
