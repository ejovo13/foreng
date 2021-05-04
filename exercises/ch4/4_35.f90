!! The lever

!! The force applied to a lever and the weight that can be lifted is given by the 
!! equation:

!!
!! F_app * d_1 = weight * d_2
!!
!! Where
!! F_app = force in newtons
!! d_1 = distance from fulcrum to where the weight is applied
!! d_2 = distance from fulcrum to location of the load
!! Weight = downward force of the load.

!! 
!! Assume that we have 400kg of weigths,
!! and we want to lift 600kg. What is the shortest d_1 that we can use?
!!

program lever

implicit none

    real :: F !! Applied force (in Newtons)
    real :: d_1 !! Distance to fulcrum from applied force (m)
    real :: d_2 !! Distance to fulcrum from load to lift (m)
    real :: W = 600!! downward force of load
    integer :: i
    real, parameter :: step_size = 0.1
    integer, parameter :: NUM_STEPS = 26
    real, parameter :: max_force = 400 !! The maximum force that we can apply

    real :: min_d1 = huge(W)


    d_2 = 1 !! Fixed at one meter

    1 format("|  F_app  |  d_1  | Weight |  d2  |")
    2 format("|", F9.3, T11, "|", X, F5.3, T19, "|", X, F7.3, T28, "|", F7.3, T35, "|")
    3 format(35("="))

    print 3
    print 1
    print 3

    do i = 1, NUM_STEPS

        d_1 = 0.5 + (i-1)*step_size
        
        F = (W * d_2) / d_1

        print 2, F, d_1, W, d_2

        if (F <= 400 .and. d_1 < min_d1) then
            min_d1 = d_1            
        end if

    end do

    print 3

    print *, "Minimum distance that can be used is:", min_d1


end program