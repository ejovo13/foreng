!! Decibels
!! The ratio of two power measurements is in decibels, or dB
!! The ratio is described as: dB = log_10(P_2/P_1)
!! Write a program that calculates the decibel level corresponding to power
!! levels between 1 and 20W, in 0.5 W steps, Assuming that P_1 = 1 W

program decibels
implicit none

    real :: P1 = 1 !! Power level 1, in Watts
    real :: P2 = 0.5 !! Power level 2, in Watts
    integer :: i

    1 format("| Power 1 | Power 2 |   dB   |")
    2 format("|", F7.3, T11, "|", X, F7.3, T21, "|", F7.3, T30, "|")
    3 format(30("="))

    print 1
    print 3

    do i = 0, 38

        P2 = P2 + i*0.5

        print 2, P1, P2, calc_decibels(P1, P2)

    end do


contains 


    real function calc_decibels(P1_, P2_) result(dB_)

        real, intent(in) :: P1_, P2_

        dB_ = log10(P2_/P1_)

    end function




end program
