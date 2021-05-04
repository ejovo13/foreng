!! Tension on a cable

!! A 200kg object is to be hund from the end of a rigid 3-m horizontal pole
! of negligible weigh. The pole is attached to a wall by a pivot and is
! supported by a 3-m cable that is attached to the wall at a higher point.
! The tension on this cable is given by the equation:

!
! T = (W * lc * lp) / (d * sqrt(lp^2 - d^2))
!

program tension

implicit none

    !! Data Dictionary:
    real :: W = 200.0
    real :: T
    real :: lc = 3.0
    real :: lp = 3.0
    real :: d
    real :: T_min = huge(d)
    real :: d_min = 0.0
    integer :: i

    print 888
    print 999
    print 888


    ! do i = 1, 24
    do i = -4, 30

        d = (0.5 + (i-1)*0.1)
        ! print *, "Calculating tension for d: ", d
        T = calc_tension(W, lc, lp, d)

        if (T < T_min) then
            T_min = T
            d_min = d
        end if

        print 111, d, T
        
        888 FORMAT(21("="))
        999 FORMAT("|", T5, "d", T10, "|", T15, "T", T21, "|")
        111 FORMAT("|", F7.3, " |", F9.3, " |")

    end do

    print 888

    123 format(A, F7.3)
    print 123, "Minimum tension occurs at d = ", d_min

contains 

    real function calc_tension(W_, lc_, lp_, d_) result (T_)

        real, intent(in) :: W_  !! Weight of the object (kg)
        real, intent(in) :: lc_ !! length of the cable (m)
        real, intent(in) :: lp_ !! Length of the pole (m)
        real, intent(in) :: d_  !! Distance to attach the weight (m)

        T_ = (W_ * lc_ * lp_) / (d_ * sqrt(lp_**2 - d_**2))

    end function



end program
