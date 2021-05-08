module foreng_science_phys

use foreng_env
use foreng_science_const

implicit none

contains

    function calc_tension(W_, lc_, lp_, d_) result (T_)

        real, intent(in) :: W_  !! Weight of the object (kg)
        real, intent(in) :: lc_ !! length of the cable (m)
        real, intent(in) :: lp_ !! Length of the pole (m)
        real, intent(in) :: d_  !! Distance to attach the weight (m)

        real :: T_ !! Tension in N

        T_ = (W_ * lc_ * lp_) / (d_ * sqrt(lp_**2 - d_**2))

    end function

    function calc_decibels(P1_, P2_) result(dB_)

        real, intent(in) :: P1_, P2_

        real :: dB_ 

        dB_ = log10(P2_/P1_)

    end function

    function calc_gravity(m1, m2, r) result(F)

        real(real64), intent(in) :: m1 !! mass of body 1 in kg
        real(real64), intent(in) :: m2 !! mass of body 2 in kg
        real(real64), intent(in) :: r  !! distance between bodies in meters

        real(real64) :: F !! Gravitational force

        F = (GRAVITATIONAL_CONSTANT * m1 * m2) / (r*r)


    end function

end module