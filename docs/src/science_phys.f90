module foreng_science_phys

use foreng_env

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

end module