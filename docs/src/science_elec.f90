module foreng_science_elec

    use foreng_env

contains 




    elemental function calc_current_flow(v_D_, I_O_, T_) result(i_D_)
    ! Data Dictionary
        real(16) :: i_D_                              !! Current flow through the diode in amperes
        real(16), intent(in) :: v_D_                  !! Voltage across the diode, in volts
        real(16), intent(in) :: I_O_                  !! Leakage current of the diode, in amperes
        real(16), intent(in) :: T_                    !! Temperature, in kelvins (K)
        real(16), parameter :: q_ = 1.602E-19         ! The charge on an electron, 1.602 X 10-19 Coulombs
        real(16), parameter :: k_ = 1.38E-23          ! Boltzmann's constant, 1.38 X 10-23 J/K
        
        i_D_ = I_O_ * (exp((q_ * v_D_)/(k_*T_)) - 1)

    end function


end module