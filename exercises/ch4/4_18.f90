! 1. Write a computer program to calculate the current flowing through this diode
! for all voltages from -1.0V to +0.6 V, in 0.1 V steps. Repeat this process for the 
! following temperatures: 75F, 100F, and 125F

program current
! Purpose: 
!   To calculate the current flowing through a semiconductor diode
implicit none

    real(16), dimension(3) :: T_f = [75, 100, 125]
    real(16), dimension(3) :: T_k
    real(16), parameter :: I_O = 2.0E-6
    real(16), dimension(17) :: v_D
    real(16), dimension(3,17) :: i_D
    integer :: i, j

    do i = 1,17
        v_D(i) = -1.0 + (0.1 * (i-1))
    end do
    
    ! print *, v_D


    T_k = to_kelvin(T_f)

    print *, " "
    print *, "Calculating the current flowing through a semiconductor diode for three separate temperatures"
    print *, "and voltages ranging from -1.0 to 0.6 V"
    print *, " "

    print 999, v_D
    print 100

    100 format(250("-"))

    do i = 1,3
        do j = 1,17
            i_D(i,j) = calc_current_flow(v_D(j), I_O, T_k(i))            
        end do
        print 99, T_k(i), i_D(i,:)
        99 format(F9.3, 2X, " | ", 17(ES10.3, 3X))
        999 format("  T(K) \ V ", " | ", 17(F6.3, 7X))
    end do

contains 

    elemental function calc_current_flow(v_D_, I_O_, T_) result(i_D_)
    ! Data Dictionary
        real(16) :: i_D_                              ! The current flow through the diode in amperes
        real(16), intent(in) :: v_D_                  ! The voltage across the diode, in volts
        real(16), intent(in) :: I_O_                 ! The leakage current of the diode, in amperes
        real(16), intent(in) :: T_                    ! Temperature, in kelvins (K)
        real(16), parameter :: q_ = 1.602E-19         ! The charge on an electron, 1.602 X 10-19 Coulombs
        real(16), parameter :: k_ = 1.38E-23          ! Boltzmann's constant, 1.38 X 10-23 J/K
        
        i_D_ = I_O_ * (exp((q_ * v_D_)/(k_*T_)) - 1)

    end function

    ! This function takes a temperature in Fahrenheit and converts it to Kelvin
    elemental real(16) function to_kelvin(F)  
        real(16), intent(in) :: F
        to_kelvin = to_celcius(F) + 273
    end function

    ! Convert a temperature from Fahrenheit to celcius
    elemental real(16) function to_celcius(F)
    real(16), intent(in) :: F
        to_celcius = (F - 32) * (5./9.)
    end function


end program

