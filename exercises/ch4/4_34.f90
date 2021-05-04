!! Ideal Gas law continued

!! Assume that a container has fixed volume at 10L.

!! Calculate the pressure of the gas as the temperature is changed from 250 to 400 Kelvin

program ideal_gas_part2

use iso_fortran_env

implicit none
    
    real, parameter :: avogadro = 6.02E23 !! Number of molecules in a mol
    real, parameter :: R = 8.314 !! Universal gas constant (L * kPa/mol * K)
    integer, parameter :: STEP_SIZE = 10 !! Step size of Temperature in do loop
    real :: T !! Absolute temperature in K
    real :: P !! Pressure in kPa
    real :: V !! Volume in liters
    real :: n !! Number of moles

    integer :: i

    P = 0
    T = 250
    n = 1

    print *, "At T = 273, n = 1: "

    1 format("|  Temp (K)  |Pressure (kPA)|")
    2 format("|", X, F10.3, T16, "|",  X, F10.3, T29, "|")
    3 format(29("="))

    print 3
    print 1
    print 3

    do i = 1,16

        T = T + STEP_SIZE

        P = (T * n * R)/V

        print 2, T, P

    end do
    print 3

    P = 1
    T = 300

    print *, "At T = 300, n = 1: "

    print 3
    print 1
    print 3

    do i = 1,16

        T = T + STEP_SIZE

        P = (T * n * R)/V

        print 2, T, P

    end do
    print 3
      
end program
