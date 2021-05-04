!! Ideal gas law.
!!
!! PV = nRT
!!
!! (a) assume 1 mole of gas and 273 K 
!! (b) Write a program to print out the volume of the gas as 
!! its pressure varies from 1 to 1001 kPa in steps of 100
!! (c) Suppose that the temperatur is increaded to 300 K. Now what does the 
!! distribution look like?
!!

program ideal_gas

use iso_fortran_env

implicit none

    real, parameter :: avogadro = 6.02E23 !! Number of molecules in a mol
    real, parameter :: R = 8.314 !! Universal gas constant (L * kPa/mol * K)
    integer, parameter :: STEP_SIZE = 100 !! Step size of kPa in do loop
    real :: T !! Absolute temperature in K
    real :: P !! Pressure in kPa
    real :: V !! Volume in liters
    real :: n !! Number of moles

    integer :: i

    P = 1
    T = 273
    n = 1

    print *, "At T = 273, n = 1: "

    1 format("|Pressure (kpa)| Volume (L) |")
    2 format("|", X, F10.3, T16, "|",  X, F10.3, T29, "|")
    3 format(29("="))

    print 3
    print 1
    print 3

    do i = 1,11

        P = P + STEP_SIZE

        V = (n * R * T)/P

        print 2, P, V

    end do
    print 3

    P = 1
    T = 300

    print *, "At T = 300, n = 1: "

    print 3
    print 1
    print 3

    do i = 1,11

        P = P + STEP_SIZE

        V = (n * R * T)/P

        print 2, P, V

    end do
    print 3




end program