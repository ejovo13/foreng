!! Gravitaional Force

!!
!!
!! The gravitational force F between two bodies of masses m1 and m2 is given by the equation:
!! F = G*m1 *m1 / r^2
!!
!! Where G is the gravitation constant (6.672 x 10^-11 N m^2/kg^2), m1 and m2 are the masses
!! of the bodies in kilograms, and r is the distance between the two bodies.
!!
!! Write a function to calculate the gravitation force between two bodies given their masses and
!! the distance between them. Test your function by determining the force on a 1000-kg satellite in
!! orbit 38,000 km above the earth

program gravitational_force

use iso_fortran_env
implicit none

    real(real64) :: satellite_mass = 1000 !! kg
    real(real64), parameter :: earth_mass = 5.98E24 !! kg
    real(real64) :: orbit_height = 3.8E7 !! meters

    print *, "Force on satellite = ", calc_gravity(satellite_mass, earth_mass, orbit_height)




contains 


    function calc_gravity(m1, m2, r) result(F)

        real(real64), parameter :: G = 6.672E-11 !! Gravitational constant, units = N*m^2/kg^2
        real(real64), intent(in) :: m1 !! mass of body 1 in kg
        real(real64), intent(in) :: m2 !! mass of body 2 in kg
        real(real64), intent(in) :: r  !! distance between bodies in meters

        real(real64) :: F !! Gravitational force

        F = (G * m1 * m2) / (r*r)


    end function



end program