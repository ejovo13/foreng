!! Velocity of orbiting object!


!! The angular velocit of an object moving with velocity v at a distance r from the origin of 
!! the coordinate system is given by the equation 
!!
!! v = r x w
!!
!! where r is the distance in meters, w (omega) is the angular velocity in radians per second,
!! and v is the velocity in meters per second
!!
!! If the distance from the center of the earth to an orbiting satellite is r = 300,000i + 400,000j + 50,000k meters,
!! and the angular velocity of the sattelite is -6E-3i + 2E-3j -9E-4k radians per second, what is the velocity
!! of the satellite in meters per second?
!!

program velocity_orbiting

use ch6_points

implicit none

    real, dimension(3), parameter :: r = [3E5, 4E5, 5E4]
    real, dimension(3), parameter :: omega = [-6E-3, 2E-3, -9E-4]

    real, dimension(3) :: v

    v = cross_product(r, omega)

    print *, "The velocity of the satellite orbiting at position "
    print *, "r:                ", r
    print *, "angular velocity: ", omega
    print *, "is =              ", v





end program