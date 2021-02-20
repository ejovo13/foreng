! Calculating Orbits. 
!
! When a satellinte orbits the Eath, the satellite's orbit will form an ellipse with the 
! Earth located at one of the focal points of the ellipse. The satellite's orbit can be 
! expressed in polar coordintaes as
!
! r = p/(1 - \epsilon * cos( \theta ))
!
! Where r and theta are the distance and angle of the satellite from the center of the earth,
! p is a parameter specifying the size of the orbit, and \epsilon is a parameter representing the
! eccentricity of the orbit. A circular orbit has an eccentricity of zero. An elliptical orbit has 
! an eccentricity of 0 <= e <= 1. If \epsilon > 1, the satellite follows a hyperbolic path and escapes
! from Earth's gravitational field.
!
! Consider a satellite with a size parameter p = 1200 km. Write a program to calculate the 
! distance of the satellite from the center of the Earth as a function of theta if the satellite has 
! an eccentricity of (a) e = 0; (b) e = 0.25; (c) e = 0.5. Write a single program in which
! r and e are both input values. 
!
! How close does each orbit come to the earth? How far away does each orbit get from the earth?

program orbit
!   Purpose:
!      To calculate the position of a satellite orbiting the Earth.
implicit none

! Data dictionary: declare variable types, definitions, and units
integer, parameter :: P = 1200                      ! Size of the orbit, in km.
integer, parameter :: NTHETA = 200
real, parameter :: PI = 3.1415927
real, parameter :: THETA_STEP = (2*PI)/NTHETA
real :: eccentricity                                ! Eccentricity of the orbit, 0 <= e <= 1
real :: theta                                       ! Polar coordinate for the satellite.
real :: r                                           ! Distance from the center of the earth.
real :: apoapsis = P                                ! Farthest distance from the center of the earth
real :: periapsis = P                               ! Closest distance from the center of the earth
integer :: ierr
character(30) :: errmsg
integer itheta


print *, "Please enter an eccentricity value, between 0 and 1."
read (*,*, iostat=ierr, iomsg=errmsg) eccentricity

do while (ierr /= 0 .and. (eccentricity > 1 .or. eccentricity < 0)) 
    print *, "Input invalid, please enter a real value between 0 and 1."
    read (*,*, iostat=ierr, iomsg=errmsg) eccentricity
end do

do itheta = 1, NTHETA
    theta = itheta*THETA_STEP
    r = p/(1 - (eccentricity * cos(theta)))
    if (r < periapsis) periapsis = r
    if (r > apoapsis) apoapsis = r
    print *, theta, r
end do

print *, "The periapsis is: ", periapsis, "km, and the apoapsis is: ", apoapsis, "km"

end program