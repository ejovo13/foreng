program flight_of_ball
! Assuming that a ball is thrown with initial velocity of 20 m/s from a height of 0, find the distance that the ball travels 
! for each theta, and then report which theta has the furthest distance travelled. This computation was made much simpler since 
! The ball started on the ground. Had it not, we would need to invoke the quadratic formula. (Which in hindsight is not too bad).
!
! Data dictionary:
! -----------------------------------------
! vx = horizontal component of velocity
! vy = vertical component of velocity
! v0 = initial velocity, 20 m/s
! theta = angle at which ball is released, in degrees
! tf = time when the ball lands
! distance_traveled = an array that keeps track of how long each throw travels along the horizontal component
! FMT = the format for printing a nice table
! max_distance = the maximum amount of distance traveled 
implicit none

real :: vx, vy, v0 = 20, tf, max_distance = 0
integer :: theta = 0, max_theta
real, dimension(0:90) :: distance_traveled
character(50) :: FMT
real, parameter :: PI = 3.1415927
FMT = "(I3, A, F5.2, A, F6.2, A, F5.2)"

print *, "Theta | time in air(s) | distance traveled(m) | vy"
print *, "--------------------------------------------------"

do theta = 0, 90, 1
    vx = v0 * cos((theta/180.)*PI)
    vy = v0 * sin((theta/180.)*PI)
    ! Find tf
    tf = vy/(4.9)

    distance_traveled(theta) = tf * vx

    if (distance_traveled(theta) > max_distance) then
        max_distance = distance_traveled(theta)
        max_theta = theta
    end if 

    print (FMT), theta, " | ", tf, " | ", distance_traveled(theta), " | ", vy

end do

print *, "Max distance traveled: ", max_distance, "With theta: ", max_theta

end program