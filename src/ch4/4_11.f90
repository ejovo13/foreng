! Modify program ball from example 4-7 to read in the acceleration due to gravity
! at a particular location, and to calculate the maximum range of the ball for that acceleration.
! After modifying the program, run it with accelerations of -9.8, -9.7, and -9.6m/s2.

! What effect does the reduction in gravitational attraction have on the range ofthe ball?
! What effect does the reduction in gravitational attraction have on the best angle theta at which to throw the ball?

program ball_modified
! Purpose:
!
!       To test how the flight of a ball changes in different gravitational fields
!
!
! Data Dictionary: Declare constants and variables
implicit none
real :: g                                       ! The gravitational constant, g. This value will be read in at the start of every execution
real :: vx                                      ! The horizontal velocity of the ball
real :: vy                                      ! The vertical velocity of the ball
real :: v0 = 20                                 ! The initial velocity of the ball
real :: tf                                      ! Time when the ball lands
real :: max_distance = 0                        ! Maximum distance traveled
integer :: theta = 0                            ! Angle at which the ball is thrown
integer :: max_theta                            ! The theta for which the ball travels the most distance
real, dimension(0:90) :: distance_traveled      ! The distance traveled for each theta value
character(50) :: FMT                            ! The format of the output table
real, parameter :: PI = 3.1415927               ! Mathematical constant pi
integer :: ierr                                 ! flag for reading in the gravitational value
character(50) :: errmsg                         ! the error message for reagding in

FMT = "(I3, A, F5.2, A, F6.2, A, F5.2)"

100 print *, "Please enter a real, negative value for the gravitational constant"
read (*,*, iostat = ierr, iomsg = errmsg) g
if (ierr /= 0) then
    print *, "ierr = ", ierr, " iomsg = ", errmsg
    goto 100
end if 

print *, "Theta | time in air(s) | distance traveled(m) | vy"
print *, "--------------------------------------------------"

do theta = 0, 90, 1
    vx = v0 * cos((theta/180.)*PI)
    vy = v0 * sin((theta/180.)*PI)
    ! Find tf
    tf = vy/(-g/2)

    distance_traveled(theta) = tf * vx

    if (distance_traveled(theta) > max_distance) then
        max_distance = distance_traveled(theta)
        max_theta = theta
    end if 

    print (FMT), theta, " | ", tf, " | ", distance_traveled(theta), " | ", vy

end do

print *, "Max distance traveled: ", max_distance, "With theta: ", max_theta


end program
