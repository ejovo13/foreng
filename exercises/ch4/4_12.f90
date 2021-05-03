! Modify program ball from Example 4-7 to read in the initial velocity with which the ball
! is thrown. After modifying the program, run it with initial velocities of 10, 20, and 20 
! m/sec. What effect does changin the initial velocity have on the range of the ball? What
! effect does it have on the best angle theta at which to throw the ball?

program ball_velocity_modified
! Purpose:
!
!       To test how the flight of a ball changes with different initial velocites
!
!
! Data Dictionary: Declare constants and variables
    implicit none
    real :: g = -9.8                                ! The gravitational constant, g. 
    real :: vx                                      ! The horizontal velocity of the ball, m/sec
    real :: vy                                      ! The vertical velocity of the ball
    real :: v0                                      ! The initial velocity of the ball, this will be read in upon execution
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
    
    100 print *, "Please enter a real value for the initial velocity"
    read (*,*, iostat = ierr, iomsg = errmsg) v0
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