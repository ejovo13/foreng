!! Distance between points

program distance_between_points

use ch6_points

implicit none

    integer :: ierr
    character(100) :: errmsg
    real, dimension(3) :: point1, point2


    print *, "Please enter the values for 2 3d points"

    read *, point1
    read *, point2

    print *, "Distance between points: ", distance(point1, point2)




end program