! This program tests the radar module functionality

program radar_test

    use radar

    implicit none

    type(detection) :: det1, det2
    type(vector) :: my_velocity

    det1 = new_detection(range=10.0, azimuth=45.0, time=10.0)
    det2 = new_detection(range=15.0, azimuth=90.0, time=15.0)


    my_velocity = det1%compute_velocity(det2)


    print *, det1
    print *, det2

    print *, "The change in pos between det1 and det2 is: ", det1%pos_diff(det2)
    print *, "Velocity is :", my_velocity


    print *, "The difference in detection between 1 and 2 is: ", det1%time_diff(det2)


end program