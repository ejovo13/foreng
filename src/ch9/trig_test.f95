program trig_test

    use trig_degrees

    implicit none

    integer :: thi = 180
    real :: thr = 180.0
    real(8) :: thd = 180.0

    print *, "thi 180 as rad = ", deg2rad(thi)
    print *, "thd 180 as rad = ", deg2rad(thd)
    print *, "thr 180 as rad = ", deg2rad(thr)

    print *, "sind of 90 ", sind(90.0)
    print *, "cosd of 180 ", cosd(180.0)
    print *, "tand of 45 ", tand(45.0)



end program