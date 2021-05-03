! Create a set of elemental functions to calculate the sine, cosine, and tangent of an angle
! theta, where theta is measured in degrees. Create a set of ELEMENTAL functions to calculate
! the arcsine, arccosine, and arctangent functions, returning the results in degrees.

program trig_test

    use trig

    implicit none

    integer :: thi = 180
    real :: thr = 180.0
    real(8) :: thd = 180.0
    real, dimension(2,3) :: arr1 = reshape([10.0, 40.0, 20.0, 50.0, 30.0, 60.0 ], [2,3])
    real, dimension(2,3) :: out1
    integer :: i


    print *, "thi 180 as rad = ", deg_to_rad(thi)
    print *, "thd 180 as rad = ", deg_to_rad(thd)
    print *, "thr 180 as rad = ", deg_to_rad(thr)

    print *, "sind of 90 ", sind(90.0)
    print *, "cosd of 180 ", cosd(180.0)
    print *, "tand of 45 ", tand(45.0)

    print *, "Modified"

    out1 = sind(arr1)

    do i = 1,2
        print *, arr1(i,:)
    end do

    do i = 1,2
        print *, out1(i,:)
    end do


end program

