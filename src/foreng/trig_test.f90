program trig_test

use foreng_math
implicit none

    real(real64) :: thetad = 90
    real(real64) :: theta 
    real(real64), dimension(3) :: max_vals = [-10.d0, 0d0, 5d0]

    theta = deg_to_rad(thetad)

    print *, "theta = ", theta
    print *, "thetad = ", thetad

    print *, "sin(theta) = ", sin(theta)
    print *, "sind(thetad) = ", sind(thetad)

    print *, "sinh(theta) = ", sinh(theta)
    print *, "sinhd(theta) = ", sinhd(thetad)

    print *, "MAXVAL(max_vals) = ", maxval(max_vals)

    print *, "asind(1.0) = ", asind(1.0)
    print *, "acosd(0.0) = ", acosd(1.0)

    print *, "sind(30) = ", sind(30d0)
    print *, "tand(-45) = ", tand(-45d0)

    print *, "atand(-1) = ", atand(-1d0)
    print *, "atand(1) = " , atand(1d0)


end program