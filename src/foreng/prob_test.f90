program prob_test


use foreng_math
implicit none

    integer, dimension(1000) :: x
    real, dimension(100000) :: x_2
    
    integer, dimension(8) :: test = [1, 2, 3, 4, 5, 6, 7, 8]

    call fill_urand(x, 1, 10)
    call fill_urand(x_2, 1., 10.)

    print *, "minval = ", minval(x)
    print *, "maxval = ", maxval(x)


    print *, "std_dev(x) = ", std_dev(x)
    print *, "mean(x) = ", mean(x)
    print *, "harmonic_mean(x) = ", mean(x, "harmonic")
    print *, "rms_mean(x) = ", mean(x, "rms")
    print *, "geometric_mean(x) = ", mean(x, "geometric")

    print *
    print *

    print *, "minval = ", minval(x_2)
    print *, "maxval = ", maxval(x_2)


    print *, "std_dev(x) = ", std_dev(x_2)
    print *, "mean(x) = ", mean(x_2)
    print *, "harmonic_mean(x) = ", mean(x_2, "harmonic")
    print *, "rms_mean(x) = ", mean(x_2, "rms")
    print *, "geometric_mean(x) = ", mean(x_2, "geometric")

end program