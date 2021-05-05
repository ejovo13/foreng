!! Evaluating infinite series


!! The value of the exponential function can be calculated by a taylor series.

!! Write the fortran functions that calculates e^x using the first 12 terms of the infinte series


program test_exp_series

use ch7_functions

implicit none

    real, dimension(8) :: X = [-10., -5., -1., 0., 1., 5., 10., 15.]

    integer :: i

    1 format("|", T2, F4.0, X, "|", X, F15.5, T24, "|", X, F15.5, T41, "|", X, F15.5, X "|")

    do i = 1, 8

        print 1, X(i), exp(X(i)), exp_series(X(i)), exp_series_r64(real(X(i), real64))

    end do


end program