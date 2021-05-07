! Write three Fortran functions to calculate the hyperbolic sine, cosine, and tangent functions:
!
! Use your functions to calculate the hyperbolic sines, cosines, and tangents of the following 
! values: -2, -1.5, -1.0, -0.5, -0.25, 0.0, 0.25, 0.5, 1.0, 1.5, 2.0
!
!
program hyperbolics_test
    use foreng_math
    implicit none
    real, dimension(11) :: inputs = [ -2., -1.5, -1.0, -0.5, -0.25, 0.0, 0.25, 0.5, 1.0, 1.5, 2.0]
    real :: x = 0.5    
    integer :: i

    x = rad_to_deg(x)

    print 300, "x"
    300 Format(5X, A, 6X, "sinh(x)", 2X, "cosh(x)", 2X, "tanh(x)")
    print *, "-----------------------------------"
    do i = 1,10
        x = inputs(i)
        print 200, x, sinhd(x), coshd(x), tanhd(x)
    end do

    200 Format(4F9.4)

end program hyperbolics_test
