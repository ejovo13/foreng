! Write a dividing algorithm for a complex number in polar coordinates

program test_div

    use polar_type

    implicit none

    type(polar) :: p1, p2, p3

    p1 = polar(10, 90)
    p2 = polar(5, 45)

    p3 = p1%div(p2)

    print *, p3

end program