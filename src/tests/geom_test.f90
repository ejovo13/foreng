program geom_test

use foreng_math_geom


implicit none

    type(point2) :: p1, p2, p3
    real(real64) :: dp
    type(point3) :: p3_1
    type(vector3) :: v1, v2, v3
    type(line2) :: line


    v1 = [1, 2, 3]
    v2 = vector3(4, 8, 1)
    p3_1 = [1, 2, 3]

    p1 = point2(10, 9)
    p2 = p2%from_pol(10, 90)

    line = [p1, p2]

    call line%print()

    print *, "line.atx(10) = ", line%atx(10.d0)

    call p1%print()
    call p1%print_pol()

    call p2%print()
    call p2%print_pol()

    p3 = p1 + p2

    ! dp = p1 .dot. p2

    call p3%print()

    call v1%print()

    v1 = v1 + v1

    call v1%print()


    v1 = 5 * v1


    call v1%print()


    v3 = v1 .cross. v2
    dp = v1 .dot. v2

    print *, "dp of v1: ", v1, " and v2: ", v2, " = ", dp
    print *, "v1 x v2 = ", v3

    v1 = scalar_times_vector3_int(5, v1)


    p2 = [3, 2]
    call p2%print()
    

    call v1%print()

    v1 = 0.02 * v1

    call v1%print()


    call p3_1%print_sph()


    ! print *, "dot product = ", dp


end program