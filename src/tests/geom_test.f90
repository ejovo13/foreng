program geom_test

use foreng_math_geom


implicit none

    type(point2) :: p1, p2, p3
    real(real64) :: dp

    p1 = point2(10, 9)
    p2 = p2%from_pol(10, 90)

    call p1%print()
    call p1%print_pol()

    call p2%print()
    call p2%print_pol()

    p3 = p1 + p2

    dp = p1 .dot. p2

    call p3%print()

    print *, "dot product = ", dp


end program