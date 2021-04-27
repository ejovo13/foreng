! Write a function that calculates the distance between two values of 
! type "point".

! this function is contained in the ejovo_types@point.f95 source file.

program dist_test

    use ejovo_types

    implicit none

    type(point) :: origin, p1, p2

    origin = point(0, 0)
    p1 = point(1, 0)
    p2 = point(1, 1)

    print *, p1

    print *, p1%x
    print *, p1%y
    print *, "dist(p1, origin) = ", p1%dist(origin)
    print *, "dist(p2, origin) = ", p2%dist(origin)
    print *, "dist(p1, p2) = ", p1%dist(p2)
    print *, "dist(p1, p1) = ", p1%dist(p1)

end program