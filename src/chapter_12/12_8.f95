! Write a function that takes two points and returns a type line
! containing the slope and y-intercept of the line.

! This function is written in the ejovo_types@point.f95 file

program find_line_test

use ejovo_types

implicit none

    type(point) :: p1, p2, origin
    type(line) :: line1, line2, line3, line4, line5

    p1 = point(4,5)
    p2 = point(5,6)
    origin = point(0,0)

    line1 = find_line(p1, p2)
    line2 = find_line(origin, p1)
    line3 = find_line(origin, p2)
    line4 = find_line(p1, p1)
    line5 = find_line(point(0,4), p1)

    print *, "line 1"
    call line1%print
    print *, "line 2"
    call line2%print
    print *, "line 3"
    call line3%print
    print *, "line 4"
    call line4%print
    print *, "line 5"
    call line5%print

end program

! Definitely works