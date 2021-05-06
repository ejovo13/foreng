program sets_test

use foreng_math
implicit none

    ! integer, dimension(10) :: setA = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    ! integer, dimension(10) :: setB = [1, -10, 3, 5, -13, 6, -2, 8, 13, 10]

    real(real32), dimension(10) :: setA = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    real(real32), dimension(10) :: setB = [1, -10, 3, 5, -13, 6, -2, 8, 13, 10]

    ! real(real64), dimension(10) :: setA = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    ! real(real64), dimension(10) :: setB = [1, -10, 3, 5, -13, 6, -2, 8, 13, 10]

    ! integer, dimension(:), allocatable :: uni_int, inters_int
    real(real32), dimension(:), allocatable :: uni, inters
    ! real(real64), dimension(:), allocatable :: uni, inters

    uni = union(setA, setB)
    inters = intersection(setA, setB)

    print *, "For:"
    print *, "setA: ", setA
    print *, "setB: ", setB

    print *, "============================"

    print *, "union:        ", uni
    print *, "intersection: ", inters

end program