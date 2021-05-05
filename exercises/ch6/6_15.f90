!! Power Supplied to an object

!! IF an object is being pushed by a force F at a velocity v, then the power supplied
!! to the object is given by the equation F .dot. v

!! Use the previous fortran program to calculate the power supplied by a Force of 
!! F = 4i + 3j - 2k to an o bject moving with a velocity of v = 4i - 2j + 1k

program power_supplied

implicit none

    real, dimension(3) :: F !! Force
    real, dimension(3) :: v !! velocity
    real :: P !! Power

    F = [4, 3, -2]
    v = [4, -2, 1]

    P = sum(F * v)

    print *, "Power supplied by force vector : ", F
    print *, "acting on velocity : ", v
    print *, " = ", P




end program