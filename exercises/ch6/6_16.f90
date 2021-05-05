!! Cross Product

!! Implement the cross product of two vectors

program crossy_product

use ch6_points

implicit none

    real, dimension(3) :: v1_ 
    real, dimension(3) :: v2_
    real, dimension(3) :: normal_

    v1_ = [5, -3, 2]
    v2_ = [2, 3, 4]

    normal_ = cross_product(v1_, v2_)

    print *, "The cross product of "
    print *, "v1: ", v1_
    print *, "v2: ", v2_
    print *, "is: ", normal_


end program