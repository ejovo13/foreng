module ch6_points
implicit none

contains

    function cross_product(v1, v2) result (normal)

        real, dimension(3), intent(in) :: v1, v2 !! Vectors to cross multiply
        real, dimension(3) :: normal !! Normal vector that is the output

        normal(1) = v1(2)*v2(3) - v2(2)*v1(3)
        normal(2) = v1(3)*v2(1) - v2(3)*v1(1)
        normal(3) = v1(1)*v2(2) - v2(1)*v1(2)

    end function

    real function distance(p1, p2) result (d1)

        real, dimension(3), intent(in) :: p1, p2 !! Points to calculate distance between
    
        d1 = sqrt((p1(1) - p2(1))**2 + (p1(2) - p2(2))**2 + (p1(3) - p2(3))**2)

    end function

end module