! Cross Product
!
! Write a function to calculate the cross product of two vectors V1 and V2
! 
! where V = Vxi + Vyj + Vzk
! Use this function to calculate the cross product of the two vectors:
! V1 = [-2, 4, 0.5] and V2 = [0.5, 3, 2]

program cross_test

    implicit none
    real, dimension(3) :: v1, v2, v3
    integer :: output
    integer :: print_array

    v1 = [-2., 4., 0.5]
    v2 = [0.5, 3., 2.]
    v3 = cross_product(v1, v2)

    output = print_array(v1)
    output = print_array(v2)
    output = print_array(v3)

    contains 

        function cross_product(v1, v2) result(v3)
            implicit none
            real, dimension(3), intent(in) :: v1, v2
            real, dimension(3) :: v3
            integer, parameter :: x = 1, y = 2, z = 3
        
            v3(x) = v1(y)*v2(z) - v2(y)*v1(z)
            v3(y) = v1(z)*v2(x) - v2(z)*v1(x)
            v3(z) = v1(x)*v2(y) - v2(x)*v1(y)
        
        end function

end program


function print_array(v1) result(ioerr)
    integer :: ioerr
    real, dimension(3), intent(in) :: v1

    print *, v1
    ioerr = 0
end function