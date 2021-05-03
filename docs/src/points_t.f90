! A module that defines the polar type, which is a coordinate represented in
! polar coordinates.

module points_t

    type, abstract :: point
        integer :: kind = 8
    end type

    type, extends(point) :: polar
        real :: r
        real :: th
    contains
        ! generic :: operator(+) => add_pol
        procedure :: to_cart => pol_to_cart
        ! procedure :: add_pol => pol_plus_cart, pol_plus_pol
    end type

    type, extends(point) :: cart
        real :: x
        real :: y
    contains 
        ! generic :: operator(+) => add
        procedure :: to_pol => cart_to_pol
        ! procedure :: add => cart_plus_cart, cart_plus_pol
    end type

    interface 
    
        ! module type(polar) function pol_plus_pol(p1, p2)
        !     class(polar), intent(in) :: p1, p2
        ! end function

        ! module type(polar) function pol_plus_cart(p1, c1)
        !     class(polar), intent(in) :: p1
        !     class(cart), intent(in) :: c1
        ! end function

        ! module type(cart) function cart_plus_cart(c1, c2)
        !     class(cart), intent(in) :: c1, c2
        ! end function

        ! module type(cart) function cart_plus_pol(c1, p1)
        !     class(cart), intent(in) :: c1
        !     class(polar), intent(in) :: p1
        ! end function

        module type(cart) function pol_to_cart(p1) result(c1)
            class(polar), intent(in) :: p1
        end function

        module type(polar) function cart_to_pol(c1) result(p1)
            class(cart), intent(in) :: c1
        end function

    end interface 

end module

program test_drive

    use points_t

    type (polar) :: my_p
    type (cart) :: my_c

    my_p = polar(kind=8, r=3, th=10)

    print *, my_p
    my_c = my_p%to_cart()
    print *, my_c

end program