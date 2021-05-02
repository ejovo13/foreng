! This module implements the polar functions


submodule (points_t) polar

contains

    ! module procedure pol_plus_pol

    ! end procedure

    module procedure pol_to_cart
        c1%x = p1%r
        c1%y = p1%th
    end procedure

end submodule

submodule (points_t) cart
contains

    ! module procedure pol_plus_pol

    ! end procedure

    module procedure cart_to_pol
        p1%r = c1%x
        p1%th = c1%y
    end procedure

end submodule