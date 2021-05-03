submodule (trig) trig_conversion
    contains
    module procedure deg_to_rad_int
        deg_to_rad_int = (theta / 180) * PI
    end procedure

    module procedure deg_to_rad_sgl
        deg_to_rad_sgl = (theta / 180.0) * real(PI)
    end procedure

    module procedure deg_to_rad_dbl
        deg_to_rad_dbl = (theta / 180.0) * PI
    end procedure

    module procedure rad_to_deg_int
        rad_to_deg_int = (theta * 180)/PI
    end procedure

    module procedure rad_to_deg_sgl
        rad_to_deg_sgl = (theta * 180.0)/real(PI)
    end procedure

    module procedure rad_to_deg_dbl
        rad_to_deg_dbl = (theta * 180.0)/PI
    end procedure

end submodule