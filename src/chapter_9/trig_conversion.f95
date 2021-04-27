submodule (trig_degrees) trig_conversion
    contains
    module procedure deg2rad_i
        deg2rad_i = (theta / 180) * PI
    end procedure

    module procedure deg2rad_r
        deg2rad_r = (theta / 180.0) * real(PI)
    end procedure

    module procedure deg2rad_d
        deg2rad_d = (theta / 180.0) * PI
    end procedure

    module procedure rad2deg_i
        rad2deg_i = (theta * 180)/PI
    end procedure

    module procedure rad2deg_r
        rad2deg_r = (theta * 180.0)/real(PI)
    end procedure

    module procedure rad2deg_d
        rad2deg_d = (theta * 180.0)/PI
    end procedure

end submodule