submodule (trig) trig_degrees

contains

    module procedure sind
        sind = sin(deg_to_rad(theta))
    end procedure

    module procedure cosd
        cosd = cos(deg_to_rad(theta))
    end procedure

    module procedure tand
        tand = tan(deg_to_rad(theta))
    end procedure

    module procedure acosd
        acosd = acos(deg_to_rad(theta))
    end procedure

    module procedure asind
        asind = asin(deg_to_rad(theta))
    end procedure

    module procedure atand
        atand = atan(deg_to_rad(theta))
    end procedure
    
end submodule