submodule (trig_degrees) trig_exec

    contains
    module procedure sind
        sind = sin(deg2rad(theta))
    end procedure

    module procedure cosd
        cosd = cos(deg2rad(theta))
    end procedure

    module procedure tand
        tand = tan(deg2rad(theta))
    end procedure

    module procedure acosd
        acosd = acos(deg2rad(theta))
    end procedure

    module procedure asind
        asind = asin(deg2rad(theta))
    end procedure

    module procedure atand
        atand = atan(deg2rad(theta))
    end procedure

end submodule