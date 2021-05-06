submodule (foreng_math) trig

implicit none 


contains

    elemental real(real32) function sind_r32(theta) result(sind)
        real(real32), intent(in) :: theta
        sind = sin(deg_to_rad(theta))
    end function

    module procedure sind_r64
        sind = sin(deg_to_rad(theta))
    end procedure

    module procedure cosd_r32
        cosd = cos(deg_to_rad(theta))
    end procedure

    module procedure cosd_r64
        cosd = cos(deg_to_rad(theta))
    end procedure

    module procedure tand_r32
        tand = tan(deg_to_rad(theta))
    end procedure

    module procedure tand_r64
        tand = tan(deg_to_rad(theta))
    end procedure

    module procedure asind_r32
        asind = asin(deg_to_rad(theta))
    end procedure

    module procedure asind_r64
        asind = asin(deg_to_rad(theta))
    end procedure

    module procedure acosd_r32
        acosd = acos(deg_to_rad(theta))
    end procedure

    module procedure acosd_r64
        acosd = acos(deg_to_rad(theta))
    end procedure

    module procedure atand_r32
        atand = atan(deg_to_rad(theta))
    end procedure

    module procedure atand_r64
        atand = atan(deg_to_rad(theta))
    end procedure

    module procedure sinhd_r32
        sinhd = sinh(deg_to_rad(theta))
    end procedure

    module procedure sinhd_r64
        sinhd = sinh(deg_to_rad(theta))
    end procedure

    module procedure coshd_r32
        coshd = cosh(deg_to_rad(theta))
    end procedure

    module procedure coshd_r64
        coshd = cosh(deg_to_rad(theta))
    end procedure

    module procedure tanhd_r32
        tanhd = tanh(deg_to_rad(theta))
    end procedure

    module procedure tanhd_r64
        tanhd = tanh(deg_to_rad(theta))
    end procedure

    module procedure deg_to_rad_r32
        radians = degrees/180_real32 * PI_32
    end procedure

    module procedure deg_to_rad_r64
        radians = degrees/180_real64 * PI
    end procedure

    module procedure rad_to_deg_r32
        degrees = radians/PI_32 * 180_real32
    end procedure

    module procedure rad_to_deg_r64
        degrees = radians/PI * 180_real64
    end procedure



end submodule