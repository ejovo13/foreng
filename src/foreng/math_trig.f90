module foreng_math_trig

use foreng_env

implicit none 

!=============================================================================!
!=                         Trigonometry Interface                            =!
!=============================================================================!
interface sind
!! This is an interface comment
!!```fortran
!!
!!print *, "sup bitch"
!!
!!
!!``
    module procedure sind_r32
    !! This is an interface procedure comment
    module procedure sind_r64
end interface

interface cosd
    module procedure cosd_r32
    module procedure cosd_r64
end interface

interface tand
    module procedure tand_r32
    module procedure tand_r64
end interface

interface asind
    module procedure asind_r32
    module procedure asind_r64
end interface

interface acosd
    module procedure acosd_r32
    module procedure acosd_r64
end interface

interface atand
    module procedure atand_r32
    module procedure atand_r64
end interface

interface sinhd
    module procedure sinhd_r32
    module procedure sinhd_r64
end interface

interface coshd
    module procedure coshd_r32
    module procedure coshd_r64
end interface

interface tanhd
    module procedure tanhd_r32
    module procedure tanhd_r64
end interface

interface deg_to_rad    
    module procedure deg_to_rad_r32
    module procedure deg_to_rad_r64
end interface

interface rad_to_deg
    module procedure rad_to_deg_r32
    module procedure rad_to_deg_r64
end interface

!=============================================================================!
!=                         Trigonometry Functions                            =!
!=============================================================================!

contains

    elemental real(real32) function sind_r32(theta) result(sind)
    !! This is a function definition comment
        real(real32), intent(in) :: theta
        sind = sin(deg_to_rad(theta))
    end function

    elemental real(real64) function sind_r64(theta) result(sind)
        real(real64), intent(in) :: theta
        sind = sin(deg_to_rad(theta))
    end function

    elemental real(real32) function cosd_r32(theta) result(cosd)
        real(real32), intent(in) :: theta
        cosd = cos(deg_to_rad(theta))
    end function

    elemental real(real64) function cosd_r64(theta) result(cosd)
        real(real64), intent(in) :: theta
        cosd = cos(deg_to_rad(theta))
    end function

    elemental real(real32) function tand_r32(theta) result(tand)
        real(real32), intent(in) :: theta
        tand = tan(deg_to_rad(theta))
    end function

    elemental real(real64) function tand_r64(theta) result(tand)
        real(real64), intent(in) :: theta
        tand = tan(deg_to_rad(theta))
    end function

    elemental real(real32) function asind_r32(theta) result(asind)
        real(real32), intent(in) :: theta
        asind = asin(deg_to_rad(theta))
    end function

    elemental real(real64) function asind_r64(theta) result(asind)
        real(real64), intent(in) :: theta
        asind = asin(deg_to_rad(theta))
    end function

    elemental real(real32) function acosd_r32(theta) result(acosd)
        real(real32), intent(in) :: theta
        acosd = acos(deg_to_rad(theta))
    end function

    elemental real(real64) function acosd_r64(theta) result(acosd)
        real(real64), intent(in) :: theta
        acosd = acos(deg_to_rad(theta))
    end function

    elemental real(real32) function atand_r32(theta) result(atand)
        real(real32), intent(in) :: theta
        atand = atan(deg_to_rad(theta))
    end function

    elemental real(real64) function atand_r64(theta) result(atand)
        real(real64), intent(in) :: theta
        atand = atan(deg_to_rad(theta))
    end function

    elemental real(real32) function sinhd_r32(theta) result(sinhd)
        real(real32), intent(in) :: theta
        sinhd = sinh(deg_to_rad(theta))
    end function

    elemental real(real64) function sinhd_r64(theta) result(sinhd)
        real(real64), intent(in) :: theta
        sinhd = sinh(deg_to_rad(theta))
    end function

    elemental real(real32) function coshd_r32(theta) result(coshd)
        real(real32), intent(in) :: theta
        coshd = cosh(deg_to_rad(theta))
    end function

    elemental real(real64) function coshd_r64(theta) result(coshd)
        real(real64), intent(in) :: theta
        coshd = cosh(deg_to_rad(theta))
    end function

    elemental real(real32) function tanhd_r32(theta) result(tanhd)
        real(real32), intent(in) :: theta
        tanhd = tanh(deg_to_rad(theta))
    end function

    elemental real(real64) function tanhd_r64(theta) result(tanhd)
        real(real64), intent(in) :: theta
        tanhd = tanh(deg_to_rad(theta))
    end function

    elemental real(real32) function deg_to_rad_r32(degrees) result(radians)
        real(real32), intent(in) :: degrees
        radians = degrees/180_real32 * PI_32
    end function

    elemental real(real64) function deg_to_rad_r64(degrees) result(radians)
        real(real64), intent(in) :: degrees
        radians = degrees/180_real64 * PI
    end function

    elemental real(real32) function rad_to_deg_r32(radians) result(degrees)
        real(real32), intent(in) :: radians
        degrees = radians/PI_32 * 180_real32
    end function

    elemental real(real64) function rad_to_deg_r64(radians) result(degrees)
        real(real64), intent(in) :: radians
        degrees = radians/PI * 180_real64
    end function

end module