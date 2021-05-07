module foreng_math_trig

use foreng_env

implicit none 

!=============================================================================!
!=                         Trigonometry Interface                            =!
!=============================================================================!
interface sind
!! Calculate \(\sin(\theta)\) where \(\theta\) is in degrees
!!```fortran
!!
!!real(real32) :: theta_r32 = 90
!!real(real64) :: theta_r64 = 30
!!
!!print *, sind(theta_r32) ! = 1.000 
!!print *, sind(theta_r64) ! = 0.500
!!```
    module procedure sind_r32
    module procedure sind_r64
end interface

interface cosd
!! Calculate \(\cos(\theta)\) where \(\theta\) is in degrees
!!```fortran
!!
!!real(real32) :: theta_r32 = 90
!!real(real64) :: theta_r64 = 60
!!
!!print *, cosd(theta_r32) ! = 0.000 
!!print *, cosd(theta_r64) ! = 0.500
!!```
    module procedure cosd_r32
    module procedure cosd_r64
end interface

interface tand
!! Calculate \(\tan(\theta)\) where \(\theta\) is in degrees
!!```fortran
!!
!!real(real32) :: theta_r32 = 45
!!real(real64) :: theta_r64 = -45
!!
!!print *, tand(theta_r32) ! = 1.000 
!!print *, tand(theta_r64) ! = -1.000
!!```
    module procedure tand_r32
    module procedure tand_r64
end interface

interface asind
!! Calculate \(\textrm{asin}(x)\) and return \(\theta\) in degrees
!!```fortran
!!
!!real(real32) :: x_r32 = 1
!!real(real64) :: x_r64 = 0
!!
!!print *, asind(x_r32) ! = 90.000
!!print *, asind(x_r64) ! = 0.000
!!```
    module procedure asind_r32
    module procedure asind_r64
end interface

interface acosd
!! Calculate \(\textrm{acos}(x)\) and return \(\theta\) in degrees
!!```fortran
!!
!!real(real32) :: x_r32 = 1
!!real(real64) :: x_r64 = 0
!!
!!print *, acosd(x_r32) ! = 0.000
!!print *, acosd(x_r64) ! = 90.000
!!```
    module procedure acosd_r32
    module procedure acosd_r64
end interface

interface atand
!! Calculate \(\textrm{atan}(x)\) and return \(\theta\) in degrees
!!```fortran
!!
!!real(real32) :: x_r32 = 1
!!real(real64) :: x_r64 = -1
!!
!!print *, acosd(x_r32) ! = 45.000
!!print *, acosd(x_r64) ! = -45.000
!!```
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
!! Convert \(\theta\) from degrees to radians
    module procedure deg_to_rad_r32
    module procedure deg_to_rad_r64
end interface

interface rad_to_deg
!! Conver \(\theta\) from radians to degrees
    module procedure rad_to_deg_r32
    module procedure rad_to_deg_r64
end interface

!=============================================================================!
!=                         Trigonometry Functions                            =!
!=============================================================================!

contains

    elemental real(real32) function sind_r32(theta) result(sind)
    !! Single precision
        real(real32), intent(in) :: theta !! \(\theta\) in degrees
        sind = sin(deg_to_rad(theta))
    end function

    elemental real(real64) function sind_r64(theta) result(sind)
    !! Double precision
        real(real64), intent(in) :: theta !! \(\theta\) in degrees
        sind = sin(deg_to_rad(theta))
    end function

    elemental real(real32) function cosd_r32(theta) result(cosd)
    !! Single precision
        real(real32), intent(in) :: theta !! \(\theta\) in degrees
        cosd = cos(deg_to_rad(theta))
    end function

    elemental real(real64) function cosd_r64(theta) result(cosd)
    !! Double precision
        real(real64), intent(in) :: theta !! \(\theta\) in degrees
        cosd = cos(deg_to_rad(theta))
    end function

    elemental real(real32) function tand_r32(theta) result(tand)
    !! Single precision
        real(real32), intent(in) :: theta !! \(\theta\) in degrees
        tand = tan(deg_to_rad(theta))
    end function

    elemental real(real64) function tand_r64(theta) result(tand)
    !! Double precision
        real(real64), intent(in) :: theta !! \(\theta\) in degrees
        tand = tan(deg_to_rad(theta))
    end function

    elemental function asind_r32(x) result(theta)
    !! Single precision
        real(real32), intent(in) :: x 
        real(real32) :: theta !! \(\theta\) in degrees
        theta = rad_to_deg(asin(x))
    end function

    elemental function asind_r64(x) result(theta)
    !! Double precision
        real(real64), intent(in) :: x 
        real(real64) :: theta !! \(\theta\) in degrees
        theta = rad_to_deg(asin(x))
    end function

    elemental function acosd_r32(x) result(theta)
    !! Single precision
        real(real32), intent(in) :: x
        real(real32) :: theta !! \(\theta\) in degrees
        theta = rad_to_deg(acos(x))
    end function

    elemental function acosd_r64(x) result(theta)
    !! Double precision
        real(real64), intent(in) :: x 
        real(real64) :: theta !! \(\theta\) in degrees
        theta = rad_to_deg(acos(x))
    end function

    elemental function atand_r32(x) result(theta)
    !! Single precision
        real(real32), intent(in) :: x
        real(real32) :: theta !! \(\theta\) in degrees
        theta = rad_to_deg(atan(x))
    end function

    elemental function atand_r64(x) result(theta)
    !! Double precision
        real(real64), intent(in) :: x 
        real(real64) :: theta !! \(\theta\) in degrees
        theta = rad_to_deg(atan(x))
    end function

    elemental real(real32) function sinhd_r32(theta) result(sinhd)
    !! Single precision
        real(real32), intent(in) :: theta !! \(\theta\) in degrees
        sinhd = sinh(deg_to_rad(theta))
    end function

    elemental real(real64) function sinhd_r64(theta) result(sinhd)
    !! Double precision
        real(real64), intent(in) :: theta !! \(\theta\) in degrees
        sinhd = sinh(deg_to_rad(theta))
    end function

    elemental real(real32) function coshd_r32(theta) result(coshd)
    !! Single precision
        real(real32), intent(in) :: theta !! \(\theta\) in degrees
        coshd = cosh(deg_to_rad(theta))
    end function

    elemental real(real64) function coshd_r64(theta) result(coshd)
    !! Double precision
        real(real64), intent(in) :: theta !! \(\theta\) in degrees
        coshd = cosh(deg_to_rad(theta))
    end function

    elemental real(real32) function tanhd_r32(theta) result(tanhd)
    !! Single precision
        real(real32), intent(in) :: theta !! \(\theta\) in degrees
        tanhd = tanh(deg_to_rad(theta))
    end function

    elemental real(real64) function tanhd_r64(theta) result(tanhd)
    !! Double precision
        real(real64), intent(in) :: theta !! \(\theta\) in degrees
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