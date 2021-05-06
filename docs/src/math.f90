module foreng_math
!! Top level module containing math functions (sets, factorials, trig)

use foreng
implicit none

! private :: EPS_32, EPS_64

! real(real32) :: EPS_32 = 1E-7
! real(real64) :: EPS_64 = 1E-14
    
!=============================================================================!
!=                         Trigonometry Interface                            =!
!=============================================================================!
interface sind
    module procedure sind_r32
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
!=                               Set Interface                               =!
!=============================================================================!

interface intersection 
    module procedure intersection_int
    module procedure intersection_r32
    module procedure intersection_r64

end interface

interface union
    module procedure union_int
    module procedure union_r32
    module procedure union_r64
end interface

interface belongs_to
    module procedure belongs_to_int
    module procedure belongs_to_r32
    module procedure belongs_to_r64
end interface

interface array_epsilon
    module procedure array_epsilon_r32
    module procedure array_epsilon_r64
end interface

interface abs_max_val
    module procedure abs_max_val_r32
    module procedure abs_max_val_r64   
end interface 



!=============================================================================!
!=                         Trigonometry Functions                            =!
!=============================================================================!
interface 

    module elemental real(real32) function sind_r32(theta) result(sind)
        real(real32), intent(in) :: theta
    end function

    module elemental real(real64) function sind_r64(theta) result(sind)
        real(real64), intent(in) :: theta
    end function

    module elemental real(real32) function cosd_r32(theta) result(cosd)
        real(real32), intent(in) :: theta
    end function

    module elemental real(real64) function cosd_r64(theta) result(cosd)
        real(real64), intent(in) :: theta
    end function

    module elemental real(real32) function tand_r32(theta) result(tand)
        real(real32), intent(in) :: theta
    end function

    module elemental real(real64) function tand_r64(theta) result(tand)
        real(real64), intent(in) :: theta
    end function

    module elemental real(real32) function asind_r32(theta) result(asind)
        real(real32), intent(in) :: theta
    end function

    module elemental real(real64) function asind_r64(theta) result(asind)
        real(real64), intent(in) :: theta
    end function

    module elemental real(real32) function acosd_r32(theta) result(acosd)
        real(real32), intent(in) :: theta
    end function

    module elemental real(real64) function acosd_r64(theta) result(acosd)
        real(real64), intent(in) :: theta
    end function

    module elemental real(real32) function atand_r32(theta) result(atand)
        real(real32), intent(in) :: theta
    end function

    module elemental real(real64) function atand_r64(theta) result(atand)
        real(real64), intent(in) :: theta
    end function

    module elemental real(real32) function sinhd_r32(theta) result(sinhd)
        real(real32), intent(in) :: theta
    end function

    module elemental real(real64) function sinhd_r64(theta) result(sinhd)
        real(real64), intent(in) :: theta
    end function

    module elemental real(real32) function coshd_r32(theta) result(coshd)
        real(real32), intent(in) :: theta
    end function

    module elemental real(real64) function coshd_r64(theta) result(coshd)
        real(real64), intent(in) :: theta
    end function

    module elemental real(real32) function tanhd_r32(theta) result(tanhd)
        real(real32), intent(in) :: theta
    end function

    module elemental real(real64) function tanhd_r64(theta) result(tanhd)
        real(real64), intent(in) :: theta
    end function

    module elemental real(real32) function deg_to_rad_r32(degrees) result(radians)
        real(real32), intent(in) :: degrees
    end function

    module elemental real(real64) function deg_to_rad_r64(degrees) result(radians)
        real(real64), intent(in) :: degrees
    end function

    module elemental real(real32) function rad_to_deg_r32(radians) result(degrees)
        real(real32), intent(in) :: radians
    end function

    module elemental real(real64) function rad_to_deg_r64(radians) result(degrees)
        real(real64), intent(in) :: radians
    end function

end interface

!=============================================================================!
!=                               Set Functions                               =!
!=============================================================================!
interface 

    module function intersection_int(setA, setB) result(inters)
        integer, dimension(:), intent(in) :: setA, setB
        integer, dimension(:), allocatable :: inters
    end function

    module function intersection_r32(setA, setB) result(inters)
        real(real32), dimension(:), intent(in) :: setA, setB
        real(real32), dimension(:), allocatable :: inters
    end function

    module function intersection_r64(setA, setB) result(inters)
        real(real64), dimension(:), intent(in) :: setA, setB
        real(real64), dimension(:), allocatable :: inters
    end function

    module function union_int(setA, setB) result(union_set)
        integer, intent(in), dimension(:) :: setA, setB
        integer, dimension(:), allocatable :: union_set
    end function

    module function union_r32(setA, setB) result(union_set)
        real(real32), intent(in), dimension(:) :: setA, setB
        real(real32), dimension(:), allocatable :: union_set
    end function

    module function union_r64(setA, setB) result(union_set)
        real(real64), intent(in), dimension(:) :: setA, setB
        real(real64), dimension(:), allocatable :: union_set
    end function

    module logical function belongs_to_int(x, A) result(bool)
        integer, intent(in) :: x 
        integer, dimension(:) :: A
    end function

    module logical function belongs_to_r32(x, A, eps) result(bool)
        real(real32), intent(in) :: x 
        real(real32), dimension(:) :: A
        real(real32), optional :: eps
    end function

    module logical function belongs_to_r64(x, A, eps) result(bool)
        real(real64), intent(in) :: x 
        real(real64), dimension(:) :: A
        real(real64), optional :: eps
    end function

    module real(real32) function array_epsilon_r32(array) result(eps)
        real(real32), dimension(:), intent(in) :: array        
    end function

    module real(real64) function array_epsilon_r64(array) result(eps)
        real(real64), dimension(:), intent(in) :: array        
    end function

    module real(real32) function abs_max_val_r32(array) result(abs_max_val)
        real(real32), dimension(:), intent(in) :: array
    end function

    module real(real64) function abs_max_val_r64(array) result(abs_max_val)
        real(real64), dimension(:), intent(in) :: array
    end function


end interface 



end module