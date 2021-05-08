!! Module for use with geometry (points, vectors, lines)

module foreng_math_geom

use foreng_env
use foreng_math_trig
implicit none

! // TODO add types : point2d, point3d, vector3d
! // TODO add point conversion (polar to cartesioan)
! // TODO add point3 conversion (cartesian to spherical)

type :: point2
!! A point object with coordinates stored in cartesian coordinates

    real(real64) :: x
    real(real64) :: y

contains 

    ! procedure, nopass :: from_pol => point2_from_polar_r32, point2_from_polar_r64, point2_from_polar_int
    procedure :: print => point2_print_cartesian
    procedure :: print_pol => point2_print_polar
    procedure :: rho => point2_rho
    procedure :: theta => point2_theta
    generic, public :: from_pol => from_pol_r32, from_pol_r64, from_pol_int
    !! This is the comment for a generic function
    procedure, nopass :: from_pol_r32 => point2_from_polar_r32
    procedure, nopass :: from_pol_r64 => point2_from_polar_r64
    procedure, nopass :: from_pol_int => point2_from_polar_int


end type


type :: point3




end type


type :: vector3d




end type

contains 

!=============================================================================!
!=                             Point2 Functions                              =!
!=============================================================================!

    function point2_from_polar_r32(rho, theta) result(point)

        real(real32), intent(in) :: rho
        real(real32), intent(in) :: theta

        type(point2) :: point

        point%x = rho * cosd(theta)
        point%y = rho * sind(theta)

    end function

    function point2_from_polar_r64(rho, theta) result(point)

        real(real64), intent(in) :: rho
        real(real64), intent(in) :: theta

        type(point2) :: point

        point%x = rho * cosd(theta)
        point%y = rho * sind(theta)

    end function

    function point2_from_polar_int(rho, theta) result(point)

        integer, intent(in) :: rho
        integer, intent(in) :: theta

        type(point2) :: point

        point%x = rho * cosd(real(theta,real64))
        point%y = rho * sind(real(theta, real64))

    end function

    subroutine point2_print_cartesian(self)

        class(point2), intent(in) :: self

        print 1, self%x, self%y

        1 format ("(", G11.5, "," G11.5, ")")

    end subroutine

    subroutine point2_print_polar(self)
    !! Print the polar coordinates of a point in this form
        class(point2), intent(in) :: self

        print 1, self%rho(), self%theta()

        1 format ("(", G11.5, "," G11.5, ")")

    end subroutine

    function point2_rho(self) result(rho)
    !! Calculate the magnitude (distance from the origin) of 
        class(point2), intent(in) :: self

        real(real64) :: rho

        rho = sqrt(self%x**2 + self%y**2)

    end function

    function point2_theta(self) result(theta)
    !! Return the angle (in degrees) of the polar representation of a point
        class(point2), intent(in) :: self

        real(real64) :: theta

        theta = rad_to_deg(atan2(self%y, self%x))

    end function






    function cross_product(v1, v2) result (normal)

        real, dimension(3), intent(in) :: v1, v2 !! Vectors to cross multiply
        real, dimension(3) :: normal !! Normal vector that is the output

        normal(1) = v1(2)*v2(3) - v2(2)*v1(3)
        normal(2) = v1(3)*v2(1) - v2(3)*v1(1)
        normal(3) = v1(1)*v2(2) - v2(1)*v1(2)

    end function

    real function distance(p1, p2) result (d1)

        real, dimension(3), intent(in) :: p1, p2 !! Points to calculate distance between

        d1 = sqrt((p1(1) - p2(1))**2 + (p1(2) - p2(2))**2 + (p1(3) - p2(3))**2)

    end function


end module