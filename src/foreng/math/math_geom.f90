!! Module for use with geometry (points, vectors, lines)

module foreng_math_geom

use foreng_env
use foreng_math_trig
implicit none

! // TODO add types : point2d, point3d, vector3d
! // TODO add point3 conversion (cartesian to spherical)

interface cross_product

    module procedure cross_product_r32
    module procedure cross_product_r64

end interface

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
    procedure :: to_array => point2_to_array
    
    
    generic :: from_pol => from_pol_r32, from_pol_r64, from_pol_int !! This is the comment for a generic function
    generic :: from_array => from_array_r32, from_array_r64, from_array_int
    generic :: operator(+) => add

    procedure :: add => point2_plus_point2
    procedure, nopass :: from_pol_r32 => point2_from_polar_r32
    procedure, nopass :: from_pol_r64 => point2_from_polar_r64
    procedure, nopass :: from_pol_int => point2_from_polar_int
    procedure, nopass :: from_array_r32 => point2_from_array_r32
    procedure, nopass :: from_array_r64 => point2_from_array_r64
    procedure, nopass :: from_array_int => point2_from_array_int

end type


type :: point3

    real(real64) :: x
    real(real64) :: y
    real(real64) :: z

contains 

    ! procedure, nopass :: from_pol => point2_from_polar_r32, point2_from_polar_r64, point2_from_polar_int
    procedure :: print => point3_print_cartesian
    procedure :: print_sph => point3_print_spherical
    procedure :: rho => point3_rho
    procedure :: theta => point3_theta
    procedure :: phi => point3_phi
    procedure :: to_array => point3_to_array

    generic :: from_sph => from_sph_r32, from_sph_r64, from_sph_int !! This is the comment for a generic function
    generic :: from_array => from_array_r32, from_array_r64, from_array_int

    ! Operators
    generic :: operator(+) => add

    ! Boung generics
    procedure :: add => point3_plus_point3
    procedure, nopass :: from_sph_r32 => point3_from_spherical_r32
    procedure, nopass :: from_sph_r64 => point3_from_spherical_r64
    procedure, nopass :: from_sph_int => point3_from_spherical_int
    procedure, nopass :: from_array_r32 => point3_from_array_r32
    procedure, nopass :: from_array_r64 => point3_from_array_r64
    procedure, nopass :: from_array_int => point3_from_array_int

end type


type :: vector3

    real(real64) :: x
    real(real64) :: y
    real(real64) :: z

contains 

    procedure :: print => vector3_print
    procedure :: length => vector3_magnitude
    procedure :: to_array => vector3_to_array

    generic :: from_array => from_array_r32, from_array_r64, from_array_int

    ! Operators

    generic :: operator(+) => add_vector3
    generic :: operator(*) => multiply_scalar
    generic :: operator(.dot.) => dot
    generic :: operator(.cross.) => cross

    ! Bound generics
    procedure, nopass :: from_array_r32 => vector3_from_array_r32
    procedure, nopass :: from_array_r64 => vector3_from_array_r64
    procedure, nopass :: from_array_int => vector3_from_array_int

    procedure :: add_vector3 => vector3_plus_vector3
    procedure :: multiply_scalar => vector3_times_scalar_r32, vector3_times_scalar_r64, vector3_times_scalar_int
    procedure :: dot => vector3_dot_vector3
    procedure :: cross => vector3_cross_vector3

end type

interface operator(*)

    module procedure scalar_times_vector3_r32
    module procedure scalar_times_vector3_r64
    module procedure scalar_times_vector3_int

end interface


contains 
!=============================================================================!
!=                             Point2 Functions                              =!
!=============================================================================!

    function point2_to_array(self) result(array)

        class(point2), intent(in) :: self
        real(real64), dimension(2) :: array

        array = [self%x, self%y]

    end function

    function point2_from_array_r32(array) result(point)

        real(real32), dimension(2) :: array

        type(point2) :: point

        point%x = array(1)
        point%y = array(2)

    end function

    function point2_from_array_r64(array) result(point)

        real(real64), dimension(2) :: array

        type(point2) :: point

        point%x = array(1)
        point%y = array(2)


    end function

    function point2_from_array_int(array) result(point)

        integer, dimension(2) :: array

        type(point2) :: point

        point%x = array(1)
        point%y = array(2)

    end function


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

        point%x = rho * cosd(real(theta, real64))
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

    function point2_plus_point2(self, p2) result(p3)

        class(point2), intent(in) :: self, p2
        type(point2) :: p3

        p3 = point2(self%x + p2%x, self%y + p2%y)

    end


!=============================================================================!
!=                             Point3 Functions                              =!
!=============================================================================!

    function point3_to_array(self) result(array)

        class(point3), intent(in) :: self
        real(real64), dimension(3) :: array

        array = [self%x, self%y, self%z]

    end function

    function point3_from_array_r32(array) result(point)

        real(real32), dimension(3) :: array

        type(point3) :: point

        point%x = array(1)
        point%y = array(2)
        point%z = array(3)

    end function

    function point3_from_array_r64(array) result(point)

        real(real64), dimension(3) :: array

        type(point3) :: point

        point%x = array(1)
        point%y = array(2)
        point%z = array(3)

    end function

    function point3_from_array_int(array) result(point)

        integer, dimension(3) :: array

        type(point3) :: point

        point%x = array(1)
        point%y = array(2)
        point%z = array(3)

    end function

    function point3_from_spherical_r32(rho, theta, phi) result(point)

        real(real32), intent(in) :: rho
        real(real32), intent(in) :: theta
        real(real32), intent(in) :: phi

        type(point3) :: point

        point%x = rho * cosd(phi) * sind(theta)
        point%y = rho * sind(phi) * sind(theta)
        point%z = rho * cosd(theta)

    end function

    function point3_from_spherical_r64(rho, theta, phi) result(point)

        real(real64), intent(in) :: rho
        real(real64), intent(in) :: theta
        real(real64), intent(in) :: phi

        type(point3) :: point

        point%x = rho * cosd(phi) * sind(theta)
        point%y = rho * sind(phi) * sind(theta)
        point%z = rho * cosd(theta)

    end function

    function point3_from_spherical_int(rho, theta, phi) result(point)

        integer, intent(in) :: rho
        integer, intent(in) :: theta
        integer, intent(in) :: phi

        type(point3) :: point

        point%x = rho * cosd(real(phi, real64)) * sind(real(theta,real64))
        point%y = rho * sind(real(phi, real64)) * sind(real(theta, real64))
        point%z = rho * cosd(real(theta, real64))

    end function

    subroutine point3_print_cartesian(self)

        class(point3), intent(in) :: self

        print 1, self%x, self%y, self%z

        1 format ("(", G11.5, "," G11.5, "," G11.5, ")")

    end subroutine

    subroutine point3_print_spherical(self)
    !! Print the polar coordinates of a point in this form
        class(point3), intent(in) :: self

        print 1, self%rho(), self%theta(), self%phi()

        1 format ("(", G11.5, "," G11.5, "," G11.5, ")")

    end subroutine

    function point3_rho(self) result(rho)
    !! Calculate the magnitude (distance from the origin) of 
        class(point3), intent(in) :: self

        real(real64) :: rho

        rho = sqrt(self%x**2 + self%y**2 + self%z**2)

    end function

    function point3_theta(self) result(theta)
    !! Return the inlination theta
        class(point3), intent(in) :: self

        real(real64) :: theta

        theta = rad_to_deg( acos( self%z / self%rho() ) )

    end function

    function point3_phi(self) result(phi)
    !! return the azimuth phi
        class(point3), intent(in) :: self

        real(real64) :: phi

        phi = rad_to_deg(atan2(self%y, self%x))
    
    end function

    function point3_plus_point3(self, p2) result(p3)

        class(point3), intent(in) :: self, p2
        type(point3) :: p3

        p3 = point3(self%x + p2%x, self%y + p2%y, self%z + p2%z)

    end function

    function point3_dot_point3(self, p2) result(dot)

        class(point3), intent(in) :: self, p2
        real(real64) :: dot

        dot = self%x*p2%x + self%y*p2%y + self%z*p2%z

    end function

    function point3_cross_point3(self, p2) result(p3)

        class(point3), intent(in) :: self, p2
        type(point3) :: p3

        p3 = p3%from_array(cross_product(self%to_array(), p2%to_array()))


    end function

!=============================================================================!
!=                             Vector3 Functions                             =!
!=============================================================================!

    subroutine vector3_print(self) 

        class(vector3), intent(in) :: self

        print 1, self%x, self%y, self%z

        1 format ("(", G11.5, "," G11.5, "," G11.5, ")")

    end subroutine

    function vector3_magnitude(self) result(length)

        class(vector3), intent(in) :: self
        real(real64) :: length

        length = sqrt(self%x**2 + self%y**2 + self%z**2)

    end function

    function vector3_to_array(self) result(array)

        class(vector3), intent(in) :: self
        real(real64), dimension(3) :: array

        array = [self%x, self%y, self%z]

    end function

    function vector3_from_array_r32(array) result(vector)

        real(real32), dimension(3), intent(in) :: array
        type(vector3) :: vector

        vector = vector3(array(1), array(2), array(3))

    end function

    function vector3_from_array_r64(array) result(vector)

        real(real64), dimension(3), intent(in) :: array
        type(vector3) :: vector

        vector = vector3(array(1), array(2), array(3))

    end function

    function vector3_from_array_int(array) result(vector)

        integer, dimension(3), intent(in) :: array
        type(vector3) :: vector

        vector = vector3(array(1), array(2), array(3))

    end function

    function vector3_plus_vector3(self, v2) result(v3) 

        class(vector3), intent(in) :: self, v2
        type(vector3) :: v3

        v3 = vector3(self%x + v2%x, self%y + v2%y, self%z + v2%z)

    end function

    function vector3_times_scalar_r32(self, k) result (v2)

        class(vector3), intent(in) :: self
        real(real32), intent(in) :: k
        type(vector3) :: v2

        v2 = vector3(self%x * k, self%y * k, self%z * k)

    end function

    function vector3_times_scalar_r64(self, k) result (v2)

        class(vector3), intent(in) :: self
        real(real64), intent(in) :: k
        type(vector3) :: v2

        v2 = vector3(self%x * k, self%y * k, self%z * k)

    end function

    function vector3_times_scalar_int(self, k) result (v2)

        class(vector3), intent(in) :: self
        integer, intent(in) :: k
        type(vector3) :: v2

        v2 = vector3(self%x * k, self%y * k, self%z * k)

    end function

    function scalar_times_vector3_r32(k, v1) result (v2)

        class(vector3), intent(in) :: v1
        real(real32), intent(in) :: k
        type(vector3) :: v2

        v2 = vector3(v1%x * k, v1%y * k, v1%z * k)

    end function

    function scalar_times_vector3_r64(k, v1) result (v2)

        class(vector3), intent(in) :: v1
        real(real64), intent(in) :: k
        type(vector3) :: v2

        v2 = vector3(v1%x * k, v1%y * k, v1%z * k)

    end function

    function scalar_times_vector3_int(k, v1) result (v2)

        class(vector3), intent(in) :: v1
        integer, intent(in) :: k
        type(vector3) :: v2

        v2 = vector3(v1%x * k, v1%y * k, v1%z * k)

    end function

    function vector3_dot_vector3(self, v2) result(dot)

        class(vector3), intent(in) :: self
        class(vector3), intent(in) :: v2

        real(real64) :: dot

        dot = self%x * v2%x + self%y * v2%y + self%z * v2%z

    end function

    function vector3_cross_vector3(self, v2) result(cross)

        class(vector3), intent(in) :: self
        class(vector3), intent(in) :: v2

        type(vector3):: cross

        cross = cross%from_array( cross_product(self%to_array(), v2%to_array()) )

    end function

!=============================================================================!
!=                              Other Functions                              =!
!=============================================================================!

    function cross_product_r32(v1, v2) result (normal)

        real, dimension(3), intent(in) :: v1, v2 !! Vectors to cross multiply
        real, dimension(3) :: normal !! Normal vector that is the output

        normal(1) = v1(2)*v2(3) - v2(2)*v1(3)
        normal(2) = v1(3)*v2(1) - v2(3)*v1(1)
        normal(3) = v1(1)*v2(2) - v2(1)*v1(2)

    end function

    function cross_product_r64(v1, v2) result (normal)

        real(real64), dimension(3), intent(in) :: v1, v2 !! Vectors to cross multiply
        real(real64), dimension(3) :: normal !! Normal vector that is the output

        normal(1) = v1(2)*v2(3) - v2(2)*v1(3)
        normal(2) = v1(3)*v2(1) - v2(3)*v1(1)
        normal(3) = v1(1)*v2(2) - v2(1)*v1(2)

    end function

    real function distance(p1, p2) result (d1)

        real, dimension(3), intent(in) :: p1, p2 !! Points to calculate distance between

        d1 = sqrt((p1(1) - p2(1))**2 + (p1(2) - p2(2))**2 + (p1(3) - p2(3))**2)

    end function


end module