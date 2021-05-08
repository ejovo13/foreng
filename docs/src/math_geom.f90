!! Module for use with geometry (points, vectors, lines)

module foreng_math_geom

use foreng_env
use foreng_math_trig
implicit none

! // TODO DOCUMENT

!=============================================================================!
!=                          Cross Product Interface                          =!
!=============================================================================!
interface cross_product

    module procedure cross_product_r32
    module procedure cross_product_r64

end interface

!=============================================================================!
!=                             Point2 Interface                              =!
!=============================================================================!
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
    procedure :: dist => point2_distance_between_point2    
    
    generic :: from_pol => from_pol_r32, from_pol_r64, from_pol_int !! This is the comment for a generic function
    generic :: assignment(=) => from_array_r32, from_array_r64, from_array_int
    generic :: operator(+) => add
    generic :: operator(-) => subtract_point2, unary_minus

    procedure :: add => point2_plus_point2
    procedure :: subtract_point2 => point2_minus_point2
    procedure :: unary_minus => point2_unary_minus
    procedure, nopass :: from_pol_r32 => point2_from_polar_r32
    procedure, nopass :: from_pol_r64 => point2_from_polar_r64
    procedure, nopass :: from_pol_int => point2_from_polar_int
    procedure :: from_array_r32 => point2_from_array_r32
    procedure :: from_array_r64 => point2_from_array_r64
    procedure :: from_array_int => point2_from_array_int

end type


!=============================================================================!
!=                             Point3 Interface                              =!
!=============================================================================!
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
    procedure :: dist => point3_distance_between_point3  

    generic :: from_sph => from_sph_r32, from_sph_r64, from_sph_int !! This is the comment for a generic function
    ! generic :: from_array => from_array_r32, from_array_r64, from_array_int

    ! Operators
    generic :: operator(+) => add
    generic :: assignment(=) => from_array_r32, from_array_r64, from_array_int
    generic :: operator(-) => subtract

    ! Boung generics
    procedure :: add => point3_plus_point3
    procedure :: subtract => point3_minus_point3, point3_unary_minus
    procedure, nopass :: from_sph_r32 => point3_from_spherical_r32
    procedure, nopass :: from_sph_r64 => point3_from_spherical_r64
    procedure, nopass :: from_sph_int => point3_from_spherical_int
    procedure :: from_array_r32 => point3_from_array_r32
    procedure :: from_array_r64 => point3_from_array_r64
    procedure :: from_array_int => point3_from_array_int


end type


!=============================================================================!
!=                            Vector3 Interface                              =!
!=============================================================================!
type :: vector3

    real(real64) :: x
    real(real64) :: y
    real(real64) :: z

contains 

    procedure :: print => vector3_print
    procedure :: length => vector3_magnitude
    procedure :: to_array => vector3_to_array

    ! Generics
    generic :: from_array => from_array_r32, from_array_r64, from_array_int

    ! Operators
    generic :: operator(+) => add_vector3
    generic :: operator(-) => subtract_vector3, unary_minus
    generic :: operator(*) => multiply_scalar_r32, multiply_scalar_r64, multiply_scalar_int
    generic :: operator(.dot.) => dot
    generic :: operator(.cross.) => cross
    generic :: operator(.angle.) => angle_between
    generic :: assignment(=) => from_array_r32, from_array_r64, from_array_int

    ! Bound generics
    procedure :: from_array_r32 => vector3_from_array_r32
    procedure :: from_array_r64 => vector3_from_array_r64
    procedure :: from_array_int => vector3_from_array_int
    procedure :: add_vector3 => vector3_plus_vector3
    procedure :: multiply_scalar_r32 => vector3_times_scalar_r32
    procedure :: multiply_scalar_r64 => vector3_times_scalar_r64
    procedure :: multiply_scalar_int => vector3_times_scalar_int
    procedure :: dot => vector3_dot_vector3
    procedure :: cross => vector3_cross_vector3
    procedure :: angle_between => vector3_angle_between_vector3
    procedure :: subtract_vector3 => vector3_minus_vector3
    procedure :: unary_minus => vector3_unary_minus

end type

!=============================================================================!
!=                              Line2 Interface                              =!
!=============================================================================!

type :: line2

    real(real64) :: m
    real(real64) :: b

contains

    procedure :: atx => line2_atx !! Evaluate y = mx + b for a given x
    procedure :: aty => line2_aty !! Evaluate y = mx + b for a given y
    procedure :: print => line2_print

    generic :: assignment(=) => from_two_points

    procedure :: from_two_points => line2_from_two_point2

end type




!=============================================================================!
!=                         Operator(*) Interface                             =!
!=============================================================================!
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

    subroutine point2_from_array_r32(point, array)

        real(real32), dimension(2), intent(in) :: array
        class(point2), intent(out) :: point

        point%x = array(1)
        point%y = array(2)

    end subroutine

    subroutine point2_from_array_r64(point, array) 

        real(real64), dimension(2), intent(in) :: array
        class(point2), intent(out) :: point

        point%x = array(1)
        point%y = array(2)


    end subroutine

    subroutine point2_from_array_int(point, array) 

        integer, dimension(2),intent(in) :: array
        class(point2),intent(out) :: point

        point%x = array(1)
        point%y = array(2)

    end subroutine


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

    function point2_minus_point2(self, p2) result(p3)

        class(point2), intent(in) :: self, p2
        type(point2) :: p3

        p3 = point2(self%x - p2%x, self%y - p2%y)

    end

    function point2_unary_minus(self) result(p3)

        class(point2), intent(in) :: self
        type(point2) :: p3

        p3 = point2(-self%x, -self%y)

    end

    function point2_distance_between_point2(self, p2) result(dist)

        class(point2), intent(in) :: self
        class(point2), intent(in) :: p2
        real(real64) :: dist

        dist = sqrt( (self%x - p2%x)**2 + (self%y - p2%y)**2 )

    end function


!=============================================================================!
!=                             Point3 Functions                              =!
!=============================================================================!

    function point3_to_array(self) result(array)

        class(point3), intent(in) :: self
        real(real64), dimension(3) :: array

        array = [self%x, self%y, self%z]

    end function

    subroutine point3_from_array_r32(point, array)

        real(real32), dimension(3), intent(in) :: array

        class(point3), intent(out) :: point

        point%x = array(1)
        point%y = array(2)
        point%z = array(3)

    end subroutine

    subroutine point3_from_array_r64(point, array) 

        real(real64), dimension(3), intent(in) :: array

        class(point3), intent(out) :: point

        point%x = array(1)
        point%y = array(2)
        point%z = array(3)

    end subroutine

    subroutine point3_from_array_int(point, array)

        integer, dimension(3),intent(in) :: array

        class(point3), intent(out) :: point

        point%x = array(1)
        point%y = array(2)
        point%z = array(3)

    end subroutine

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

    function point3_minus_point3(self, p2) result(p3)

        class(point3), intent(in) :: self, p2
        type(point3) :: p3

        p3 = point3(self%x - p2%x, self%y - p2%y, self%z - p2%z)

    end

    function point3_unary_minus(self) result(p3)

        class(point3), intent(in) :: self
        type(point3) :: p3

        p3 = point3(-self%x, -self%y, -self%z)

    end

    function point3_dot_point3(self, p2) result(dot)

        class(point3), intent(in) :: self, p2
        real(real64) :: dot

        dot = self%x*p2%x + self%y*p2%y + self%z*p2%z

    end function

    function point3_cross_point3(self, p2) result(p3)

        class(point3), intent(in) :: self, p2
        type(point3) :: p3

        p3 = cross_product(self%to_array(), p2%to_array())

    end function

    function point3_distance_between_point3(self, p2) result(dist)

        class(point3), intent(in) :: self
        class(point3), intent(in) :: p2
        real(real64) :: dist

        dist = sqrt( (self%x - p2%x)**2 + (self%y - p2%y)**2 + (self%z - p2%z)**2 )

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

    subroutine vector3_from_array_r32(vector, array)

        real(real32), dimension(3), intent(in) :: array
        class(vector3), intent(out) :: vector

        vector%x = array(1)
        vector%y = array(2)
        vector%z = array(3)

    end subroutine

    subroutine vector3_from_array_r64(vector, array)

        real(real64), dimension(3), intent(in) :: array
        class(vector3), intent(out) :: vector

        vector%x = array(1)
        vector%y = array(2)
        vector%z = array(3)

    end subroutine

    subroutine vector3_from_array_int(vector, array)

        integer, dimension(3), intent(in) :: array
        class(vector3), intent(out) :: vector

        vector%x = array(1)
        vector%y = array(2)
        vector%z = array(3)

    end subroutine

    function vector3_plus_vector3(self, v2) result(v3) 

        class(vector3), intent(in) :: self, v2
        type(vector3) :: v3

        v3 = vector3(self%x + v2%x, self%y + v2%y, self%z + v2%z)

    end function

    function vector3_minus_vector3(self, v2) result(v3) 

        class(vector3), intent(in) :: self, v2
        type(vector3) :: v3

        v3 = vector3(self%x - v2%x, self%y - v2%y, self%z - v2%z)

    end function

    function vector3_unary_minus(self) result(v3) 

        class(vector3), intent(in) :: self
        type(vector3) :: v3

        v3 = vector3(-self%x, -self%y, -self%z)

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

    function vector3_cross_vector3(self, v2) result(normal)

        class(vector3), intent(in) :: self
        class(vector3), intent(in) :: v2

        type(vector3):: normal

        normal = cross_product(self%to_array(), v2%to_array()) 

    end function

    function vector3_angle_between_vector3(self, v2) result(theta)
        !! Return angle between vectors in degrees
        class(vector3), intent(in) :: self
        class(vector3), intent(in) :: v2

        real(real64) :: theta

        theta = deg_to_rad( acos( (self .dot. v2) / ( self%length() * v2%length() ) ) )

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

!=============================================================================!
!=                              Line2 Functions                              =!
!=============================================================================!

    function line2_atx (self, x) result (y)
        
        class(line2), intent(in) :: self
        real(real64), intent(in) :: x
        real(real64) :: y

        y = self%m * x + self%b        
    
    end function

    function line2_aty (self, y) result (x)
        
        class(line2), intent(in) :: self
        real(real64), intent(in) :: y
        real(real64) :: x

        x = (y - self%b) / self%m      
    
    end function

    subroutine line2_from_two_point2(line, two_points)

        class(point2), dimension(2), intent(in), target :: two_points

        type(point2), pointer :: p1, p2
        class(line2), intent(out) :: line

        p1 => two_points(1)
        p2 => two_points(2)

        print *, "p1 = ", p1
        print *, "p2 = ", p2


        line%m = (p2%y - p1%y) / (p2%x - p1%x)
        line%b = p1%y - ( p1%x * line%m )

    end subroutine

    subroutine line2_print(self)

        class(line2), intent(in) :: self

        print 1, self%m, self%b

        1 format("y = ", G11.5, "x + ", G11.5)

    end subroutine

end module