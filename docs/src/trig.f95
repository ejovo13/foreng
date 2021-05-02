module trig

    real(8), parameter :: PI_D = 3.141592653589793
    real, parameter :: PI = 3.14159265
    integer, parameter :: SGL = selected_int_kind(7)
    integer, parameter :: DBL = selected_int_kind(15)

    interface deg_to_rad    
        module procedure deg_to_rad_sgl
        module procedure deg_to_rad_dbl
        module procedure deg_to_rad_int
    end interface

    interface rad_to_deg
        module procedure rad_to_deg_sgl
        module procedure rad_to_deg_dbl
        module procedure rad_to_deg_int
    end interface

    interface
    
    module elemental real function sind(theta)
        real, intent(in) :: theta
    end function

    module elemental real function cosd(theta)
        real, intent(in) :: theta
    end function

    module elemental real function tand(theta)
        real, intent(in) :: theta
    end function

    module elemental real function asind(theta)
        real, intent(in) :: theta
    end function
    
    module elemental real function acosd(theta)
        real, intent(in) :: theta
    end function 

    module elemental real function atand(theta)
        real, intent(in) :: theta
    end function

    end interface  

    contains 
        elemental real(SGL) function deg_to_rad_sgl(degrees) result(rad)
            real(SGL), intent(in) :: degrees
            rad = degrees/180.0 * PI
        end function

        elemental real(DBL) function deg_to_rad_dbl(degrees) result(rad)
            real(DBL), intent(in) :: degrees
            rad = degrees/180.0 * PI_D
        end function

        elemental real(SGL) function deg_to_rad_int(degrees) result(rad)
            integer, intent(in) :: degrees
            rad = degrees/180.0 * PI
        end function

        elemental real(SGL) function rad_to_deg_sgl(rad) result(degrees)
            real(SGL), intent(in) :: rad
            degrees = rad/PI * 180.0
        end function

        elemental real(DBL) function rad_to_deg_dbl(rad) result(degrees)
            real(DBL), intent(in) :: rad
            degrees = rad/PI_D * 180.0
        end function

        elemental real(SGL) function rad_to_deg_int(rad) result(degrees)
            integer, intent(in) :: rad
            degrees = rad/PI * 180.0
        end function

end module 