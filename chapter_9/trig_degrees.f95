module trig_degrees

    real(8), parameter :: PI = 3.141592653589793
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
    
    interface deg2rad

    module elemental integer function deg2rad_i(theta)
        integer, intent(in) :: theta
    end function 
    module elemental real function deg2rad_r(theta)
        real, intent(in) :: theta
    end function 
    module elemental real(8) function deg2rad_d(theta)
        real(8), intent(in) :: theta
    end function 

    end interface

    interface rad2deg

    module elemental integer function rad2deg_i(theta)
        integer, intent(in) :: theta
    end function 
    module elemental real function rad2deg_r(theta)
        real, intent(in) :: theta
    end function 
    module elemental real(8) function rad2deg_d(theta)
        real(8), intent(in) :: theta
    end function 

    end interface

end module