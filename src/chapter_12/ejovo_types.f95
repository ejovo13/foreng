module ejovo_types
    
    implicit none
    
    real, parameter :: PI = 3.141592653589793
    
    type polar     
        real :: r
        real :: theta       ! The principal argument in degrees
    contains
        procedure, pass :: mult
        procedure, pass :: div    
    end type    

    type line
        real :: m
        real :: b
    contains
        procedure, pass :: print
    end type

    type point
        real :: x
        real :: y
    contains
        procedure, pass :: dist
        procedure, pass :: find_line
    end type
    
    interface ! Polar type code and functions    
    
        module function comp2pol(c) result(pol)
    
            complex, intent(in) :: c
            type (polar) :: pol  
    
        end function
    
        module function pol2comp(pol) result(c)
    
            type (polar), intent(in) :: pol
            complex :: c
    
        end function

        module pure type (polar) function mult(p1, p2)

            class(polar), intent(in) :: p1, p2

        end function
    
    
        module pure type (polar) function div(p1, p2)

            class(polar), intent(in) :: p1, p2

        end function

    end interface 

    interface ! Line
        module subroutine print(line1)

            class(line), intent(in) :: line1

        end subroutine
    end interface       
    
    interface ! Point

        ! Find the distance between two points
        module pure real function dist(p1, p2)
            class(point), intent(in) :: p1, p2
        end function

        module pure type(line) function find_line(p1, p2)
            class(point), intent(in) :: p1, p2
        end function

    end interface 
    
end module