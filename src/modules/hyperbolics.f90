module hyperbolics
    implicit none
    
    contains 
    
        function my_sinh(x) result(sinhx)
            real, intent(in) :: x
            real :: sinhx
    
            sinhx = (exp(x) - exp(-x))/2
            
        end function
    
        function my_cosh(x) result(coshx)
            real, intent(in) :: x
            real :: coshx
    
            coshx = (exp(x) + exp(-x))/2
        end function
    
        function my_tanh(x) result(tanhx)
            real, intent(in) :: x
            real :: tanhx
    
            tanhx = my_sinh(x)/my_cosh(x)
    
        end function
    
    
    end module hyperbolics