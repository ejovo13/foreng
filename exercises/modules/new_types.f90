module new_types
    implicit none
    
    type vector
        real :: x
        real :: y
        character(10) :: name = "default"
    end type    
    
end module