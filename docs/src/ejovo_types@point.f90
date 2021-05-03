submodule (ejovo_types) point

contains

    module procedure distance_between_points
        
        real :: delta_x, delta_y
        delta_x = p2%x - p1%x
        delta_y = p2%y - p1%y

        dist = sqrt(delta_x**2 + delta_y**2)

    end procedure

    module procedure find_line

        real :: delta_x, delta_y

        if (p1%x == p2%x .and. p1%y == p2%y) then
            l%m = 0
            l%b = 0
        else

        delta_x = p2%x - p1%x
        delta_y = p2%y - p1%y      
        
        l%m = delta_y/delta_x
        l%b = p1%y - (l%m *p1%x)

        end if

    end procedure

end submodule