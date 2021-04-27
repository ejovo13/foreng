submodule (ejovo_types) point

contains

    module procedure dist
        
        real :: delta_x, delta_y
        delta_x = p2%x - p1%x
        delta_y = p2%y - p1%y

        dist = sqrt(delta_x**2 + delta_y**2)

    end procedure

    module procedure find_line

        real :: delta_x, delta_y

        if (p1%x == p2%x .and. p1%y == p2%y) then
            find_line%m = 0
            find_line%b = 0
        else

        delta_x = p2%x - p1%x
        delta_y = p2%y - p1%y      
        
        find_line%m = delta_y/delta_x
        find_line%b = p1%y - (find_line%m *p1%x)

        end if


    end procedure

end submodule