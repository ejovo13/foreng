submodule (ejovo_types) polar

contains

    module procedure comp2pol
        
        real :: magnitude, theta, x, y

        x = realpart(c)
        y = imagpart(c)

        magnitude = sqrt(realpart(c)**2 + imagpart(c)**2)
        theta = atan2(y,x)
        theta = theta * (180.0/PI)

        pol%r = magnitude
        pol%theta = theta

    end procedure

    module procedure pol2comp

        real :: theta_rads, x, y
        
        theta_rads = (pol%theta/180)*PI

        x = pol%r * cos(theta_rads)
        y = pol%r * sin(theta_rads)

        c = complex(x,y) 

    end procedure

    module procedure mult
        p3%r = p1%r * p2%r
        p3%theta = p1%theta + p2%theta

        if (p3%theta > 180.0) then
            p3%theta = p3%theta - 360.0
        else if (p3%theta < -180.0) then
            p3%theta = p3%theta + 360
        end if
    end procedure

    module procedure div
        p3%r = p1%r / p2%r
        p3%theta = p1%theta - p2%theta

        if (p3%theta > 180.0) then
            p3%theta = p3%theta - 360.0
        else if (p3%theta < -180.0) then
            p3%theta = p3%theta + 360
        end if
    end procedure

end submodule