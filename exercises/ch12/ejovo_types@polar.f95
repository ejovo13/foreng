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
        mult%r = p1%r * p2%r
        mult%theta = p1%theta + p2%theta

        if (mult%theta > 180.0) then
            mult%theta = mult%theta - 360.0
        else if (mult%theta < -180.0) then
            mult%theta = mult%theta + 360
        end if
    end procedure

    module procedure div
        div%r = p1%r / p2%r
        div%theta = p1%theta - p2%theta

        if (div%theta > 180.0) then
            div%theta = div%theta - 360.0
        else if (div%theta < -180.0) then
            div%theta = div%theta + 360
        end if
    end procedure

end submodule