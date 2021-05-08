module foreng_numeric_calculus

use foreng_env

contains 

    function calc_derivative(func, x_0, dx) result (dydx)
        
        real(real64), external :: func  !! Ptr to an external function
        real(real64), intent(in) :: x_0 !! Value at which to calculate the derivative
        real(real64), optional :: dx    !! Optional step size. If not present, defaults to \(10^{-100}\) 
        real(real64) :: dydx            !! \(\frac{dy}{dx}\) at \(x = x_0\)

        real(real64) :: f_x0, f_x1
        real(real64) :: dx_

        if(.not. present(dx)) then
            dx_ = 1D-100
        else
            dx_ = dx
        end if

        f_x0 = func(x_0)
        f_x1 = func(x_0 + dx_)
        dydx = (f_x1 - f_x0)/dx_

    end function

end module