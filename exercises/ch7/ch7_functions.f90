module ch7_functions
use iso_fortran_env
implicit none


contains

    recursive integer function fact(x) result (x_fact)
        integer, intent(in) :: x
        if (x == 0) then
            x_fact = 1
            return
        end if
        x_fact = x * fact(x - 1)

    end function

    recursive integer(int64) function fact_int64(x) result (x_fact)

        integer(int64), intent(in) :: x
        if (x == 0) then
            x_fact = 1
            return
        end if
        x_fact = x * fact_int64(x - 1)

    end function

    real function exp_series(x) result(exp)
    !! Calculate e^x using a taylor series with 12 terms

        real, intent(in) :: x
    
        integer, parameter :: n_terms = 12
        integer :: i

        exp = 0

        do i = 0, n_terms-1

            exp = exp + (x**i / fact(i))

        end do

    end function

    real(real64) function exp_series_r64(x) result(exp)
    !! Calculate e^x using a taylor series with 20 terms

        real(real64), intent(in) :: x
    
        integer(int64), parameter :: n_terms = 19
        integer(int64) :: i

        exp = 0

        do i = 0, n_terms-1

            exp = exp + (x**i / fact_int64(i))

        end do

    end function

end module