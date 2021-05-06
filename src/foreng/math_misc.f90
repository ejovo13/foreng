module foreng_math_misc

use foreng_env

!=============================================================================!
!=                           Factorial Interface                             =!
!=============================================================================!
interface factorial
!! Compute \(x!\)
    module procedure factorial_int16
    module procedure factorial_int32
    module procedure factorial_int64
    module procedure factorial_int128
end interface

!=============================================================================!
!=                            nth Root Interface                             =!
!=============================================================================!
interface nth_root
!! Compute \(x!\)
    module procedure nth_root_r32
    module procedure nth_root_r64
end interface

!=============================================================================!
!=                     Infinite Series Interface                             =!
!=============================================================================!
interface exp_series
!! Compute \(x!\)
    module procedure exp_series_r32
    module procedure exp_series_r64
end interface

! //TODO Add sin series functions
! // TODO add comments
!=============================================================================!
!=                           Factorial Functions                             =!
!=============================================================================!
contains

    recursive function factorial_int16(x) result (x_fact)
        
        integer(int16), intent(in) :: x
        integer(int16) :: x_fact !! \(x!\)

        if ( x >= 8) then 
            x_fact = 0
            return 
        end if


        if (x == 0) then
            x_fact = 1_int16
            return
        end if
        x_fact = x * factorial(x - 1_int16)

    end function

    recursive function factorial_int32(x) result (x_fact)

        integer(int32), intent(in) :: x
        integer(int32) :: x_fact !! \(x!\)

        if ( x >= 17) then 
            x_fact = 0
            return 
        end if

        if (x == 0) then
            x_fact = 1_int32
            return
        end if
        x_fact = x * factorial(x - 1_int32)

    end function

    recursive function factorial_int64(x) result (x_fact)

        integer(int64), intent(in) :: x
        integer(int64) :: x_fact !! \(x!\)

        if ( x >= 21) then 
            x_fact = 0
            return 
        end if

        if (x == 0) then
            x_fact = 1_int64
            return
        end if
        x_fact = x * factorial(x - 1_int64)

    end function

    recursive function factorial_int128(x) result (x_fact)
    !! Supported for gnufortran compiler with kind = 16
    integer(int128), intent(in) :: x
    integer(int128) :: x_fact !! \(x!\)

    if ( x >= 34) then 
        x_fact = 0
        return 
    end if

    if (x == 0) then
        x_fact = 1_int128
        return
    end if
    x_fact = x * factorial(x - 1_int128)

end function

!=============================================================================!
!=                     Infinite Series Functions                             =!
!=============================================================================!

    real function exp_series_r32(x) result(exp)
    !! Calculate e^x using a taylor series with 12 terms

        real(real32), intent(in) :: x

        integer, parameter :: n_terms = 12
        integer :: i

        exp = 0

        do i = 0, n_terms-1

            exp = exp + (x**i / factorial(i))

        end do

    end function

    real(real64) function exp_series_r64(x) result(exp)
    !! Calculate e^x using a taylor series with 20 terms

        real(real64), intent(in) :: x

        integer(int64), parameter :: n_terms = 19
        integer(int64) :: i

        exp = 0

        do i = 0, n_terms-1

            exp = exp + (x**i / factorial(i))

        end do

    end function

!=============================================================================!
!=                            nth Root Functions                             =!
!=============================================================================!

    function nth_root_r32(x_, n_) result(root_)

        real(real32), intent(in) :: x_
        real(real32) :: root_

        integer, intent(in) :: n_
        real(real32), parameter :: BASE = 10

        root_ = BASE**((1.0/n_)*log10(x_))

    end function

    function nth_root_r64(x_, n_) result(root_)

        real(real64), intent(in) :: x_
        real(real64) :: root_

        integer, intent(in) :: n_
        real(real64), parameter :: BASE = 10

        root_ = BASE**((1.0/n_)*log10(x_))

    end function

end module  