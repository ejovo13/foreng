module foreng_math_misc
!! Miscellaneous Mathematic functions

use foreng_env
use foreng_math_trig

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
!! Compute \(\sqrt[n]{x}\)
    module procedure nth_root_r32
    module procedure nth_root_r64
end interface

!=============================================================================!
!=                     Infinite Series Interface                             =!
!=============================================================================!
interface exp_series
!! Compute \(e^x\) using a truncated taylor series
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
    !! Recursively compute the factorial of a 16-bit integer. The max value that can be passed is 7
    !! If x is less than 0 or greater 7, the function will return 0
        integer(int16), intent(in) :: x !! \( 0 \leq x \leq 7\)
        integer(int16) :: x_fact !! \(x!\)

        if ( x >= 8) then 
            x_fact = 0
            return 
        else if ( x < 0) then
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
    !! Recursively compute the factorial of a 32-bit integer. The max value that can be passed is 16
    !! If x is less than 0 or greater 16, the function will return 0
        integer(int32), intent(in) :: x !! \( 0 \leq x \leq 16 \)
        integer(int32) :: x_fact !! \(x!\)

        if ( x >= 17) then 
            x_fact = 0
            return 
        else if ( x < 0) then
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
    !! Recursively compute the factorial of a 64-bit integer. The max value that can be passed is 20
    !! If x is less than 0 or greater 20, the function will return 0
        integer(int64), intent(in) :: x !! \( 0 \leq x \leq 20 \)
        integer(int64) :: x_fact !! \(x!\)

        if ( x >= 21) then 
            x_fact = 0
            return 
        else if ( x < 0) then
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
    !! Recursively compute the factorial of a 128-bit integer [supported on gfortran]. The max value that can be passed is 33
    !! If x is less than 0 or greater 33, the function will return 0
        integer(int128), intent(in) :: x !! \( 0 \leq x \leq 33 \)
        integer(int128) :: x_fact !! \(x!\)

        if ( x >= 34) then 
            x_fact = 0
            return 
        else if ( x < 0) then
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
    !! Compute \(e^x\) using a taylor series with 12 terms

        real(real32), intent(in) :: x

        integer, parameter :: n_terms = 12
        integer :: i

        exp = 0

        do i = 0, n_terms-1

            exp = exp + (x**i / factorial(i))

        end do

    end function

    real(real64) function exp_series_r64(x) result(exp)
    !! Calculate \(e^x\) using a taylor series with 20 terms

        real(real64), intent(in) :: x

        integer(int64), parameter :: n_terms = 19
        integer(int64) :: i

        exp = 0

        do i = 0, n_terms-1

            exp = exp + (x**i / factorial(i))

        end do

    end function

    real function sind_series(x_, n_) result(sin_x)
    !! Compute sine using a truncated taylor series
        real, intent(in) :: x_ !! Angle in degrees
        integer, intent(in) :: n_ !! Number of terms to use
        integer :: i_
        real :: x_rad

        x_rad = deg_to_rad(x_)

        sin_x = 0

        do i_ = 1, n_

            sin_x = sin_x + ( (-1)**(i_ - 1) )*( (x_rad ** (2 * i_ - 1)) / (factorial((2 * i_) - 1)))        

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

!=============================================================================!
!=                           Fibonacci Functions                             =!
!=============================================================================!

    integer(int64) function fibonacci_loop(n) result(f_n)

        integer(int64), intent(in) :: n!! Nth fibonacci to compute
        
        integer(int64) :: f_n_1, f_n_2 !! Previous two fibonacci numbers
        integer(int64) :: i !! Looping index

        if (n == 1 .or. n == 2) then
            f_n = 1
            return
        else if (n > 2) then 

            f_n_1 = 1
            f_n_2 = 1

            do i = 3, n

                f_n = f_n_1 + f_n_2 !! Update current F number
                f_n_2 = f_n_1 
                f_n_1 = f_n !! Update previous two numbers

            end do            

        else 
            print *, "N must be a natural number"
        end if      

    end function

end module  