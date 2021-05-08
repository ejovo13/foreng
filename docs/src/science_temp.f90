module foreng_science_temp

use foreng_env

implicit none

interface fahrenheit_to_celcius

    module procedure fahrenheit_to_celcius_r32
    module procedure fahrenheit_to_celcius_r64

end interface

interface celcius_to_fahrenheit

    module procedure celcius_to_fahrenheit_r32
    module procedure celcius_to_fahrenheit_r64

end interface

interface celcius_to_kelvin

    module procedure celcius_to_kelvin_r32
    module procedure celcius_to_kelvin_r64

end interface

interface kelvin_to_celcius

    module procedure kelvin_to_celcius_r32
    module procedure kelvin_to_celcius_r64

end interface

interface kelvin_to_fahrenheit

    module procedure kelvin_to_fahrenheit_r32
    module procedure kelvin_to_fahrenheit_r64

end interface

interface fahrenheit_to_kelvin

    module procedure fahrenheit_to_kelvin_r32
    module procedure fahrenheit_to_kelvin_r64

end interface




contains

    elemental function fahrenheit_to_celcius_r32(F) result(C)
        real(real32), intent(in) :: F
        real(real32) :: C
        C = (F - 32._real32) * (5._real32/9._real32)
    end function

    elemental function fahrenheit_to_celcius_r64(F) result(C)
        real(real64), intent(in) :: F
        real(real64) :: C
        C = (F - 32._real64) * (5._real64/9._real64)
    end function

    elemental function celcius_to_fahrenheit_r32(C) result(F)
        real(real32), intent(in) :: C
        real(real32) :: F
        F = C * (9._real32/5._real32) + 32._real32
    end function

    elemental function celcius_to_fahrenheit_r64(C) result(F)
        real(real64), intent(in) :: C
        real(real64) :: F
        F = C * (9._real64/5._real64) + 32._real64
    end function

    elemental function celcius_to_kelvin_r32(C) result(K)
        real(real32), intent(in) :: C
        real(real32) :: K
        K = C + 273._real32
    end function

    elemental function celcius_to_kelvin_r64(C) result(K)
        real(real64), intent(in) :: C
        real(real64) :: K
        K = C + 273._real64
    end function

    elemental function kelvin_to_celcius_r32(K) result(C)
        real(real32), intent(in) :: K
        real(real32) :: C
        C = K - 273._real32
    end function

    elemental function kelvin_to_celcius_r64(K) result(C)
        real(real64), intent(in) :: K
        real(real64) :: C
        C = K - 273._real32
    end function
    
    elemental function kelvin_to_fahrenheit_r32(K) result(F)
        
        real(real32), intent(in) :: K
        real(real32) :: F

        F = celcius_to_fahrenheit(kelvin_to_celcius(K))

    end function

    elemental function kelvin_to_fahrenheit_r64(K) result(F)
            
        real(real64), intent(in) :: K
        real(real64) :: F

        F = celcius_to_fahrenheit(kelvin_to_celcius(K))

    end function

    elemental function fahrenheit_to_kelvin_r32(F) result(K)
        
        real(real32), intent(in) :: F
        real(real32) :: K

        K = celcius_to_kelvin(fahrenheit_to_celcius(F))

    end function

    elemental function fahrenheit_to_kelvin_r64(F) result(K)
            
        real(real64), intent(in) :: F
        real(real64) :: K

        K = celcius_to_fahrenheit(kelvin_to_celcius(K))

    end function

end module