!! Module to contain statistics routines like finding the mean, std dev

module foreng_math_stats

use foreng_env
use foreng_math_misc
implicit none

!=============================================================================!
!=                          Generic Mean Interface                           =!
!=============================================================================!
interface mean

    module procedure rank_1_mean_r32
    module procedure rank_1_mean_r64
    module procedure rank_1_mean_i32
    module procedure rank_1_mean_i64    

end interface

!=============================================================================!
!=                        Arithmetic Mean Interface                          =!
!=============================================================================!
interface arithmetic_mean

    module procedure rank1_arithmetic_mean_r32
    module procedure rank1_arithmetic_mean_r64
    module procedure rank1_arithmetic_mean_i32
    module procedure rank1_arithmetic_mean_i64

end interface

!=============================================================================!
!=                   Root-mean-squared Mean Interface                        =!
!=============================================================================!
interface rms_mean

    module procedure rank1_root_mean_squared_mean_r32
    module procedure rank1_root_mean_squared_mean_r64
    module procedure rank1_root_mean_squared_mean_i32
    module procedure rank1_root_mean_squared_mean_i64

end interface

!=============================================================================!
!=                          Harmonic Mean Interface                          =!
!=============================================================================!
interface harmonic_mean

    module procedure rank1_harmonic_mean_r32
    module procedure rank1_harmonic_mean_r64
    module procedure rank1_harmonic_mean_i32
    module procedure rank1_harmonic_mean_i64

end interface

!=============================================================================!
!=                         Geometric Mean Interface                          =!
!=============================================================================!
interface geometric_mean

    module procedure rank1_geometric_mean_r32
    module procedure rank1_geometric_mean_r64
    module procedure rank1_geometric_mean_i32
    module procedure rank1_geometric_mean_i64

end interface

!=============================================================================!
!=                      Standard Deviation Interface                         =!
!=============================================================================!
interface std_dev

    module procedure std_dev_r32
    module procedure std_dev_r64
    module procedure std_dev_i32
    module procedure std_dev_i64

end interface 

!=============================================================================!
!=                          Generic Mean Functions                           =!
!=============================================================================!
contains

    function rank_1_mean_r32(data_, type_) result(mean_)
    !! Calculate the mean of a 1d data set, given the "type"

        character(LEN=*), intent(in), optional :: type_
        real(real32), intent(in), dimension(:) :: data_
        real(real32) :: mean_

        if (present(type_)) then 
            !! Check which mean to call
            select case(type_)

                case("arithmetic")

                    mean_ = arithmetic_mean(data_)

                case("geometric")

                    mean_ = geometric_mean(data_)

                case("rms")

                    mean_ = rms_mean(data_)

                case("harmonic")

                    mean_ = harmonic_mean(data_)

                case default 

                    error stop "No recognized mean type selected"
                    
            end select

        else 
            mean_ = arithmetic_mean(data_)
        end if       

    end function

    function rank_1_mean_r64(data_, type_) result(mean_)
    !! Calculate the mean of a 1d data set, given the "type"

        character(LEN=*), intent(in), optional :: type_
        real(real64), intent(in), dimension(:) :: data_
        real(real64) :: mean_

        if (present(type_)) then 
            !! Check which mean to call
            select case(type_)

                case("arithmetic")

                    mean_ = arithmetic_mean(data_)

                case("geometric")

                    mean_ = geometric_mean(data_)

                case("rms")

                    mean_ = rms_mean(data_)

                case("harmonic")

                    mean_ = harmonic_mean(data_)

                case default 

                    error stop "No recognized mean type selected"
                    
            end select

        else 
            mean_ = arithmetic_mean(data_)
        end if        

    end function

    function rank_1_mean_i32(data_, type_) result(mean_)
    !! Calculate the mean of a 1d data set, given the "type"

        character(LEN=*), intent(in), optional :: type_
        integer(int32), intent(in), dimension(:) :: data_
        real(real32) :: mean_

        if (present(type_)) then 
            !! Check which mean to call
            select case(type_)

                case("arithmetic")

                    mean_ = arithmetic_mean(data_)

                case("geometric")

                    mean_ = geometric_mean(data_)

                case("rms")

                    mean_ = rms_mean(data_)

                case("harmonic")

                    mean_ = harmonic_mean(data_)

                case default 

                    error stop "No recognized mean type selected"
                    
            end select

        else 
            mean_ = arithmetic_mean(data_)
        end if        

    end function

    function rank_1_mean_i64(data_, type_) result(mean_)
    !! Calculate the mean of a 1d data set, given the "type"

        character(LEN=*), intent(in), optional :: type_
        integer(int64), intent(in), dimension(:) :: data_
        real(real64) :: mean_

        if (present(type_)) then 
            !! Check which mean to call
            select case(type_)

                case("arithmetic")

                    mean_ = arithmetic_mean(data_)

                case("geometric")

                    mean_ = geometric_mean(data_)

                case("rms")

                    mean_ = rms_mean(data_)

                case("harmonic")

                    mean_ = harmonic_mean(data_)

                case default 

                    error stop "No recognized mean type selected"
                    
            end select

        else 

            mean_ = arithmetic_mean(data_)

        end if
        

    end function

!=============================================================================!
!=                        Arithmetic Mean Functions                          =!
!=============================================================================!

    function rank1_arithmetic_mean_r32(data_) result(arithmetic_mean_)
    !! Calculate the arithmetic mean of a rank 1 array

        real(real32), intent(in) :: data_(:)
        real(real32) :: arithmetic_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        arithmetic_mean_ = sum(data_)/n_elements

    end function

    function rank1_arithmetic_mean_r64(data_) result(arithmetic_mean_)
    !! Calculate the arithmetic mean of a rank 1 array

        real(real64), intent(in) :: data_(:)
        real(real64) :: arithmetic_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        arithmetic_mean_ = sum(data_)/n_elements

    end function

    function rank1_arithmetic_mean_i32(data_) result(arithmetic_mean_)
    !! Calculate the arithmetic mean of a rank 1 array

        integer(int32), intent(in) :: data_(:) !! Rank 1 array containing the elements to calculate the mean for
        real(real32) :: arithmetic_mean_
        
        integer :: n_elements

        n_elements = size(data_)

        arithmetic_mean_ = sum(data_)/n_elements

    end function

    function rank1_arithmetic_mean_i64(data_) result(arithmetic_mean_)
    !! Calculate the arithmetic mean of a rank 1 array

        integer(int64), intent(in) :: data_(:)
        real(real64) :: arithmetic_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        arithmetic_mean_ = sum(data_)/n_elements

    end function

!=============================================================================!
!=                   Root-mean-squared Mean Functions                        =!
!=============================================================================!
    function rank1_root_mean_squared_mean_r32(data_) result(rms_)

        real(real32), intent(in) :: data_(:)
        real(real32) :: rms_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        rms_ = sqrt(sum(data_*data_)/n_elements)

    end function

    function rank1_root_mean_squared_mean_r64(data_) result(rms_)

        real(real64), intent(in) :: data_(:)
        real(real64) :: rms_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        rms_ = sqrt(sum(data_*data_)/n_elements)

    end function

    function rank1_root_mean_squared_mean_i32(data_) result(rms_)

        integer(int32), intent(in) :: data_(:)
        real(real32) :: rms_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        rms_ = sqrt(sum(real(data_*data_))/n_elements)

    end function

    function rank1_root_mean_squared_mean_i64(data_) result(rms_)

        integer(int64), intent(in) :: data_(:)
        real(real64) :: rms_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        rms_ = sqrt(sum(real(data_*data_, real64))/n_elements)

    end function

!=============================================================================!
!=                          Harmonic Mean Functions                          =!
!=============================================================================!
    function rank1_harmonic_mean_r32(data_) result(harmonic_mean_)

        real(real32), intent(in) :: data_(:)
        real(real32) :: harmonic_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        real(real32), allocatable :: inverses(:)

        n_elements = size(data_)
        inverses = 1/data_

        harmonic_mean_ = n_elements/sum(inverses)

    end function

    function rank1_harmonic_mean_r64(data_) result(harmonic_mean_)

        real(real64), intent(in) :: data_(:)
        real(real64) :: harmonic_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        real(real64), allocatable :: inverses(:)

        n_elements = size(data_)
        inverses = 1_real64/data_

        harmonic_mean_ = n_elements/sum(inverses)

    end function

    function rank1_harmonic_mean_i32(data_) result(harmonic_mean_)

        integer(int32), intent(in) :: data_(:)
        real(real32) :: harmonic_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        real(real32), allocatable :: inverses(:)

        n_elements = size(data_)
        inverses = 1/data_

        harmonic_mean_ = n_elements/sum(inverses)

    end function

    function rank1_harmonic_mean_i64(data_) result(harmonic_mean_)

        integer(int64), intent(in) :: data_(:)
        real(real64) :: harmonic_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        real(real64), allocatable :: inverses(:)

        n_elements = size(data_)
        inverses = 1_real64/data_

        harmonic_mean_ = n_elements/sum(inverses)

    end function

!=============================================================================!
!=                         Geometric Mean Functions                          =!
!=============================================================================!
    function rank1_geometric_mean_r32(data_) result(geometric_mean_)

        real(real32), intent(in) :: data_(:)
        real(real32) :: geometric_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        geometric_mean_ = nth_root(product(data_), n_elements)

    end function

    function rank1_geometric_mean_r64(data_) result(geometric_mean_)

        real(real64), intent(in) :: data_(:)
        real(real64) :: geometric_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        geometric_mean_ = nth_root(product(data_), n_elements)

    end function

    function rank1_geometric_mean_i32(data_) result(geometric_mean_)

        integer(int32), intent(in) :: data_(:)
        real(real32) :: geometric_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        geometric_mean_ = nth_root(real(product(data_), real32), n_elements)

    end function

    function rank1_geometric_mean_i64(data_) result(geometric_mean_)

        integer(int64), intent(in) :: data_(:)
        real(real64) :: geometric_mean_
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        geometric_mean_ = nth_root(real(product(data_), real64), n_elements)

    end function

!=============================================================================!
!=                      Standard Deviation Functions                         =!
!=============================================================================!

    function std_dev_r32(data) result(sigma)

        real(real32), dimension(:), intent(in) :: data
        real(real32) :: sigma !! population standard deviation
        real(real32) :: mu !! Average

        integer :: n_elements

        n_elements = size(data)
        mu = mean(data)

        sigma = sqrt( (sum( (data - mu )**2) ) / n_elements )

    end function

    function std_dev_r64(data) result(sigma)

        real(real64), dimension(:), intent(in) :: data
        real(real64) :: sigma !! population standard deviation
        real(real64) :: mu !! Average

        integer :: n_elements

        n_elements = size(data)
        mu = mean(data)

        sigma = sqrt( (sum( (data - mu )**2) ) / n_elements )

    end function

    function std_dev_i32(data_) result(sigma)

        integer(int32), dimension(:), intent(in) :: data_
        real(real32) :: sigma !! population standard deviation
        real(real32) :: mu !! Average

        integer :: n_elements

        n_elements = size(data_)
        mu = mean(data_)

        sigma = sqrt( (sum( (data_ - mu)**2 ) ) / n_elements )

    end function

    function std_dev_i64(data) result(sigma)

        integer(int64), dimension(:), intent(in) :: data
        real(real64) :: sigma !! population standard deviation
        real(real64) :: mu !! Average

        integer :: n_elements

        n_elements = size(data)
        mu = mean(data)

        sigma = sqrt( (sum( (data - mu )**2) ) / n_elements )

    end function


end module