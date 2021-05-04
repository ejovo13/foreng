module ch4_means 
    
    use iso_fortran_env
    
    implicit none

interface mean

    module procedure rank_1_mean_r32
    ! module procedure rank_2 mean

end interface


interface arithmetic

    module procedure rank1_arithmetic_mean_r32

end interface

interface rms

    module procedure rank1_root_mean_squared_mean_r32

end interface

interface harmonic

    module procedure rank1_harmonic_mean_r32

end interface

interface geometric

    module procedure rank1_geometric_mean_r32

end interface

contains

    real(real32) function rank_1_mean_r32(data_, type_) result(mean_)
    !! Calculate the mean of a 1d data set, given the "type"

        character(LEN=*), intent(in), optional :: type_
        real(real32), intent(in), dimension(:) :: data_

        if (present(type_)) then 
            !! Check which mean to call
            select case(type_)

                case("arithmetic")

                    mean_ = arithmetic(data_)

                case("geometric")

                    mean_ = geometric(data_)

                case("rms")

                    mean_ = rms(data_)

                case("harmonic")

                    mean_ = harmonic(data_)

                case default 

                    error stop "No recognized mean type selected"
                    
            end select

        else 

            mean_ = arithmetic(data_)

        end if
        

    end function







    real(real32) function rank1_arithmetic_mean_r32(data_) result(arithemetic_mean)
    !! Calculate the arithmetic mean of a rank 1 array

        real(real32), intent(in) :: data_(:)
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        arithemetic_mean = sum(data_)/n_elements

    end function

    ! // TODO add generic support to mean

    real(real32) function rank1_geometric_mean_r32(data_) result(geometric_mean)

        real(real32), intent(in) :: data_(:)
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        geometric_mean = nth_root(product(data_), n_elements)

    end function

    real(real32) function rank1_root_mean_squared_mean_r32(data_) result(rms)

        real(real32), intent(in) :: data_(:)
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        n_elements = size(data_)

        rms = sqrt(sum(data_*data_)/n_elements)

    end function

    real(real32) function rank1_harmonic_mean_r32(data_) result(harmonic_mean)

        real(real32), intent(in) :: data_(:)
        !! Rank 1 array containing the elements to calculate the mean for
        integer :: n_elements

        real(real32), allocatable :: inverses(:)

        n_elements = size(data_)
        inverses = 1/data_

        harmonic_mean = n_elements/sum(inverses)

    end function




    !=======================================================================!
    !=                      Helper Functions                               =!
    !=======================================================================!

    real(real32) function nth_root(x_, n_) result(root_)

        real(real32), intent(in) :: x_
        integer, intent(in) :: n_
        real(real32), parameter :: BASE = 10

        root_ = BASE**((1.0/n_)*log10(x_))

    end function
    ! // TODO add generic support to nth_root















end module