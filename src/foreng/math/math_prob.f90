module foreng_math_prob
!! Contains functions used to compute probability distributions

use foreng_env
use foreng_math_misc
implicit none

!=============================================================================!
!=                      Random Distribution Interface                        =!
!=============================================================================!
interface urand
!! Return a uniform random variable from min (inclusive) to max (inclusive for integer functions, exclusive for real)
    module procedure urand_r32
    module procedure urand_r64
    module procedure urand_i32
    module procedure urand_i64

end interface 

interface fill_urand
!! Fill a rank 1, 2, or 3 array with a uniform random variable from min (inclusive) to max (inclusive for integer functions, exclusive for real)
!!
!! Example usage:
!!```fortran
!! 
!!integer, dimension(64) :: x_i32
!!real(real32), dimension(8, 8) :: x_r32
!!real(real64), dimension(4, 4, 4) :: x_64
!!
!!call fill_urand(x_i32, 0, 10) ! Fill rank1, integer array
!!call fill_urand(x_r32, -1.0, 1.0) ! Fill rank2, real(real32) array
!!call fill_urand(x_r64, -10.d0, 10.d0) ! Fill rank3, real(real64) array
!!```
    module procedure fill_urand_rank1_r32
    module procedure fill_urand_rank1_r64
    module procedure fill_urand_rank1_i32
    module procedure fill_urand_rank1_i64

    module procedure fill_urand_rank2_r32
    module procedure fill_urand_rank2_r64
    module procedure fill_urand_rank2_i32
    module procedure fill_urand_rank2_i64

    module procedure fill_urand_rank3_r32
    module procedure fill_urand_rank3_r64
    module procedure fill_urand_rank3_i32
    module procedure fill_urand_rank3_i64

end interface

! // TODO Add Normal distribution functions
! // TODO add documentation

!=============================================================================!
!=                      Random Distribution Functions                        =!
!=============================================================================!
contains

    function urand_r32(min,max) result(x)
    !! Return a uniform random variable [min, max)
        real(real32), intent(in) :: min !! \(a\), inclusive
        real(real32), intent(in) :: max !! \(b\), exclusive
        real(real32) :: x !! \(x \sim \mathcal{U}[a, b)\)

        real(real32) :: temp_rand ! A value between 0 and 1
        real(real32), parameter :: rand_mid = 0.5
        real(real32) :: scale ! What we should multiply the range by
        real(real32) :: shift ! What we should shift the dist by

        ! mid_point = (min + max) / 2
        shift = min
        scale = max - min

        call random_number(temp_rand)
        ! print *, temp_rand

        ! Shift the midpoint to the midpoint of the new range
        x = temp_rand*scale + shift   

    end function

    function urand_r64(min,max) result(x)
    !! Return a uniform random variable [min, max)
        real(real64), intent(in) :: min !! \(a\), inclusive
        real(real64), intent(in) :: max !! \(b\), exclusive
        real(real64) :: x !! \(x \sim \mathcal{U}[a, b)\)

        real(real64) :: temp_rand ! A value between 0 and 1
        real(real64), parameter :: rand_mid = 0.5
        real(real64) :: scale ! What we should multiply the range by
        real(real64) :: shift ! What we should shift the dist by

        ! mid_point = (min + max) / 2
        shift = min
        scale = max - min

        call random_number(temp_rand)
        ! print *, temp_rand

        ! Shift the midpoint to the midpoint of the new range
        x = temp_rand*scale + shift   

    end function

    function urand_i32(min, max) result(x)
    !! Return a uniform random variable [min, max] 
        integer(int32), intent(in) :: min !! \(a\), inclusive
        integer(int32), intent(in) :: max !! \(b\), inclusive
        integer(int32) :: x !! \(x \sim \mathcal{U}[a, b]\)

        real(real32) :: temp_rand ! A value between 0 and 1
        real(real32), parameter :: rand_mid = 0.5
        integer(int32) :: scale ! What we should multiply the range by
        integer(int32) :: shift ! What we should shift the dist by

        ! mid_point = (min + max) / 2
        shift = min
        scale = max - (min - 1)

        call random_number(temp_rand)
        ! print *, temp_rand

        ! Shift the midpoint to the midpoint of the new range
        x = int(temp_rand*scale + shift, int32)   

    end function

    function urand_i64(min, max) result(x)
    !! Return a uniform random variable [min, max] 
        integer(int64), intent(in) :: min !! \(a\), inclusive
        integer(int64), intent(in) :: max !! \(b\), inclusive
        integer(int64) :: x !! \(x \sim \mathcal{U}[a, b]\)

        real(real64) :: temp_rand ! A value between 0 and 1
        real(real64), parameter :: rand_mid = 0.5
        integer(int64) :: scale ! What we should multiply the range by
        integer(int64) :: shift ! What we should shift the dist by

        ! mid_point = (min + max) / 2
        shift = min
        scale = max - min

        call random_number(temp_rand)
        ! print *, temp_rand

        ! Shift the midpoint to the midpoint of the new range
        x = int(temp_rand*scale + shift, int64)   

    end function

!=============================================================================!
!=                            Rank1 Fill Functions                           =!
!=============================================================================!
    subroutine fill_urand_rank1_r32(A, min, max)

        real(real32), dimension(:), intent(inout) :: A
        real(real32), intent(in) :: min, max

        integer :: i
        integer :: ni

        ni = size(A)

        do i = 1,ni

            A(i) = urand(min, max)

        end do

    end subroutine

    subroutine fill_urand_rank1_r64(A, min, max)

        real(real64), dimension(:), intent(inout) :: A
        real(real64), intent(in) :: min, max

        integer :: i
        integer :: ni

        ni = size(A)

        do i = 1,ni

            A(i) = urand(min, max)

        end do

    end subroutine

    subroutine fill_urand_rank1_i32(A, min, max)

        integer(int32), dimension(:), intent(inout) :: A
        integer(int32), intent(in) :: min, max

        integer :: i
        integer :: ni

        ni = size(A)

        do i = 1,ni

            A(i) = urand(min, max)

        end do

    end subroutine

    subroutine fill_urand_rank1_i64(A, min, max)

        integer(int64), dimension(:), intent(inout) :: A
        integer(int64), intent(in) :: min, max

        integer :: i
        integer :: ni

        ni = size(A)

        do i = 1,ni

            A(i) = urand(min, max)

        end do

    end subroutine

!=============================================================================!
!=                            Rank2 Fill Functions                           =!
!=============================================================================!
    subroutine fill_urand_rank2_r32(A, min, max)

        real(real32), dimension(:,:), intent(inout) :: A
        real(real32), intent(in) :: min, max

        integer :: i, j
        integer :: ni, nj

        ni = size(A, DIM=1)
        nj = size(A, DIM=2)

        do i = 1,ni
            do j = 1,nj
                A(i,j) = urand(min, max)
            end do
        end do

    end subroutine

    subroutine fill_urand_rank2_r64(A, min, max)

        real(real64), dimension(:,:), intent(inout) :: A
        real(real64), intent(in) :: min, max

        integer :: i, j
        integer :: ni, nj

        ni = size(A, DIM=1)
        nj = size(A, DIM=2)

        do i = 1,ni
            do j = 1,nj
                A(i,j) = urand(min, max)
            end do
        end do

    end subroutine

    subroutine fill_urand_rank2_i32(A, min, max)

        integer(int32), dimension(:,:), intent(inout) :: A
        integer(int32), intent(in) :: min, max

        integer :: i, j
        integer :: ni, nj

        ni = size(A, DIM=1)
        nj = size(A, DIM=2)

        do i = 1,ni
            do j = 1,nj
                A(i,j) = urand(min, max)
            end do
        end do

    end subroutine

    subroutine fill_urand_rank2_i64(A, min, max)

        integer(int64), dimension(:,:), intent(inout) :: A
        integer(int64), intent(in) :: min, max

        integer :: i, j
        integer :: ni, nj

        ni = size(A, DIM=1)
        nj = size(A, DIM=2)

        do i = 1,ni
            do j = 1,nj
                A(i,j) = urand(min, max)
            end do
        end do

    end subroutine

!=============================================================================!
!=                            Rank3 Fill Functions                           =!
!=============================================================================!
    subroutine fill_urand_rank3_r32(A, min, max)

        real(real32), dimension(:,:,:), intent(inout) :: A
        real(real32), intent(in) :: min, max

        integer :: i, j, k
        integer :: ni, nj, nk

        ni = size(A, DIM=1)
        nj = size(A, DIM=2)
        nk = size(A, DIM=3)

        do i = 1,ni
            do j = 1,nj
                do k = 1,nk
                    A(i,j,k) = urand(min, max)
                end do
            end do
        end do

    end subroutine

    subroutine fill_urand_rank3_r64(A, min, max)

        real(real64), dimension(:,:,:), intent(inout) :: A
        real(real64), intent(in) :: min, max

        integer :: i, j, k
        integer :: ni, nj, nk

        ni = size(A, DIM=1)
        nj = size(A, DIM=2)
        nk = size(A, DIM=3)

        do i = 1,ni
            do j = 1,nj
                do k = 1,nk
                    A(i,j,k) = urand(min, max)
                end do
            end do
        end do


    end subroutine

    subroutine fill_urand_rank3_i32(A, min, max)

        integer(int32), dimension(:,:,:), intent(inout) :: A
        integer(int32), intent(in) :: min, max

        integer :: i, j, k
        integer :: ni, nj, nk

        ni = size(A, DIM=1)
        nj = size(A, DIM=2)
        nk = size(A, DIM=3)

        do i = 1,ni
            do j = 1,nj
                do k = 1,nk
                    A(i,j,k) = urand(min, max)
                end do
            end do
        end do


    end subroutine

    subroutine fill_urand_rank3_i64(A, min, max)

        integer(int64), dimension(:,:,:), intent(inout) :: A
        integer(int64), intent(in) :: min, max

        integer :: i, j, k
        integer :: ni, nj, nk

        ni = size(A, DIM=1)
        nj = size(A, DIM=2)
        nk = size(A, DIM=3)

        do i = 1,ni
            do j = 1,nj
                do k = 1,nk
                    A(i,j,k) = urand(min, max)
                end do
            end do
        end do


    end subroutine

!=============================================================================!
!=                     Poisson Distribution Functions                        =!
!=============================================================================!


    function poisson(k, t, lamda) result (P)
        ! Purpose:
        !
        !   To return the probability that the number of calls, k, is observed after a given
        !   time, t.
        !
        ! Data dictionary : Declare values used inside the function
        integer, intent(in) :: k         ! The number of observations in a given time
        real, intent(in) :: t            ! The amount of time passed
        real, intent(in) :: lamda        ! The paramater of a poisson distribution
        real :: P                        ! The probability that k observations occured in time t
        integer :: k_fact
    
        k_fact = factorial(k)
        P = exp(-lamda * t) * (((lamda * t) ** k)/ k_fact)
    
    end function

!=============================================================================!
!=                     Normal Distribution Functions                         =!
!=============================================================================!

    function standard_normal_rand() result (x)

        real(real64) :: x 

        real(real64) :: x_1, x_2, r

        test_xs: do

            x_1 = urand_r64(-1._real64, 1._real64)
            x_2 = urand_r64(-1._real64, 1._real64)

            r = x_1*x_1 + x_2*x_2

            if (r >= 1) then
                cycle test_xs
            else
                exit test_xs
            end if
        end do test_xs

        x = sqrt( (-2._real64 * log(r) ) / r ) * x_1        

    end function







end module