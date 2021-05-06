!! Contrain functions used to compute distributions

module foreng_math_prob

use foreng_env
implicit none

!=============================================================================!
!=                      Random Distribution Interface                        =!
!=============================================================================!
interface urand
!! Return a uniform random variable from min to max
    module procedure urand_r32
    module procedure urand_r64
    module procedure urand_i32
    module procedure urand_i64

end interface 

interface fill_urand
    
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

    real(real32) function urand_r32(min,max)

        real(real32), intent(in) :: min, max

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
        urand_r32 = temp_rand*scale + shift   

    end function

    real(real64) function urand_r64(min,max)

        real(real64), intent(in) :: min, max

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
        urand_r64 = temp_rand*scale + shift   

    end function

    integer(int32) function urand_i32(min, max)
    !! Return a uniform random variable [min, max] 
        integer(int32), intent(in) :: min, max

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
        urand_i32 = int(temp_rand*scale + shift, int32)   

    end function

    integer(int64) function urand_i64(min, max)

        integer(int64), intent(in) :: min, max

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
        urand_i64 = int(temp_rand*scale + shift, int64)   

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












end module