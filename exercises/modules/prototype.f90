!------------------------------------------------------------------------------
! MODULE prototype
!
!       A collection of submodules that contain functions and 
!       problems worthy of collection and organization into 
!       a library
!
!------------------------------------------------------------------------------ 
module prototype 
    ! This module contains all of the functions that I find necessary/want 
    ! to highlight on my github page
    implicit none

    public :: least_squares_fit, solve_n


!------------------------------------------------------------------------------
! SUBMODULE least_squares
!
!       A series of subroutines and functions that are used to solve a system
!       of linear equations to find the least-squares fits of two dimensional
!       data points, using polynomials.
!
!------------------------------------------------------------------------------  
interface 

    !--------------------------------------------------------------------------
    ! SUBROUTINE least_squares_fit
    !
    !       Solve a least_squares problem and give the coefficients of 
    !       a polynomial of order ORDER.
    module subroutine least_squares_fit(X, Y, order, c)
    ! Data dictionary
        real(8), dimension(:), intent(in) :: X, Y                   ! The x and y coordinates to be fit
        integer, intent(in) :: order                                ! The order of the polynomial to fit
        real(8), dimension(:), allocatable, intent(out) :: c        ! The right hand side of Ac = u

    end subroutine

    !--------------------------------------------------------------------------
    ! SUBROUTINE generate_sys_eqns
    !
    !       Generate a sys of equations to solve a polynomial least squares 
    !       problem of order n.
    !
    module subroutine generate_sys_eqns(X, Y, order, A, u)
    ! Data dictionary
        real(8), dimension(:), intent(in) :: X, Y                   ! The x and y coordinates to be fit
        integer, intent(in) :: order                                ! The order of the polynomial to fit
        real(8), dimension(:,:), allocatable, intent(out) :: A      ! The system of equations we will solve
        real(8), dimension(:), allocatable, intent(out) :: u        ! The right hand side of Ac = u
        
    end subroutine

    !--------------------------------------------------------------------------
    ! SUBROUTINE solve_n
    !
    !       Solve a system of n equations by gaussian elimination
    !       of the form Ax = y.
    !--------------------------------------------------------------------------
    module subroutine solve_n(A, x, y)
        ! Purpose
        !   To solve the system of equations Ax = y, where A is a nxn matrix
    
        ! Data dictionary
        real(8), dimension(:,:), allocatable, intent(in) :: A           ! The linear transformation
        real(8), dimension(:), intent(out) :: x                         ! The vector that A acts upon
        real(8), dimension(:), intent(inout) :: y                       ! The right hand side of Ax = y

    end subroutine

    !--------------------------------------------------------------------------
    ! SUBROUTINE switch_rows(A, row1, row2)
    !
    !       Switch rows row1 and row2 for m x n matrix A
    !
    !--------------------------------------------------------------------------
    module subroutine switch_rows(A, row1, row2)

        real(8), dimension(:,:), intent(inout) :: A  
        integer, intent(in) :: row1, row2

    end subroutine

    !--------------------------------------------------------------------------
    ! SUBROUTINE back_sub(U, x, y)
    !
    !       When given an Upper triangular matrix whose diagonals
    !       are unit undergoe the backwards substitution step of
    !       Gauss's method.
    !
    !--------------------------------------------------------------------------
    module subroutine back_sub(U, x, y)
    ! Data dictionary
        real(8), dimension(:,:), intent(in) :: U            ! The upper Triangular matrix 
        real(8), dimension(:), intent(inout) :: x, y        ! Ux = y
        
    end subroutine

    !--------------------------------------------------------------------------
    ! SUBROUTINE elim_col(A, icol)
    !
    !       Perform Gaussian row elimination for a specified
    !       column. Reduces each pivot to unit and stops
    !       if the matrix is singular.
    !
    !--------------------------------------------------------------------------
    module subroutine elim_col(A, icol)
    ! Purpose
    !   To reduce the pivots of a given column.  The row pivot to divide by is assumed to be the same as the column number
    
    ! Data dictionary
        real(8), dimension(:,:), intent(inout) :: A       ! The matrix whose column we want to eliminate
        integer, intent(in) :: icol                       ! The column to eliminate
        
    
    end subroutine

    !--------------------------------------------------------------------------
    ! ELEMENTAL LOGICAL FUNCTION is_zero(X)
    !
    !       Perform Gaussian row elimination for a specified
    !       column. Reduces each pivot to unit and stops
    !       if the matrix is singular.
    !
    !--------------------------------------------------------------------------
    module elemental logical function is_zero(X) 

        real(8), intent(in) :: X

    end function

    !--------------------------------------------------------------------------
    ! SUBROUTINE add_noise(Y, noise, Y_noise, min, max)
    !
    !       Add a uniform distribution of random noise in the range
    !       [min, max) to the vector Y.
    !
    !--------------------------------------------------------------------------
    module subroutine add_noise(Y, noise, Y_noise, min, max)

        real(8), dimension(:), intent(in) :: Y
        real(8), dimension(:), intent(inout) :: noise, Y_noise
        real(4), intent(in) ::  min, max

        
    end subroutine

end interface

!------------------------------------------------------------------------------
! GENERIC SUBROUTINE print_mat2(X)
!
!       Print out a rank 2 matrix with the first dimension
!       represented as rows and the second as columns
!
!------------------------------------------------------------------------------
interface print_mat2

    module subroutine print_mati(A)
        integer, dimension(:,:), intent(in) :: A
    end subroutine

    module subroutine print_matr(A)
        real, dimension(:,:), intent(in) :: A
    end subroutine

    module subroutine print_matd(A)
        real(8), dimension(:,:), intent(in) :: A
    end subroutine

    module subroutine print_matc(A)
        complex, dimension(:,:), intent(in) :: A
    end subroutine

end interface 

contains

    !--------------------------------------------------------------------------
    ! REAL FUNCTION urand(min,max)
    !
    !       Return a real value between [min, max)
    !       Uniform distribution
    !
    !--------------------------------------------------------------------------
    real function urand(min,max)
        real, intent(in) :: min, max
        
        real :: temp_rand ! A value between 0 and 1
        real, parameter :: rand_mid = 0.5
        real :: scale ! What we should multiply the range by
        real :: shift ! What we should shift the dist by

        ! mid_point = (min + max) / 2
        shift = min
        scale = max - min

        call random_number(temp_rand)
        ! print *, temp_rand

        ! Shift the midpoint to the midpoint of the new range
        urand = temp_rand*scale + shift    
    end function

    

end module prototype
