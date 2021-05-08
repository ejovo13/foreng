module foreng_numeric_matrices

use foreng_env

! This module provides an interface for a variety of different elimination methods
! to solve systems of linear equations.

INTERFACE gauss_elim

    MODULE PROCEDURE gauss_elim_sing_32
    MODULE PROCEDURE gauss_elim_sing_64
    MODULE PROCEDURE gauss_elim_sing_128

    MODULE PROCEDURE gauss_elim_mult_32
    MODULE PROCEDURE gauss_elim_mult_64
    MODULE PROCEDURE gauss_elim_mult_128

    ! ERROR RETURNS:
    !----------------
    ! 111 - Error allocating memory for augmented matrix
    ! 999 - Nonsingular matrix

END INTERFACE 

INTERFACE inverse

    MODULE PROCEDURE gauss_elim_inverse_32
    MODULE PROCEDURE gauss_elim_inverse_64
    MODULE PROCEDURE gauss_elim_inverse_128

END INTERFACE 

INTERFACE lu

    MODULE PROCEDURE lu_decomp_32
    MODULE PROCEDURE lu_decomp_64
    MODULE PROCEDURE lu_decomp_128

END INTERFACE

INTERFACE eliminate_column

    MODULE PROCEDURE eliminate_column_32
    MODULE PROCEDURE eliminate_column_64
    MODULE PROCEDURE eliminate_column_128 

END INTERFACE


INTERFACE  ! Explicit declaration of gauss elimination functions

    MODULE FUNCTION gauss_elim_sing_32(A, b) RESULT(x)

        REAL(real32), DIMENSION(:,:), INTENT(IN) :: A            ! System of linear equations
        REAL(real32), DIMENSION(size(A,DIM=1)), INTENT(IN) :: b  ! RHS of the equation Ax = b
        REAL(real32), DIMENSION(size(A,DIM=1)) :: x              ! Solution to the system of equations

    END FUNCTION

    MODULE FUNCTION gauss_elim_sing_64(A, b) RESULT(x)

        REAL(real64), DIMENSION(:,:), INTENT(IN) :: A            ! System of linear equations
        REAL(real64), DIMENSION(size(A,DIM=1)), INTENT(IN) :: b  ! RHS of the equation Ax = b
        REAL(real64), DIMENSION(size(A,DIM=1)) :: x              ! Solution to the system of equations

    END FUNCTION

    MODULE FUNCTION gauss_elim_sing_128(A, b) RESULT(x)

        REAL(real128), DIMENSION(:,:), INTENT(IN) :: A            ! System of linear equations
        REAL(real128), DIMENSION(size(A,DIM=1)), INTENT(IN) :: b  ! RHS of the equation Ax = b
        REAL(real128), DIMENSION(size(A,DIM=1)) :: x  ! Solution to the system of equations

    END FUNCTION

    MODULE FUNCTION gauss_elim_mult_32(A, b) RESULT(x)

        
        REAL(real32), DIMENSION(:,:), INTENT(IN) :: A               ! System of linear equations
        REAL(real32), DIMENSION(:,:), INTENT(IN) :: b               ! RHS of the equation Ax = b
        REAL(real32), DIMENSION(size(A,DIM=1), size(b,DIM=2)) :: x  ! Solution to the system of equations

    END FUNCTION

    MODULE FUNCTION gauss_elim_mult_64(A, b) RESULT(x)

        
        REAL(real64), DIMENSION(:,:), INTENT(IN) :: A               ! System of linear equations
        REAL(real64), DIMENSION(:,:), INTENT(IN) :: b               ! RHS of the equation Ax = b
        REAL(real64), DIMENSION(size(A,DIM=1), size(b,DIM=2)) :: x  ! Solution to the system of equations

    END FUNCTION

    MODULE FUNCTION gauss_elim_mult_128(A, b) RESULT(x)

        
        REAL(real128), DIMENSION(:,:), INTENT(IN) :: A              ! System of linear equations
        REAL(real128), DIMENSION(:,:), INTENT(IN) :: b              ! RHS of the equation Ax = b
        REAL(real128), DIMENSION(size(A,DIM=1), size(b,DIM=2)) :: x ! Solution to the system of equations

    END FUNCTION

    MODULE FUNCTION gauss_elim_inverse_32(A) RESULT(A_inv)

        REAL(real32), DIMENSION(:,:), INTENT(IN) :: A
        REAL(real32), DIMENSION(size(A, DIM=1), size(A,DIM=1)) :: A_inv

    END FUNCTION

    MODULE FUNCTION gauss_elim_inverse_64(A) RESULT(A_inv)

        REAL(real64), DIMENSION(:,:), INTENT(IN) :: A
        REAL(real64), DIMENSION(size(A, DIM=1), size(A,DIM=1)) :: A_inv

    END FUNCTION

    MODULE FUNCTION gauss_elim_inverse_128(A) RESULT(A_inv)

        REAL(real128), DIMENSION(:,:), INTENT(IN) :: A
        REAL(real128), DIMENSION(size(A, DIM=1), size(A,DIM=1)) :: A_inv

    END FUNCTION

    MODULE SUBROUTINE eliminate_column_32(A, icol, order)

        REAL(real32), DIMENSION(:,:), INTENT(INOUT) :: A
        INTEGER, INTENT(IN) :: icol
        INTEGER, DIMENSION(:), INTENT(INOUT) :: order

    END SUBROUTINE

    MODULE SUBROUTINE eliminate_column_64(A, icol, order)

        REAL(real64), DIMENSION(:,:), INTENT(INOUT) :: A
        INTEGER, INTENT(IN) :: icol
        INTEGER, DIMENSION(:), INTENT(INOUT) :: order

    END SUBROUTINE

    MODULE SUBROUTINE eliminate_column_128(A, icol, order)

        REAL(real128), DIMENSION(:,:), INTENT(INOUT) :: A
        INTEGER, INTENT(IN) :: icol
        INTEGER, DIMENSION(:), INTENT(INOUT) :: order

    END SUBROUTINE

    MODULE SUBROUTINE lu_decomp_32(A, L, U)

        REAL(real32), DIMENSION(:,:), INTENT(IN) :: A
        REAL(real32), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: L, U

    END SUBROUTINE

    MODULE SUBROUTINE lu_decomp_64(A, L, U)

        REAL(real64), DIMENSION(:,:), INTENT(IN) :: A
        REAL(real64), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: L, U

    END SUBROUTINE

    MODULE SUBROUTINE lu_decomp_128(A, L, U)

        REAL(real128), DIMENSION(:,:), INTENT(IN) :: A
        REAL(real128), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: L, U

    END SUBROUTINE

END INTERFACE

end module