SUBMODULE (foreng_numeric_matrices) foreng_numeric_matrices_inverse

IMPLICIT NONE

CONTAINS 

    MODULE PROCEDURE gauss_elim_inverse_32

        REAL(real32), DIMENSION(SIZE(A, DIM=1), SIZE(A, DIM=1)) :: Identity

        INTEGER :: i, j, nrow, ncol
        
        nrow = SIZE(A, DIM=1)
        ncol = SIZE(A, DIM=2)

        IF (nrow /= ncol) THEN 
            ERROR STOP "A is not a square matrix"
        END IF

        Identity = 0
        FORALL(i = 1:nrow, j = 1:ncol, i == j)
            Identity(i,j) = 1
        END FORALL

        A_inv = gauss_elim(A, Identity)        

    END PROCEDURE

    MODULE PROCEDURE gauss_elim_inverse_64

        REAL(real64), DIMENSION(SIZE(A, DIM=1), SIZE(A, DIM=1)) :: Identity

        INTEGER :: i, j, nrow, ncol
        
        nrow = SIZE(A, DIM=1)
        ncol = SIZE(A, DIM=2)

        IF (nrow /= ncol) THEN 
            ERROR STOP "A is not a square matrix"
        END IF

        Identity = 0
        FORALL(i = 1:nrow, j = 1:ncol, i == j)
            Identity(i,j) = 1
        END FORALL

        A_inv = gauss_elim(A, Identity)        

    END PROCEDURE

    MODULE PROCEDURE gauss_elim_inverse_128

        REAL(real128), DIMENSION(SIZE(A, DIM=1), SIZE(A, DIM=1)) :: Identity

        INTEGER :: i, j, nrow, ncol
        
        nrow = SIZE(A, DIM=1)
        ncol = SIZE(A, DIM=2)

        IF (nrow /= ncol) THEN 
            ERROR STOP "A is not a square matrix"
        END IF

        Identity = 0
        FORALL(i = 1:nrow, j = 1:ncol, i == j)
            Identity(i,j) = 1
        END FORALL

        A_inv = gauss_elim(A, Identity)        

    END PROCEDURE

END SUBMODULE