! What will the output from each of the WRITE statements in the following program?
! Why is the output of the two statements different?

PROGRAM test_output
    IMPLICIT NONE
    INTEGER, DIMENSION(0:7) :: my_data
    INTEGER :: i, j
    my_data = [ 1, 2, 3, 4, 5, 6, 7, 8 ]
    DO i = 0,1
    WRITE (*,100) (my_data(4*i+j), j=0,3) ! Will write two lists of 4 items
    100 FORMAT (6(1X,I4))
    END DO
    WRITE (*,100) ((my_data(4*i+j), j=0,3), i=0,1) ! Will write one line with 6 items and a second line with 2 itmes
END PROGRAM test_output
    
