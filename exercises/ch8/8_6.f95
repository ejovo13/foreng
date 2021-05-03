! What will be printed out by the following program?

program test
    implicit none 
    integer, parameter :: N = 5, M = 10
    integer, dimension(N:M, M-N:M+N) :: info

    write (*, 100) shape(info) ! The array is 5 x 11
    100 format ('The shape of the array is:        ', 2I6)
    WRITE (*,110) SIZE(info)
    110 FORMAT ('The size of the array is: ',I6)
    WRITE (*,120) LBOUND(info)
    120 FORMAT ('The lower bounds of the array are: ',2I6)
    WRITE (*,130) UBOUND(info)
    130 FORMAT ('The upper bounds of the array are: ',2I6)
END PROGRAM test