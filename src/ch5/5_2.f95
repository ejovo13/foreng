! What is printed out by the following Fortran statemenst?
program test_output

    implicit none 

    ! (a)
    ! integer :: i
    ! character(len=20) :: fmt
    ! fmt = "('i = ', I8.5)"
    ! i = -123
    ! write(*,fmt) i
    ! write(*, '(I0)') i



    ! (b)
    ! real :: a, b, sum, difference
    ! a = 1.0020E6
    ! b = 1.0001E6
    ! sum = a + b
    ! difference = a - b
    ! write (*, 101) a, b, sum, difference
    ! 101 FORMAT('A = ', ES14.6, ' B = ', E14.6, &
    ! ' Sum = ', E14.6, ' Diff = ', F14.6)

    ! (c)
    ! integer :: i1, i2
    ! i1 = 10
    ! i2 = 4**2
    ! write (*, 300) i1 > i2
    ! 300 FORMAT ('Result = ', L6)

end program