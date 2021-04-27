! Determine whether the following subroutine calls are correct or not. If there is an error,
! specify what is wrong with them.


! (a)
program sum_sqrt
    implicit none
    integer, parameter :: length = 20
    integer :: result 
    real :: test(length) = &
        [ 1., 2., 3., 4., 5., 6., 7., 8., 9., 10., &
        11., 12., 13., 14., 15., 16., 17., 18., 19., 20. ]

    call test_sub (length, test, result)

end program sum_sqrt

subroutine test_sub (length, array, res)
    implicit none
    integer, intent(in) :: length
    real, intent(out) :: res
    integer, dimension(length), intent(in) :: array
    integer :: i ! The intent attribute needed to be removed, because this is a local variable
    do i = 1, length
        res = res + sqrt(array(i)) ! Apparently, sqrt needs to be called with a real value
    end do
end subroutine test_sub


! (b)
PROGRAM test
    IMPLICIT NONE
    CHARACTER(len=8) :: str = '1AbHz05Z'
    CHARACTER :: largest
    CALL max_char (str, largest)
    WRITE (*,100) str, largest
    100 FORMAT (' The largest character in ', A, ' is ', A)
END PROGRAM test
SUBROUTINE max_char(string, big)
    IMPLICIT NONE
    CHARACTER(len=10), INTENT(IN) :: string
    CHARACTER, INTENT(OUT) :: big
    INTEGER :: i
    big = string(1:1)
    DO i = 2, 10
    IF ( string(i:i) > big ) THEN
    big = string(i:i)
    END IF
    END DO
END SUBROUTINE max_char
    