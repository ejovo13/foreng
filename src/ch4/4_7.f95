program stats_2_modified
! Modify program stats_2 to use the DO WHILE construct instaed of the while construct currently in the program

!     A modified statistical analysis program that avoids the divide-by-zero problems inherent in
! program stats_1.
!
! Purpose:
!
!  To calculate mean and the standard deviation of an input
!
!  data set containing an arbitrary number of input values.
!
! Record of revisions:
!
!  Date      Programmer     Description of change
!  ====      ==========     =====================
! 11/10/15  S. J. Chapman   Original code
! 11/12/15  S. J. Chapman   Correct divide-by-0 error if
!                           0 or 1 input values given.
! 02/16/21  E. J. Voyles    Add do-while construct
!
IMPLICIT NONE
! Data dictionary: declare variable types, definitions, & units
INTEGER :: n = 0
 ! The number of input samples.
REAL :: std_dev = 0. ! The standard deviation of the input samples.
REAL :: sum_x = 0.
 ! The sum of the input values.
REAL :: sum_x2 = 0. ! The sum of the squares of the input values.
REAL :: x = 0.
 ! An input data value.
REAL :: x_bar
 ! The average of the input samples.
! While Loop to read input values.
DO
! Read in next value
WRITE (*,*) 'Enter number: '
READ (*,*) x
WRITE (*,*) 'The number is ', x
! Test for loop exit
IF ( x < 0 ) EXIT
    ! Otherwise, accumulate sums.
    n = n + 1
    sum_x = sum_x + x
    sum_x2 = sum_x2 + x**2
END DO
! Check to see if we have enough input data.
IF ( n < 2 ) THEN ! Insufficient information
    WRITE (*,*) 'At least 2 values must be entered!'
ELSE ! There is enough information, so
! calculate the mean and standard deviation
    x_bar = sum_x / real(n)
    std_dev = sqrt( (real(n) * sum_x2 - sum_x**2) / (real(n)*real(n-1)))
    ! Tell user.
    WRITE (*,*) 'The mean of this data set is:', x_bar
    WRITE (*,*) 'The standard deviation is: ', std_dev
    WRITE (*,*) 'The number of data points is:', n
END IF
END PROGRAM stats_2_modified
