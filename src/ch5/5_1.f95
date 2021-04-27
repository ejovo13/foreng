! What is the purpose of a format? In what three ways can formats be specified?

!  The purpose of a format is to have complete control of the output/input of a program. We can
!  for example, style the output into a table using formats. 
!  We can define a format three different ways,
!  1. Using a raw format string in the call: write(*, '(F5.3, A)') my_float, my_character
!  2. Using a label combined with a format call: 100 FORMAT(F5.3, A)  
!                                                write(*,100) my_float, my_character
!  3. Storing the format with a variable: FMT = '(F5.3, A)'
!                                         write(*, FMT) my_float, my_character

program format_test
implicit none

character(20) :: my_char = "Yo what's good"
real :: my_float = 30.348234

character(30) :: FMT
FMT = '(F6.3, 10X, A)'

write(*,FMT) my_float, my_char


end program
