! Write a Fortran program to convert all uppercase characters in a user-supplied character
! string to lowercase, without changing the uppercase and nonalphabetic characters in the string.
! Assume that your computer uses the ASCII collating sequence.

PROGRAM to_lower
!   Purpose:
!       This program converts all uppercase letters to lowercase letters
IMPLICIT NONE

! Data Dictionary: declare variable types, definitions, and units
integer, parameter :: MAX_LENGTH = 30
character(len=MAX_LENGTH) :: start_string
character(MAX_LENGTH) :: end_string
integer :: i
integer :: this_char_index
character :: this_char

print *, "Please enter a string you would like to convert to lowercase"
read '(30A)', start_string

do i = 1, MAX_LENGTH 
    this_char = start_string(i:i)
    this_char_index = iachar(this_char)

    if (this_char_index >= 65 .and. this_char_index <= 90) then
        end_string(i:i) = achar(this_char_index + 32)
    else
        end_string(i:i) = this_char
    end if

end do

print *, "Converted string: ", start_string, " to string: ", end_string



end program

