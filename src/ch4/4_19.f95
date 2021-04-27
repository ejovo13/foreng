! 1. Binary to Decimal Conversion

! Write a program that promps a user for a binary number, which will be entered as a string
! of 0s and 1s in a character variable. This program should be able to handle numbers whos
! binary representation is up to 10 bits, corresponding to values from 0 to 1023.

program binary_converter
! Purpose:
!   To convert binary numbers to decimal.
implicit none

! Data dictionary
integer, parameter :: MAX_LEN = 10
integer, parameter :: ZERO_ASCII = 48
integer, parameter :: ONE_ASCII = 49
character(MAX_LEN) :: binary_representation
character(:), allocatable :: binary_trimmed
integer :: len_trim
logical :: input_is_valid = .true.
integer :: i
character(1) :: this_char
integer :: char_value
real :: decimal_representation = 0
integer :: this_bit


print *, "Please enter a binary number that is between 1 and 10 bits"
read (*,"(A10)") binary_representation


! print *, "Converting binary number: ", binary_representation
binary_trimmed = trim(binary_representation)
len_trim = len(binary_trimmed)

! First let's validate the input
do i = 1,len_trim
    this_char = binary_trimmed(i:)
    char_value = ichar(this_char)
    if (char_value /= ZERO_ASCII .and. char_value /= ONE_ASCII) then
        input_is_valid = .false.
        print *, "invalid char = ", this_char
        print *, "invalid val = ", char_value
        print *, "invalid pos = ", i

        print *, ""
        print *, "ZERO_ASCII = ", ZERO_ASCII
        print *, "ONE_ASCII = ", ONE_ASCII
        exit
    end if
end do

if (input_is_valid) then
    ! print *, "input is valid, converting now"

    do i = 1,len_trim
        read(binary_representation(i:),"(I1)") this_bit
        ! print *, "Current working bit: ", this_bit
        decimal_representation = (this_bit * 2 ** (len_trim - i)) + decimal_representation
    end do
else
    print *, "input is invalid, please try restarting the program"
end if

print 99,  binary_representation, int(decimal_representation)
99 format("binary num: ", T15, A10, /, 'decimal:', T15, I0 )

end program