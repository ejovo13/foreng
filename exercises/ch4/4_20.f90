! Write a program that prompts a user for a decimal integer in the range 0 to 1023, and converts
! The number into binary. The binary number should consist of 0s and 1s in a string.

program to_binary
implicit none

    integer :: input = 0
    integer :: ierr, i
    character(LEN=100) :: errmsg
    character(LEN=10) :: binary_number = "0000000000"

    print *, "Enter an integer between 0 and 1023"

    read: do

        read(*,*,iostat=ierr, iomsg=errmsg) input

        if (ierr /= 0) then

            print *, "Please enter a valid number"
            cycle read

        else if (input < 0 .or. input > 1023) then

            print *, "Please enter an integer between 0 and 1023"
            cycle read

        else

            exit read

        end if

    end do read

    print *, "Input = ", input

    do i = 1, 10

        if (2 ** (10 - i) <= input) then
        ! if 2 ^ n is less than the remaining binary, subtract 2^n and add a 1 to the binary digit
            input = input - 2**(10-i)
            binary_number(i:i) = "1"

            ! print *, "Subtracted exponent: ", 2**(10-i)
            ! print *, "Binary number updated to ", binary_number
            ! print *, "Input updated to ", input
        end if

    end do

    print *, "Binary number: ", binary_number



end program