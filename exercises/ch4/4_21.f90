! Octal to decimal conversion

! Up to five octal digits
program octal_to_decimal

    integer, parameter :: MAX_OCTAL_DIGITS = 5
    integer :: i, ierr, num_oct_digits, decimal_conversion = 0
    character(LEN=100) :: errmsg
    character(MAX_OCTAL_DIGITS) :: input
    character :: this_dig_char
    character(:), ALLOCATABLE :: octal
    integer :: this_dig_int

    print *, "Enter an octal number up to 5 digits"

    read: do

        read (*,*, iostat=ierr, iomsg=errmsg) input

        if (ierr /= 0) then
            print *, "Please enter a valid octal number"
            cycle
        else if (.not. verify_octal(trim(input))) then
            print *, "Octal numbers only have digits 0-7"
        else
            exit read
        end if

    end do read
        
    octal = trim(input)
    num_oct_digits = len(octal)

    do i = 1, num_oct_digits
        
        this_dig_char = octal(i:i)
        read(this_dig_char, *) this_dig_int

        decimal_conversion = decimal_conversion + this_dig_int*(8 ** (num_oct_digits - i))

    end do

    123 format(A, T25, A)
    234 format(A, T25, I0)
    print 123, "Octal input value:", octal
    print 234, "Decimal conversion:", decimal_conversion
    
contains

    logical function verify_octal(oct)
    !! Verify that the character string is a valid octal number

        character(LEN=*), intent(in) :: oct

        integer, parameter :: ASCII_ZERO = 48, ASCII_SEVEN = 55        
        integer :: ndigits = 0, i = 0
        integer :: this_digit_int

        ndigits = len(oct)

        do i = 1,ndigits

            this_digit_int = iachar(oct(i:i))

            if (this_digit_int < ASCII_ZERO .or. this_digit_int > ASCII_SEVEN) then
                verify_octal = .false.
                return
            end if

        end do

        verify_octal = .true.

    end function


end program