! 1. State the problem:

! Write a program caps that reads in a character string, searches for all of the words 
! within the string, and capitalizes the first letter of each word, while shifting the 
! remainder of the word to lowercase. Assume that all nonalphabetic and nonumeric characters
! can mark the boundaries of a word within the character variable (periods, commas,etc.). 
! Nonalphabetic characters should be left unchanged.

! 2. Define the input and output

! input: A character string from the terminal.
! output: The same string with the only the first letter per word capitalized.

! 3. Describe the algorithm

! We should first read in the string
! Alphabetic character ranges:
!   lowercase: [97, 122]
!   uppercase: [65, 90]
! Numeric character ranges:
!   [48, 57]
!
! We assume that all nonalphabetic and nonnumeric characters can mark the boundaries of a word.

program caps
! Purpose:
!   To capitalize the first letter per word in a character string.
implicit none

! Data dictionary:
integer, parameter :: MAX_STR_LEN = 500
character(:), allocatable :: input_str
character(30) :: FMT
integer :: i
integer :: num_chars
character(1) :: prev_char, this_char

print *, "Please enter a the number of characters to record"
read (*,"(I10)") num_chars


allocate(character(num_chars) :: input_str)
write(FMT, '("(A", I0, ")")') num_chars

! print *, "Format statement saved as: ", FMT

print *, "Now please enter the string you would like to transform"
read (*,FMT) input_str

print *, "String before transformation: ", input_str

if (is_alph(input_str(1:))) then
    call to_upper(input_str(1:))
end if

do i = 2,MAX_STR_LEN
    prev_char = input_str(i-1:)
    this_char = input_str(i:)

    ! If the character is the start of the word, then capitalize it
    if (.not. is_alphnum(prev_char)) then   ! If the previous character is a word boundary
        if(is_lower(this_char)) then        ! And this character is lowercase
            call to_upper(input_str(i:))    ! Then capitalize it
        end if
    else
        if(is_upper(this_char)) then
            call to_lower(input_str(i:))
        end if
    end if
end do

print *, "String after transformation: ", input_str


contains

    logical function is_alphnum(char)
        character(1), intent(in) :: char

        if(is_num(char) .or. is_alph(char)) then
            is_alphnum = .true.
        else 
            is_alphnum = .false.
        end if

    end function


    logical function is_alph(char)
        character(1), intent(in) :: char
        
        if(is_upper(char) .or. is_lower(char)) then
            is_alph = .true.
        else 
            is_alph = .false.
        end if

    end function

    logical function is_num(char)
        character(1), intent(in) :: char
        integer :: char_value

        char_value = ichar(char)
        if(char_value >= 48 .and. char_value <= 57) then
            is_num = .true.
        else
            is_num = .false.
        end if 


    end function

    logical function is_lower(char)
        character(1), intent(in) :: char
        integer :: char_value

        char_value = ichar(char)
        if(char_value >= 97 .and. char_value <= 122) then
            is_lower = .true.
        else
            is_lower = .false.
        end if      

    end function

    logical function is_upper(char)
        character(1), intent(in) :: char
        integer :: char_value

        char_value = ichar(char)
        if(char_value >= 65 .and. char_value <= 90) then
            is_upper = .true.
        else
            is_upper = .false.
        end if        

    end function

    subroutine to_lower(char)
        character(1), intent(inout) :: char

        if (is_upper(char)) char = achar((ichar(char) + 32))        

    end subroutine

    subroutine to_upper(char)
        character(1), intent(inout) :: char

        if (is_lower(char)) char = achar((ichar(char) - 32)) 

    end subroutine




end program