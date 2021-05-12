module foreng_io_string

implicit none

    integer, parameter :: ZERO_ASCII = 48 !! ASCII value of the digit '0'
    integer, parameter :: NINE_ASCII = 57 !! ASCII value of the digit '9'
    integer, parameter :: UPPER_A_ASCII = 65 !! ASCII VALUE of the character 'A'
    integer, parameter :: UPPER_Z_ASCII = 90 !! ASCII VALUE of the character 'Z'
    integer, parameter :: LOWER_A_ASCII = 97 !! ASCII VALUE of the character 'a'
    integer, parameter :: LOWER_Z_ASCII = 122 !! ASCII VALUE of the character 'z'
    integer, parameter :: UPPER_LOWER_OFFSET = 32 !! The difference between UPPER_A_ASCII and LOWER_A_ASCII


contains

! // TODO DOCUMENT THE FUCK OUT OF THIS CODE

    logical elemental function is_alphnum(char)
    !! Check if a character is alphanumeric
    !! This function checks the ASCII collating sequence of the passed character and returns true if the character is alpha (A-Z, a-z) or numeric (0-9)
        character(1), intent(in) :: char

        if(is_num(char) .or. is_alph(char)) then
            is_alphnum = .true.
        else 
            is_alphnum = .false.
        end if

    end function


    logical elemental function is_alph(char)
    !! Check if a character belongs to the alphabet
    !! This function checks the ASCII collating sequence of the passed character and returns true if the character belongs to the Roman alphabet.
        character(1), intent(in) :: char

        if(is_upper(char) .or. is_lower(char)) then
            is_alph = .true.
        else 
            is_alph = .false.
        end if

    end function

    logical elemental function is_num(char)
    !! Check if a character is a numerical digit.
    !! Return true if a character is between 0-9
        character(1), intent(in) :: char
        integer :: char_value

        char_value = iachar(char)
        if(char_value >= ZERO_ASCII .and. char_value <= NINE_ASCII) then
            is_num = .true.
        else
            is_num = .false.
        end if 

    end function

    logical elemental function is_lower(char)
    !! Check if a character is a lowercase alpha character
    !! Returns true if the ASCII value of char lies between LOWER_A_ASCII and LOWER_Z_ASCII
        character(1), intent(in) :: char
        integer :: char_value

        char_value = iachar(char)
        if(char_value >= LOWER_A_ASCII .and. char_value <= LOWER_Z_ASCII) then
            is_lower = .true.
        else
            is_lower = .false.
        end if      

    end function

    logical elemental function is_upper(char)
    !! Check if a character is a uppercase alpha character
    !! Returns true if the ASCII value of char lies between UPPER_A_ASCII and UPPER_Z_ASCII
        character(1), intent(in) :: char
        integer :: char_value

        char_value = iachar(char)
        if(char_value >= UPPER_A_ASCII .and. char_value <= UPPER_Z_ASCII) then
            is_upper = .true.
        else
            is_upper = .false.
        end if        

    end function


    character(1) elemental function to_lower(char) result(char_upper)
    !! Convert an uppercase alpha character to its lowercase counterpart. If the passed character is not uppercase, return the same character.
        character(1), intent(in) :: char

        if (is_upper(char)) then 
            char_upper = achar((iachar(char) + UPPER_LOWER_OFFSET))        
        else 
            char_upper = char
        end if

    end function

    character(1) elemental function to_upper(char) result(char_lower)
    !! Convert a lowercase alpha character to its uppercase counterpart. If the passed character is not lowercase, return the same character.
        character(1), intent(in) :: char

        if (is_lower(char)) then 
            char_lower = achar((iachar(char) - UPPER_LOWER_OFFSET)) 
        else
            char_lower = char
        end if

    end function



end module