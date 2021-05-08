module foreng_io_string


contains

! // TODO DOCUMENT THE FUCK OUT OF THIS CODE

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

        char_value = iachar(char)
        if(char_value >= 48 .and. char_value <= 57) then
            is_num = .true.
        else
            is_num = .false.
        end if 

    end function

    logical function is_lower(char)

        character(1), intent(in) :: char
        integer :: char_value

        char_value = iachar(char)
        if(char_value >= 97 .and. char_value <= 122) then
            is_lower = .true.
        else
            is_lower = .false.
        end if      

    end function

    logical function is_upper(char)

        character(1), intent(in) :: char
        integer :: char_value

        char_value = iachar(char)
        if(char_value >= 65 .and. char_value <= 90) then
            is_upper = .true.
        else
            is_upper = .false.
        end if        

    end function


    subroutine to_lower(char)

        character(1), intent(inout) :: char

        if (is_upper(char)) char = achar((iachar(char) + 32))        

    end subroutine

    subroutine to_upper(char)

        character(1), intent(inout) :: char

        if (is_lower(char)) char = achar((iachar(char) - 32)) 

    end subroutine



end module