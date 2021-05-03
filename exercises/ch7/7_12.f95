! Write a subroutine ucase that accepts a character string, andconverts any lowercase
! letter in the string to uppercase without affecting any nonalphabetic characters in the string.

program test_ucase
    implicit none
    character(30) :: my_lowercase
    character(30) :: my_upper

    print *, "Please enter a short expression (< 30 characters) to switch to uppercase"
    read 100, my_lowercase
    100 Format(30A)
    call ucase(my_lowercase, my_upper)

    


end program test_ucase

subroutine ucase (start_string, upper_case)
    implicit none
    character(30), intent(in) :: start_string
    character(30), intent(out) ::  upper_case
    integer :: str_len, ichar, char_value, new_char_value
    character :: this_char, new_char

    str_len = len(start_string)

    do ichar = 1,str_len
        this_char = start_string(ichar:)        

        char_value = iachar(this_char)
        if (char_value >= 97 .and. char_value <= 122) then
            new_char_value = char_value - 32
            new_char = achar(new_char_value)
        else
            new_char = this_char
        end if 
        upper_case(ichar:) = new_char

        ! print *, "Old char: ", this_char, "  with value: ", char_value, " changed to: ", new_char, "  with new value: ", &
        !  new_char_value
    end do


    print *, upper_case


end subroutine