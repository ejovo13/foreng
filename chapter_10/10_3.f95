! Rewrite ucase as a character function. Note that this function must return a 
! variable-length character string.

program ten_three
implicit none

character(36) :: string

string = "Yo whatsup my fucking bitchass boy!!"

print *, "Testing upper case function"

print *, string
print *, ucase(string)



contains 

function ucase(string) result(string_upper)

    character(*), intent(in) :: string
    character(len(string)) :: string_upper

    integer :: str_length, i, char_int
    character(len(string)) :: temp_string
    str_length = len(string)
    
    do i = 1,str_length
        char_int = iachar(string(i:i))
        if (char_int >= 97 .and. char_int <= 122) then
            char_int = char_int - 32
        end if
        temp_string(i:i) = achar(char_int)
    end do

    string_upper = temp_string

end function

end program