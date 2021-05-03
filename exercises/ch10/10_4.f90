! Write a subroutine lcase that properly converts a string to lowecase regardless
! of collating sequence 

program lower_driver
implicit none

character(33) :: test_string

test_string = "YO WHATS UP MY FUCKING GUYYYYY!!!"

print *, test_string
print *, lcase(test_string)

contains

    function lcase(string) result(string_lower)

        character(*), intent(in) :: string
        character(len(string)) :: string_lower
        character(len(string)) :: temp_string

        integer :: i, char_int, str_len

        str_len = len(string)

        do i = 1, str_len
            char_int = iachar(string(i:i))

            if (char_int >= 65 .and. char_int <= 90) then
                char_int = char_int + 32
            end if

            temp_string(i:i) = achar(char_int)

        end do

        string_lower = temp_string

    end function

end program