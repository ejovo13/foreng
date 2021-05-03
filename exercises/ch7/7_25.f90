! Two's complement arithmetic

! Assume that a two's complement binary number is supplied in an eight-character
! variable containing 0s and 1s, and perform the following instructions:

! (A)
! Write a subroutine or function that adds 2 two's complement binary numbers stored
! in character variables, and returns the result in a third character variable.

module twos_complement

implicit none

contains 

    function add(twos_comp1, twos_comp2) result(sum)
        character(8), intent(in) :: twos_comp1, twos_comp2
        character(8) :: sum

        integer, parameter :: ONE = 49, ZERO = 48
        integer :: i, carry, bit1, bit2, int_sum, out_digit
        
        sum = "00000000"

        carry = 0
        99 format(I1)
        do i = 8,1,-1
            read(twos_comp1(i:),99) bit1
            read(twos_comp2(i:),99) bit2
            int_sum = bit1 + bit2 + carry

            print *, "bit1 = ", bit1, " bit2 = ", bit2, " carry = ", carry, " int_sum = ", int_sum


            if(int_sum == 2) then
                carry = 1
                out_digit = 0
            else if (int_sum == 3) then
                carry = 1
                out_digit = 1
            else                
                carry = 0
                out_digit = int_sum
            end if  
            
            write(sum(i:i),99) out_digit
            ! print *, " out = ", out_digit

        end do


    end function

end module

program two_test

    use twos_complement

    character(8) :: bin1, bin2, sum

    bin1 = "10101011"
    bin2 = "01010101"

    sum = add(bin1,bin2)

    print *, sum


end program