! Sort with Carry

! It is often useful to sort an array arr1 into ascending order, while simultaneaously
! carrying along a second array arr2. In such a sort, each time an element of arr1 is 
! exchanged with anoter element of arr1, the corresponding element of array arr2 are also
! swapped. When the sort is over, the elements of array arr1 are in ascending order, while
! the elements of arr2 that were associated with arr1 are still associated with them. 

! Write a subroutine to sort one real array into ascending order while carrying along
! a second one. Test the subroutine.

program sort_with_carry
implicit none

real, dimension(9) :: a,b

a = [1., 11., -6., 17.,-23., 0., 5., 1., -1.]
b = [31.,101., 36., -17., 0., 10., -8., -1., -1.]

call carry_sort(a,b)

print *, "A sorted = ", a
print *, "B sorted = ", b


contains

    subroutine carry_sort(arr1, arr2)

        real, dimension(:), intent(inout) :: arr1, arr2
        
        real :: temp_min, swap_value
        integer :: n, i, j, imin

        n = size(arr1)

        if (n /= size(arr2)) then
            error stop "Arr1 and Arr2 must be the same length"
        end if

        do i = 1,n
            temp_min = arr1(i)
            ! Find the min value
            do j = i,n
                if (arr1(j) <= temp_min) then
                    ! Mark the index of the min value
                    temp_min = arr1(j)
                    imin = j
                end if    
            end do
            ! Swap the value in arr1
            swap_value = arr1(imin)
            arr1(imin) = arr1(i)
            arr1(i) = swap_value

            ! Swap the value in arr2
            swap_value = arr2(imin)
            arr2(imin) = arr2(i)
            arr2(i) = swap_value

        end do


    end subroutine


end program

