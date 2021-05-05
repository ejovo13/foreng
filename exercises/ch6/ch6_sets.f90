!! Union and Intersection functions (probably really slow algorithmically)

module ch6_sets
implicit none


contains

    function intersection(set1, set2) result (inters)

        integer, dimension(:), intent(in) :: set1, set2
        integer, dimension(:), allocatable :: inters

        integer :: set1_card
        integer :: i
        integer :: zero = 0

        set1_card = size(set1)

        do i = 1, set1_card

            if (belongs_to(set1(i), set2)) then
                !! If element of set1 is ALSO in set2...

                if (allocated(inters)) then
                    inters = [inters, set1(i)]
                else
                    allocate(inters(1))
                    inters = set1(i)
                end if
            else 
                cycle
            end if

        end do

        if (.not. allocated(inters)) then 
            !! If the intersection is the empty set
            allocate(inters(1))
            inters = zero/zero

        end if

    end function

    function union(set1, set2) result (uni)

        integer, dimension(:), intent(in) :: set1, set2
        integer, dimension(:), allocatable :: uni

        integer :: set2_card
        integer :: i

        set2_card = size(set2)

        !! Set union right away to set1

        uni = set1


        do i = 1, set2_card

            if(belongs_to(set2(i), uni)) then
            !! If set2(i) is already a part of the union, then don't add it
                cycle
            else
                uni = [uni, set2(i)]
            end if

        end do

    end function

    logical function belongs_to(x, A) result(bool)
    !! Check if an element belongs to a set

        integer, intent(in) :: x !! Element of set we are checking
        integer, dimension(:) :: A !! Set we are checking

        integer :: cardinality !! Cardinality (size) of the set to check
        integer :: i

        cardinality = size(A)

        do i = 1, cardinality

            bool = .false.

            if (A(i) == x) then 
                bool = .true.
                return
            else
                cycle
            end if


        end do

    end function


end module

