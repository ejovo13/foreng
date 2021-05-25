module foreng_math_sets
!! Contains functions that deal with the mathematical definition of sets. Find the union of two rank 1 arrays with \(\verb|union|\). Find 
!! the intersection with \(\verb|intersection|\). Check if a given x belongs to a set with \(\verb|belongs_to|\)
use foreng_env

implicit none

!=============================================================================!
!=                               Set Interface                               =!
!=============================================================================!
interface intersection 
!! Compute the intersection of two sets
    module procedure intersection_int
    module procedure intersection_r32
    module procedure intersection_r64

end interface

interface union
!! Compute the union of two sets
    module procedure union_int
    module procedure union_r32
    module procedure union_r64
end interface

interface belongs_to
!! Check if \(x \in \textrm{A}\) <br>
!! \(x\) belongs to \(A\) if there is at least one element \(y\) in \(A\) for which \(x = y\)
    module procedure belongs_to_int
    module procedure belongs_to_r32
    module procedure belongs_to_r64
end interface

interface array_epsilon
    module procedure array_epsilon_r32
    module procedure array_epsilon_r64
end interface

interface abs_max_val
    module procedure abs_max_val_r32
    module procedure abs_max_val_r64   
end interface 

!=============================================================================!
!=                               Set Functions                               =!
!=============================================================================!
contains 

    function intersection_int(A, B) result(inters)
        integer, dimension(:), intent(in) :: A !! set A
        integer, dimension(:), intent(in) :: B !! set B
        integer, dimension(:), allocatable :: inters !! \( A \cap B\)

        integer :: A_card
        integer :: i
        integer :: zero = 0

        A_card = size(A)

        do i = 1, A_card

            if (belongs_to(A(i), B)) then
                ! If element of set1 is ALSO in set2...

                if (allocated(inters)) then
                    inters = [inters, A(i)]
                else
                    allocate(inters(1))
                    inters = A(i)
                end if
            else 
                cycle
            end if

        end do

        if (.not. allocated(inters)) then 
            ! If the intersection is the empty set
            allocate(inters(1))
            inters = zero/zero

        end if

    end function

    function intersection_r32(A, B) result(inters)
        real(real32), dimension(:), intent(in) :: A !! set A
        real(real32), dimension(:), intent(in) :: B !! set B
        real(real32), dimension(:), allocatable :: inters !! \( A \cap B\)

        integer :: A_card
        integer :: i
        real(real32) :: zero = 0
        real(real32) :: eps

        eps = array_epsilon([A, B])
    
        A_card = size(A)

        do i = 1, A_card

            if (belongs_to(A(i), B, eps)) then
                ! If element of set1 is ALSO in set2...
                if (allocated(inters)) then
                    inters = [inters, A(i)]
                else
                    allocate(inters(1))
                    inters = A(i)
                end if
            else 
                cycle
            end if

        end do

        if (.not. allocated(inters)) then 
            ! If the intersection is the empty set
            allocate(inters(1))
            inters = zero/zero

        end if

    end function

    function intersection_r64(A, B) result(inters)
        real(real64), dimension(:), intent(in) :: A !! set A
        real(real64), dimension(:), intent(in) :: B !! set B
        real(real64), dimension(:), allocatable :: inters !! \( A \cap B\)

        integer :: A_card
        integer :: i
        real(real64) :: zero = 0
        real(real64) :: eps

        eps = array_epsilon([A, B])

        A_card = size(A)

        do i = 1, A_card

            if (belongs_to(A(i), B, eps)) then
                ! If element of set1 is ALSO in set2...

                if (allocated(inters)) then
                    inters = [inters, A(i)]
                else
                    allocate(inters(1))
                    inters = A(i)
                end if
            else 
                cycle
            end if

        end do

        if (.not. allocated(inters)) then 
            ! If the intersection is the empty set
            allocate(inters(1))
            inters = zero/zero

        end if

    end function

    function union_int(A, B) result(union_set)
        integer, intent(in), dimension(:) :: A !! set A
        integer, intent(in), dimension(:) :: B !! set B
        integer, dimension(:), allocatable :: union_set !! \( A \cup B\)

        integer :: B_cardinality
        integer :: i


        B_cardinality = size(B)

        ! Set union right away to A

        union_set = A

        do i = 1, B_cardinality

            if(belongs_to(B(i), union_set)) then
            ! If set2(i) is already a part of the union, then don't add it
                cycle
            else
                union_set = [union_set, B(i)]
            end if

        end do

    end function

    function union_r32(A, B) result(union_set)
        real(real32), intent(in), dimension(:) :: A !! set A
        real(real32), intent(in), dimension(:) :: B !! set B
        real(real32), dimension(:), allocatable :: union_set !! \( A \cup B\)

        integer :: B_cardinality
        integer :: i


        B_cardinality = size(B)

        ! Set union right away to A

        union_set = A

        do i = 1, B_cardinality

            if(belongs_to(B(i), union_set)) then
            ! If set2(i) is already a part of the union, then don't add it
                print *, "MATCH FOUND!!"
                cycle
            else
                union_set = [union_set, B(i)]
            end if

        end do

    end function

    function union_r64(A, B) result(union_set)
        real(real64), intent(in), dimension(:) :: A !! set A
        real(real64), intent(in), dimension(:) :: B !! set B
        real(real64), dimension(:), allocatable :: union_set !! \( A \cup B\)

        integer :: B_cardinality
        integer :: i


        B_cardinality = size(B)

        ! Set union right away to A

        union_set = A

        do i = 1, B_cardinality

            if(belongs_to(B(i), union_set)) then
            ! If set2(i) is already a part of the union, then don't add it
                cycle
            else
                union_set = [union_set, B(i)]
            end if

        end do

    end function

    logical function belongs_to_int(x, A) result(bool)
    !! Check if a value \(x\) belongs to a set \(\textrm{A}\)
        integer, intent(in) :: x 
        integer, dimension(:) :: A
        ! Check if an element belongs to a set

        integer :: cardinality ! Cardinality (size) of the set to check
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

    logical function belongs_to_r32(x, A, eps) result(bool)
    !! Check if a value \(x\) belongs to a set within a certain tolerance \(\epsilon\). If \(\epsilon\) is not specified, it will automatically
    !! calculated using the \(\verb|array_epsilon|\) function
        real(real32), intent(in) :: x 
        real(real32), dimension(:) :: A
        real(real32), optional :: eps !! \(x = y \iff \textrm{abs}(x - y) < \epsilon\)
        ! Check if an element belongs to a set

        integer :: cardinality ! Cardinality (size) of the set to check
        integer :: i
        real(real32) :: eps_

        if(.not. present(eps)) then
            eps_ = array_epsilon(A)
        else
            eps_ = eps
        end if

        cardinality = size([A, x])

        do i = 1, cardinality

            bool = .false.

            if (abs(A(i) - x) <= eps_) then 
                bool = .true.
                return
            else
                cycle
            end if

        end do

    end function

    logical function belongs_to_r64(x, A, eps) result(bool)
    !! Check if a value \(x\) belongs to a set within a certain tolerance \(\epsilon\). If \(\epsilon\) is not specified, it will automatically
    !! calculated using the \(\verb|array_epsilon|\) function
        real(real64), intent(in) :: x 
        real(real64), dimension(:) :: A
        real(real64), optional :: eps !! \(x = y \iff \textrm{abs}(x - y) < \epsilon\)
        ! Check if an element belongs to a set
    
            integer :: cardinality ! Cardinality (size) of the set to check
            integer :: i
    
            if(.not. present(eps)) then
                eps = array_epsilon([A, x])
            end if
    
            cardinality = size(A)
    
            do i = 1, cardinality
    
                bool = .false.
    
                if (abs(A(i) - x) < eps) then 
                    bool = .true.
                    return
                else
                    cycle
                end if
    
            end do
    
    end function    

    real(real32) function array_epsilon_r32(array) result(eps)
        real(real32), dimension(:), intent(in) :: array   

        real(real32) :: max_val
        
        max_val = abs_max_val(array)
        eps = epsilon(max_val)

    end function

    real(real64) function array_epsilon_r64(array) result(eps)
        real(real64), dimension(:), intent(in) :: array  

        real(real64) :: max_val
        
        max_val = abs_max_val(array)
        eps = epsilon(max_val)

    end function

    real(real32) function abs_max_val_r32(array) result(abs_max_val)
        real(real32), dimension(:), intent(in) :: array
        abs_max_val = maxval(abs(array))
    end function

    real(real64) function abs_max_val_r64(array) result(abs_max_val)
        real(real64), dimension(:), intent(in) :: array
        abs_max_val = maxval(abs(array))
    end function



end module