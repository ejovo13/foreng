submodule (foreng_math) sets

implicit none


contains 

    module procedure intersection_int

        integer :: setA_card
        integer :: i
        integer :: zero = 0

        setA_card = size(setA)

        do i = 1, setA_card

            if (belongs_to(setA(i), setB)) then
                !! If element of set1 is ALSO in set2...

                if (allocated(inters)) then
                    inters = [inters, setA(i)]
                else
                    allocate(inters(1))
                    inters = setA(i)
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

    end procedure

    module procedure intersection_r32

        integer :: setA_card
        integer :: i
        real(real32) :: zero = 0
        real(real32) :: eps

        eps = array_epsilon([setA, setB])
    
        setA_card = size(setA)

        do i = 1, setA_card

            if (belongs_to(setA(i), setB, eps)) then
                !! If element of set1 is ALSO in set2...
                print *, "MATCH!!"
                if (allocated(inters)) then
                    inters = [inters, setA(i)]
                else
                    allocate(inters(1))
                    inters = setA(i)
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

    end procedure

    module procedure intersection_r64

        integer :: setA_card
        integer :: i
        real(real64) :: zero = 0
        real(real64) :: eps

        eps = array_epsilon([setA, setB])

        setA_card = size(setA)

        do i = 1, setA_card

            if (belongs_to(setA(i), setB, eps)) then
                !! If element of set1 is ALSO in set2...

                if (allocated(inters)) then
                    inters = [inters, setA(i)]
                else
                    allocate(inters(1))
                    inters = setA(i)
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

    end procedure

    module procedure union_int

        integer :: setB_cardinality
        integer :: i


        setB_cardinality = size(setB)

        !! Set union right away to setA

        union_set = setA

        do i = 1, setB_cardinality

            if(belongs_to(setB(i), union_set)) then
            !! If set2(i) is already a part of the union, then don't add it
                cycle
            else
                union_set = [union_set, setB(i)]
            end if

        end do

    end procedure

    module procedure union_r32

        integer :: setB_cardinality
        integer :: i


        setB_cardinality = size(setB)

        !! Set union right away to setA

        union_set = setA

        do i = 1, setB_cardinality

            if(belongs_to(setB(i), union_set)) then
            !! If set2(i) is already a part of the union, then don't add it
                print *, "MATCH FOUND!!"
                cycle
            else
                union_set = [union_set, setB(i)]
            end if

        end do

    end procedure

    module procedure union_r64

        integer :: setB_cardinality
        integer :: i


        setB_cardinality = size(setB)

        !! Set union right away to setA

        union_set = setA

        do i = 1, setB_cardinality

            if(belongs_to(setB(i), union_set)) then
            !! If set2(i) is already a part of the union, then don't add it
                cycle
            else
                union_set = [union_set, setB(i)]
            end if

        end do

    end procedure

    module procedure belongs_to_int
    !! Check if an element belongs to a set

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

    end procedure

    module procedure belongs_to_r32
        !! Check if an element belongs to a set

        integer :: cardinality !! Cardinality (size) of the set to check
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

    end procedure

    module procedure belongs_to_r64
        !! Check if an element belongs to a set
    
            integer :: cardinality !! Cardinality (size) of the set to check
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
    
    end procedure    

    module procedure array_epsilon_r32

        real(real32) :: max_val
        
        max_val = abs_max_val(array)
        eps = epsilon(max_val)

    end procedure

    module procedure array_epsilon_r64

        real(real64) :: max_val
        
        max_val = abs_max_val(array)
        eps = epsilon(max_val)

    end procedure

    module procedure abs_max_val_r32
        abs_max_val = maxval(abs(array))
    end procedure

    module procedure abs_max_val_r64
        abs_max_val = maxval(abs(array))
    end procedure



end submodule