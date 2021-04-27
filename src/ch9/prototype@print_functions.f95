submodule (prototype) print_functions

contains

    module procedure print_mati

        integer, dimension(2) :: shape_A
        integer :: nrows, i

        shape_A = shape(A)
        nrows = shape_A(1)

        do i = 1, nrows
            print *, A(i,:)
        end do

    end procedure

    module procedure print_matr

        integer, dimension(2) :: shape_A
        integer :: nrows, i

        shape_A = shape(A)
        nrows = shape_A(1)

        do i = 1, nrows
            print *, A(i,:)
        end do

    end procedure

    module procedure print_matd

        integer, dimension(2) :: shape_A
        integer :: nrows, i

        shape_A = shape(A)
        nrows = shape_A(1)

        do i = 1, nrows
            print *, A(i,:)
        end do

    end procedure  

    module procedure print_matc

        integer, dimension(2) :: shape_A
        integer :: nrows, i

        shape_A = shape(A)
        nrows = shape_A(1)

        do i = 1, nrows
            print *, A(i,:)
        end do

    end procedure
        

end submodule