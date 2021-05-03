! How may arrays be declared?
! Arrays are declared using the following format:

program array_declaration
    implicit none
    integer, dimension(1:10) :: my_array
    integer, dimension(-10:-1) :: my_neg_array
    double precision, dimension(10) :: my_product
    my_array = 3
    my_neg_array = -2
    my_product = real(my_array) ** real(my_neg_array)

    print *, my_product

end program


    