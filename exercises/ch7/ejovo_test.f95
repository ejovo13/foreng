! To be compiled with ejovo, this program tests some calls
program ejovo_test

    use ejovo
    implicit none
    integer :: x = 4, y = 2, error

    print *, "Enter two integer values for x and y"
    read *, x, y

    call return_10()
    call three_args(x, y, error)

end program