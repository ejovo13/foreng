program lib_test

    use prototype 

    implicit none

    real(8), dimension(:,:), allocatable :: A 
    real(8), dimension(:), allocatable :: u
    real(8), dimension(5) :: c
    real(8), dimension(6) :: x,y

    x = [-2.0, -1.0, 0.0, 1.0, 2.0, 3.0]
    y = [16.0, 1., 0.0, 1.0, 16.0, 81.0]

    call generate_sys_eqns(x, y, 4, A, u)

    call solve_n(A, c, u)

    ! Test the fitting of a polynomial of order 4


end program