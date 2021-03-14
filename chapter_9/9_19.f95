! Higher-Order Least-Squares Fits.

module least_squares

    contains

    !---------------------------------------------------------------
    ! SUBROUTINE generate_sys_eqns
    !
    !
    subroutine generate_sys_eqns(X, Y, order, A, u)
    ! Data dictionary
        real(8), dimension(:), intent(in) :: X, Y                   ! The x and y coordinates to be fit
        integer, intent(in) :: order                                ! The order of the polynomial to fit
        real(8), dimension(:,:), allocatable, intent(out) :: A      ! The system of equations we will solve
        real(8), dimension(:), allocatable, intent(out) :: u        ! The right hand side of Ac = u

        integer :: i,j,ndim

        ndim = order + 1

        allocate(A(ndim, ndim), u(ndim))


        do i = 1,ndim
            do j = 1,ndim
                A(i,j) = sum(X**(i + j - 2))
            end do
        end do

        A(1,1) = size(X)

        do i = 1,ndim
            u(i) = sum(Y * (X ** (i-1)))
        end do
    end subroutine

end module

program least_squares_test
    use least_squares

    implicit none

    real(8), dimension(3) :: X,Y
    integer :: order = 3
    real(8), dimension(:,:), allocatable :: A
    real(8), dimension(:), allocatable :: u
    integer :: i

    X = [ -1.0, 0.0, 1.0 ]
    Y = [ -1.0, 0.0, 1.0 ]

    call generate_sys_eqns(X, Y, order, A, u)

    do i = 1,4
        print *, A(i,:)
    end do



end program
