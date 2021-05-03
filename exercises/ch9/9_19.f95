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

    !---------------------------------------------------------------
    ! SUBROUTINE solve_n
    !
    !       Solve a system of n equations by gaussian elimination
    !---------------------------------------------------------------
    subroutine solve_n(A, x, y)
    ! Purpose
    !   To solve the system of equations Ax = y, where A is a nxn matrix

    ! Data dictionary
        real(8), dimension(:,:), intent(in) :: A 
        real(8), dimension(:), intent(out) :: x
        real(8), dimension(:), intent(inout) :: y
        real(8) :: max_pivot
        integer :: i, ipivot, nstep, n
        integer, dimension(2) :: A_shape

        ! What follows is a really crude version of gaussian elimination
      

        real(8), dimension(:,:), allocatable :: A_aux

        A_shape = shape(A)
        n = A_shape(1)

        allocate(A_aux(n,n+1))

        A_aux(1:n,1:n) = A
        A_aux(1:n,n+1) = y
                
        ! Switch the pivot columns
        
        ! Eliminate the first column
        do nstep = 1,n
            ! Find the largest pivot in A_aux
            max_pivot = A_aux(nstep,nstep)
            do i = nstep,n
                if (abs(A_aux(i,nstep)) >= max_pivot) then
                    max_pivot = A_aux(i,nstep)
                    ipivot = i
                end if
            end do
            ! print *, "max_pivot for col ", nstep, " = ", max_pivot
            if (is_zero(max_pivot)) then
                STOP "Matrix is singular, stopping elimination"
            end if
        
            call switch_rows(A_aux, ipivot, nstep)

            call elim_col(A_aux, nstep)

            ! print *, "A after elimination: "

            ! do i = 1,n
            !     print *, A_aux(i,:)
            ! end do
     
        end do


        y = A_aux(:,n+1)

        call back_sub(A_aux(1:n,1:n), x, y)
        
        print *, "X = "
        do i = 1,n
            print *, x(i)
        end do

    end subroutine

    subroutine switch_rows(A, row1, row2)
        
        real(8), dimension(:,:), intent(inout) :: A
        integer, intent(in) :: row1, row2

        real(8), dimension(:), allocatable :: temp_row

        temp_row = A(row1,:)
        A(row1,:) = A(row2,:)
        A(row2,:) = temp_row

    end subroutine
    
    subroutine back_sub(UpperTriangular, x, y)
        real(8), dimension(:,:), intent(in) :: UpperTriangular
        real(8), dimension(:), intent(inout) :: x, y
        integer :: n, i, j
        integer, dimension(2) :: U_shape

        U_shape = shape(UpperTriangular)
        n = U_shape(1)

        ! print *, "Entering backsub"
        ! print *, "n = ", n
        ! print *, "size(x) = ", size(x)
        ! print *, "size(y) = ", size(y)

        do i = n,1,-1
            do j = i,n
                ! print *, "i,j = ", i, j
                if (i == j) then
                    x(i) = y(i)
                else
                    x(i) = x(i) - UpperTriangular(i,j) * x(j)
                end if
            end do
        end do


    end subroutine

    subroutine elim_col(A, icol)
        ! Purpose
        !   To reduce the pivots of a given column.  The row pivot to divide by is assumed to be the same as the column number
            real(8), dimension(:,:), intent(inout) :: A
            integer, intent(in) :: icol
    
            real(8) :: coeff
            real(8) :: working_pivot
            integer, dimension(2) :: A_shape
            integer :: nrows, i
    
            coeff = A(icol,icol)
            A_shape = shape(A)
            nrows = A_shape(1)
    
            do i = icol+1,nrows
                working_pivot = A(i,icol)
                coeff = working_pivot/A(icol,icol)
                A(i,:) = A(i,:) - (coeff * A(icol,:))
            end do
    
            
            A(icol,:) = A(icol,:) * (1/A(icol,icol))
    
        end subroutine
    

    pure logical function is_zero(X) 
        real(8), intent(in) :: X
        if (abs(X) < 1E-9) then
            is_zero = .true.
        else 
            is_zero = .false.
        end if
    end function

end module

program least_squares_test
    use least_squares

    implicit none

    real(8), dimension(4) :: X,Y
    integer :: order = 3
    real(8), dimension(:,:), allocatable :: A
    real(8), dimension(:), allocatable :: u
    real(8), dimension(:), allocatable :: c
    integer :: i


    X = [ -1.0, 0.0, 1.0, 2.0]
    Y = [ -2.0, 0.0, 2.0, 16.0]

    
    call generate_sys_eqns(X, Y, order, A, u)

    print *, "Printing 4 x 4 generated sys of equations"

    do i = 1,4
        print *, A(i,:)
    end do

    print *, "Ac = u, Printing the U vector"

    do i = 1,4
        print *, u(i)
    end do

    allocate(c(order+1))
    
    call solve_n(A, c, u)




end program
