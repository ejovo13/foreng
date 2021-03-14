! Second-Order Least-Squares Fits.

! Sometimes, it makes no sense to fit a set of data points to a straight line. For example,
! consider a thrown ball. We know from basic physicsthat the height of the ball versus time 
! will follow a parabolic shape, not a linear shape. How do we fit noisy data to a shape that 
! is not a straight line?

! To solve for the coefficients, we are going to setup a system of linear equations and then use
! an internal subroutine to solve the 3x3 matrix

program least_squares_two
! Purpose:
!   To calculate the coefficients of the second order parabola that best fits
!   a set of points
implicit none

! real(8), dimension(3,3) :: test_A
real(8), dimension(3,3) :: least_squares
real(8), dimension(3) :: c ! Ac = y
real(8), dimension(3) :: y ! Ac = y
real(8), dimension(18) :: x_coords, y_coords
integer :: i, ierr
character(100) :: emsg

open(unit=10, file='9_17_time.dat', action='read', status='old', iostat=ierr, iomsg=emsg)
open(unit=11, file='9_17_pos.dat', action='read', status='old', iostat=ierr, iomsg=emsg)


if (ierr == 0) then ! open was successful
    read(10,*) x_coords
    read(11,*) y_coords

    ! print *, "X_coords loaded in as: "
    ! print *, x_coords

    ! print *, "Y_coords loaded in as: "
    ! ! print *, y_coords
else 
    print *, "Open failed, ", emsg
    stop
end if



! test_A = reshape([ 1, 4, 7, 2, 5, 8, 3, 6, 9 ], [3,3])
! M = reshape([8, 3, 4, 1, 5, 9, 6, 7, 2], [3,3])
! y = [1.0,1.0,1.0]

! x_coords = [-1.0, 0.0, 1.0]
! y_coords = [1.0, 0.0, 1.0]

call build_sys_of_eqns(x_coords, y_coords, least_squares, y)

do i = 1,3
    print *, least_squares(i,:)
end do


call solve3(least_squares, c, y)
! call solve3(M, c, y)
! call solve3(test_A, c, y)




! 100 format(100("*"))


contains

    subroutine build_sys_of_eqns(X, Y, A, u)
    ! Purpose:
    !   Given a list of x and y coordinates, create the matrix representing the system of equations needed to solve
    !   a least squares problem
        real(8), dimension(:) :: X,Y
        real(8), dimension(3,3) :: A
        real(8), dimension(3) :: u ! This is the right hand side of the equation Ax = u

        integer :: num_coords
        real(8), dimension(:), allocatable :: X2, X3, X4, X2Y, XY

        num_coords = size(X)

        X2 = X * X
        X3 = X2 * X
        X4 = X3 * X
        X2Y = X2 * Y
        XY = X * Y

        A(1,1) = num_coords
        A(1,2) = sum(X)
        A(1,3) = sum(X2)
        A(2,1) = sum(X)
        A(2,2) = sum(X2)
        A(2,3) = sum(X3)
        A(3,1) = sum(X2)
        A(3,2) = sum(X3)
        A(3,3) = sum(X4)

        u(1) = sum(Y)
        u(2) = sum(XY)
        u(3) = sum(X2Y)

    end subroutine

    subroutine solve3(A, x, y)
    ! Purpose
    !   To solve the system of equations Ax = y, where A is a 3x3 matrix

    ! Data dictionary
        real(8), dimension(3,3), intent(in) :: A 
        real(8), dimension(3), intent(out) :: x
        real(8), dimension(3), intent(inout) :: y
        real(8) :: max_pivot
        integer :: i, ipivot, nstep

        ! What follows is a really crude version of gaussian elimination, and this is only
        ! tailored to solve a 3x3 matrix.

        real(8), dimension(3,4) :: A_aux
        A_aux(1:3,1:3) = A
        A_aux(1:3,4) = y


        
        ! Switch the pivot columns
        
        ! Eliminate the first column
        do nstep = 1,3
            ! Find the largest pivot in A_aux
            max_pivot = A_aux(nstep,nstep)
            do i = nstep,3
                if (abs(A_aux(i,nstep)) >= max_pivot) then
                    max_pivot = A_aux(i,nstep)
                    ipivot = i
                end if
            end do
            if (is_zero(max_pivot)) then
                STOP "Matrix is singular, stopping elimination"
            end if
        
            call switch_rows(A_aux, ipivot, nstep)

            call elim_col(A_aux, nstep)
     
        end do


        y = A_aux(:,4)

        call back_sub(A_aux(1:3,1:3), x, y)
        
        print *, "X = "
        do i = 1,3
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

    subroutine back_sub(UpperTriangular, x, y)
        real(8), dimension(:,:), intent(in) :: UpperTriangular
        real(8), dimension(:), intent(inout) :: x, y

        x(3) = y(3)
        x(2) = y(2) - (x(3) * UpperTriangular(2,3))
        x(1) = y(1) - (x(2) * UpperTriangular(1,2)) - (x(3) * UpperTriangular(1,3))

    end subroutine

    pure logical function is_zero(X) 
        real(8), intent(in) :: X
        if (abs(X) < 1E-9) then
            is_zero = .true.
        else 
            is_zero = .false.
        end if
    end function




end program
