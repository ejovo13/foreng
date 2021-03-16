submodule (prototype) least_squares

contains

    module procedure least_squares_fit
    
        real(8), dimension(:,:), allocatable :: A 
        real(8), dimension(:), allocatable :: u

        allocate(c(order + 1))

        call generate_sys_eqns(x, y, order, A, u)
        call solve_n(A, c, u)
    
    end procedure

    module procedure generate_sys_eqns
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
    end procedure

    module procedure solve_n

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
        
        do i = 1,n
            print 111, i, x(i)
        end do

        111 format("X(", I0, ") = ", F12.7)

    end procedure

    module procedure switch_rows
        
        real(8), dimension(:), allocatable :: temp_row
        
        temp_row = A(row1,:)
        A(row1,:) = A(row2,:)
        A(row2,:) = temp_row
        
    end procedure

    
    module procedure back_sub
        
        integer :: n, i, j
        integer, dimension(2) :: U_shape
        
        U_shape = shape(U)
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
                    x(i) = x(i) - U(i,j) * x(j)
                end if
            end do
        end do
        
    end procedure
        
    module procedure elim_col
        
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
        
    end procedure
            
    module procedure is_zero
        
        if (abs(X) < 1E-9) then
            is_zero = .true.
        else 
            is_zero = .false.
        end if
            
    end procedure

    module procedure add_noise

        integer :: i

        do i = 1,51
            noise(i) = urand(min, max)
        end do
        Y_noise = Y + noise
        
    end procedure
                
                
                
end submodule