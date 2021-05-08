SUBMODULE (foreng_numeric_matrices) foreng_numeric_matrices_gauss

IMPLICIT NONE

CONTAINS

    MODULE PROCEDURE gauss_elim_sing_32

       ! Data Dictionary : declare local types & definitions
        INTEGER, DIMENSION(size(A, DIM=1)) :: order             ! The order to process backwards substitution
        INTEGER :: neqs                                         ! Number of equations in linear system of equations
        INTEGER :: icol, i, j, index                            ! Various indexing variables 

        REAL(real32), PARAMETER :: eps = 1E-13                 ! Cutoff for 0
        REAL(real32), DIMENSION(:,:), ALLOCATABLE :: A_aug     ! Augmented matrix
        
        ! Initiate order matrix
        order = [ (i, i = 1, size(A,DIM=1)) ]
        neqs = size(A, DIM=1)

        allocate(A_aug(neqs, neqs+1))

        ! Instantiate Augmented matrix [A | b]
        if(allocated(A_aug)) then
            A_aug(:,1:neqs) = A
            A_aug(:,neqs+1) = b
        else 
            error stop "Error allocating A_aug"
        end if

        do icol = 1, neqs-1
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !       ELIMINATE ONE COLUMN        !
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call eliminate_column(A_aug, icol, order)

        end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!       Back Substitution       !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do i = neqs, 1, -1

            index = order(i)           
            x(i) = A_aug(index,neqs+1) 

            do j = neqs, i+1, -1

                x(i) = x(i) - A_aug(index,j)*x(j)

            end do

            x(i) = x(i) / A_aug(index,i)

        end do


    END PROCEDURE
    
    MODULE PROCEDURE gauss_elim_sing_64

        ! Data Dictionary : declare local types & definitions
        INTEGER, DIMENSION(size(A, DIM=1)) :: order             ! The order to process backwards substitution
        INTEGER :: neqs                                         ! Number of equations in linear system of equations
        INTEGER :: icol, i, j, index                            ! Various indexing variables 

        REAL(real64), PARAMETER :: eps = 1E-13                 ! Cutoff for 0
        REAL(real64), DIMENSION(:,:), ALLOCATABLE :: A_aug     ! Augmented matrix
        
        ! Initiate order matrix
        order = [ (i, i = 1, size(A,DIM=1)) ]
        neqs = size(A, DIM=1)

        allocate(A_aug(neqs, neqs+1))

        ! Instantiate Augmented matrix [A | b]
        if(allocated(A_aug)) then
            A_aug(:,1:neqs) = A
            A_aug(:,neqs+1) = b
        else 
            error stop "Error allocating A_aug"
        end if

        do icol = 1, neqs-1
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !       ELIMINATE ONE COLUMN        !
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call eliminate_column(A_aug, icol, order)

        end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!       Back Substitution       !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do i = neqs, 1, -1

            index = order(i)           
            x(i) = A_aug(index,neqs+1) 

            do j = neqs, i+1, -1

                x(i) = x(i) - A_aug(index,j)*x(j)

            end do

            x(i) = x(i) / A_aug(index,i)

        end do

    END PROCEDURE

    MODULE PROCEDURE gauss_elim_sing_128

        ! Data Dictionary : declare local types & definitions
        INTEGER, DIMENSION(size(A, DIM=1)) :: order             ! The order to process backwards substitution
        INTEGER :: neqs                                         ! Number of equations in linear system of equations
        INTEGER :: icol, i, j, index                            ! Various indexing variables 

        REAL(real128), PARAMETER :: eps = 1E-13                 ! Cutoff for 0
        REAL(real128), DIMENSION(:,:), ALLOCATABLE :: A_aug     ! Augmented matrix
        
        ! Initiate order matrix
        order = [ (i, i = 1, size(A,DIM=1)) ]
        neqs = size(A, DIM=1)

        allocate(A_aug(neqs, neqs+1))

        ! Instantiate Augmented matrix [A | b]
        if(allocated(A_aug)) then
            A_aug(:,1:neqs) = A
            A_aug(:,neqs+1) = b
        else 
            error stop "Error allocating A_aug"
        end if

        do icol = 1, neqs-1
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !       ELIMINATE ONE COLUMN        !
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call eliminate_column(A_aug, icol, order)

        end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!       Back Substitution       !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do i = neqs, 1, -1

            index = order(i)           
            x(i) = A_aug(index,neqs+1) 

            do j = neqs, i+1, -1

                x(i) = x(i) - A_aug(index,j)*x(j)

            end do

            x(i) = x(i) / A_aug(index,i)

        end do


    END PROCEDURE

    MODULE PROCEDURE gauss_elim_mult_32

        ! Data Dictionary : declare local types & definitions
        INTEGER, DIMENSION(size(A, DIM=1)) :: order             ! The order to process backwards substitution
        INTEGER :: neqs                                         ! Number of equations in linear system of equations
        INTEGER :: nsols                                        ! Number of columns of b, the solution vector we are solving for
        INTEGER :: icol, i, j, k, index                         ! Various indexing variables 

        REAL(real32), PARAMETER :: eps = 1E-13                 ! Cutoff for 0
        REAL(real32), DIMENSION(:,:), ALLOCATABLE :: A_aug     ! Augmented matrix


        ! Assert that matrix dimensions agree

        nsols = size(b, DIM=2)
        neqs = size(A, DIM=1)

        if (neqs /= size(b, DIM=1)) then
            error stop "Number of rows of A and b do not match"
        end if
        
        ! Initiate order matrix
        order = [ (i, i = 1, size(A,DIM=1)) ]

        allocate(A_aug(neqs, neqs+nsols))

        ! Instantiate Augmented matrix [A | b]
        if(allocated(A_aug)) then
            A_aug(:,1:neqs) = A
            A_aug(:,neqs+1:) = b
        else 
            error stop "Error allocating A_aug"
        end if

        do icol = 1, neqs-1
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !       ELIMINATE ONE COLUMN        !
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call eliminate_column(A_aug, icol, order)

        end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!       Back Substitution       !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        do k = 1,nsols

            do i = neqs, 1, -1

                index = order(i)           
                x(i,k) = A_aug(index,neqs+k) 

                do j = neqs, i+1, -1

                    x(i,k) = x(i,k) - A_aug(index,j)*x(j,k)

                end do

                x(i,k) = x(i,k) / A_aug(index,i)

            end do

        end do

    
    end procedure

    MODULE PROCEDURE gauss_elim_mult_64

        ! Data Dictionary : declare local types & definitions
        INTEGER, DIMENSION(size(A, DIM=1)) :: order             ! The order to process backwards substitution
        INTEGER :: neqs                                         ! Number of equations in linear system of equations
        INTEGER :: nsols                                        ! Number of columns of b, the solution vector we are solving for
        INTEGER :: icol, i, j, k, index                         ! Various indexing variables 

        REAL(real64), PARAMETER :: eps = 1E-13                 ! Cutoff for 0
        REAL(real64), DIMENSION(:,:), ALLOCATABLE :: A_aug     ! Augmented matrix


        ! Assert that matrix dimensions agree

        nsols = size(b, DIM=2)
        neqs = size(A, DIM=1)

        if (neqs /= size(b, DIM=1)) then
            error stop "Number of rows of A and b do not match"
        end if
        
        ! Initiate order matrix
        order = [ (i, i = 1, size(A,DIM=1)) ]

        allocate(A_aug(neqs, neqs+nsols))

        ! Instantiate Augmented matrix [A | b]
        if(allocated(A_aug)) then
            A_aug(:,1:neqs) = A
            A_aug(:,neqs+1:) = b
        else 
            error stop "Error allocating A_aug"
        end if

        do icol = 1, neqs-1
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !       ELIMINATE ONE COLUMN        !
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call eliminate_column(A_aug, icol, order)

        end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!       Back Substitution       !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        do k = 1,nsols

            do i = neqs, 1, -1

                index = order(i)           
                x(i,k) = A_aug(index,neqs+k) 

                do j = neqs, i+1, -1

                    x(i,k) = x(i,k) - A_aug(index,j)*x(j,k)

                end do

                x(i,k) = x(i,k) / A_aug(index,i)

            end do

        end do
    
    end procedure

    MODULE PROCEDURE gauss_elim_mult_128

        ! Data Dictionary : declare local types & definitions
        INTEGER, DIMENSION(size(A, DIM=1)) :: order             ! The order to process backwards substitution
        INTEGER :: neqs                                         ! Number of equations in linear system of equations
        INTEGER :: nsols                                        ! Number of columns of b, the solution vector we are solving for
        INTEGER :: icol, i, j, k, index                         ! Various indexing variables 

        REAL(real128), PARAMETER :: eps = 1E-13                 ! Cutoff for 0
        REAL(real128), DIMENSION(:,:), ALLOCATABLE :: A_aug     ! Augmented matrix


        ! Assert that matrix dimensions agree

        nsols = size(b, DIM=2)
        neqs = size(A, DIM=1)

        if (neqs /= size(b, DIM=1)) then
            error stop "Number of rows of A and b do not match"
        end if
        
        ! Initiate order matrix
        order = [ (i, i = 1, size(A,DIM=1)) ]

        allocate(A_aug(neqs, neqs+nsols))

        ! Instantiate Augmented matrix [A | b]
        if(allocated(A_aug)) then
            A_aug(:,1:neqs) = A
            A_aug(:,neqs+1:) = b
        else 
            error stop "Error allocating A_aug"
        end if

        do icol = 1, neqs-1
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !       ELIMINATE ONE COLUMN        !
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call eliminate_column(A_aug, icol, order)

        end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!       Back Substitution       !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        do k = 1,nsols

            do i = neqs, 1, -1

                index = order(i)           
                x(i,k) = A_aug(index,neqs+k) 

                do j = neqs, i+1, -1

                    x(i,k) = x(i,k) - A_aug(index,j)*x(j,k)

                end do

                x(i,k) = x(i,k) / A_aug(index,i)

            end do

        end do
    
    end procedure

    MODULE PROCEDURE eliminate_column_32

        INTEGER :: nrow 
        INTEGER :: ncol
        INTEGER :: nnz, max_scaled_pivot_index
        INTEGER :: irow, i, temp_row, temp_index
        REAL(real32) :: eps = 1E-13
        REAL(real32) :: scaled_pivot, max_scaled_pivot, scalar

        nrow = size(A, dim=1)
        ncol = size(A, dim=2)
        nnz = count(abs(A(icol:nrow, icol)) > eps)

        if(nnz == 0) then
            error stop "Matrix is singular, stopping elimination"
        end if

        ! Instantiate pivot variables to find max_scaled_pivot
        max_scaled_pivot = 0
        max_scaled_pivot_index = 0

        ! Find max_scaled_pivot and its index
        do irow = icol, nrow

            i = order(irow)

            if (abs(A(i,icol)) > eps) then

                scaled_pivot = abs(A(i, icol) / maxval(A(i,icol:nrow)))

                if (scaled_pivot > max_scaled_pivot) then
                    max_scaled_pivot = scaled_pivot
                    max_scaled_pivot_index = i
                end if                    

            end if                   

        end do

        if(max_scaled_pivot_index /= order(icol)) then
                
            temp_row = order(icol)
            temp_index = find_index(max_scaled_pivot_index, order, icol)
            ! print *, "temp_row:   ", temp_row
            ! print *, "temp_index: ", temp_index
            order(icol) = max_scaled_pivot_index
            order(temp_index) = temp_row
            
        end if

        ! print *, "order after swap: ", order
        
        ! Eliminate values under pivot column
        do irow = icol + 1, nrow

            scalar = A(order(irow),icol) / A(max_scaled_pivot_index,icol)

            ! Set the column we are eliminating to 0               
            A(order(irow),icol) = 0.0

            ! Perform row - scalar * pivot row
            A(order(irow),icol+1:) = A(order(irow),icol+1:) - scalar * A(max_scaled_pivot_index,icol+1:)           

        end do

    END PROCEDURE

    MODULE PROCEDURE eliminate_column_64

        INTEGER :: nrow 
        INTEGER :: ncol
        INTEGER :: nnz, max_scaled_pivot_index
        INTEGER :: irow, i, temp_row, temp_index
        REAL(real64) :: eps = 1E-13
        REAL(real64) :: scaled_pivot, max_scaled_pivot, scalar

        nrow = size(A, dim=1)
        ncol = size(A, dim=2)
        nnz = count(abs(A(icol:nrow, icol)) > eps)

        if(nnz == 0) then
            error stop "Matrix is singular, stopping elimination"
        end if

        ! Instantiate pivot variables to find max_scaled_pivot
        max_scaled_pivot = 0
        max_scaled_pivot_index = 0

        ! Find max_scaled_pivot and its index
        do irow = icol, nrow

            i = order(irow)

            if (abs(A(i,icol)) > eps) then

                scaled_pivot = abs(A(i, icol) / maxval(A(i,icol:nrow)))

                if (scaled_pivot > max_scaled_pivot) then
                    max_scaled_pivot = scaled_pivot
                    max_scaled_pivot_index = i
                end if                    

            end if                   

        end do

        if(max_scaled_pivot_index /= order(icol)) then
                
            temp_row = order(icol)
            temp_index = find_index(max_scaled_pivot_index, order, icol)
            ! print *, "temp_row:   ", temp_row
            ! print *, "temp_index: ", temp_index
            order(icol) = max_scaled_pivot_index
            order(temp_index) = temp_row
            
        end if

        ! print *, "order after swap: ", order
        
        ! Eliminate values under pivot column
        do irow = icol + 1, nrow

            scalar = A(order(irow),icol) / A(max_scaled_pivot_index,icol)

            ! Set the column we are eliminating to 0               
            A(order(irow),icol) = 0.0

            ! Perform row - scalar * pivot row
            A(order(irow),icol+1:) = A(order(irow),icol+1:) - scalar * A(max_scaled_pivot_index,icol+1:)           

        end do

    END PROCEDURE

    MODULE PROCEDURE eliminate_column_128

        INTEGER :: nrow 
        INTEGER :: ncol
        INTEGER :: nnz, max_scaled_pivot_index
        INTEGER :: irow, i, temp_row, temp_index
        REAL(real128) :: eps = 1E-13
        REAL(real128) :: scaled_pivot, max_scaled_pivot, scalar

        nrow = size(A, dim=1)
        ncol = size(A, dim=2)
        nnz = count(abs(A(icol:nrow, icol)) > eps)

        if(nnz == 0) then
            error stop "Matrix is singular, stopping elimination"
        end if

        ! Instantiate pivot variables to find max_scaled_pivot
        max_scaled_pivot = 0
        max_scaled_pivot_index = 0

        ! Find max_scaled_pivot and its index
        do irow = icol, nrow

            i = order(irow)

            if (abs(A(i,icol)) > eps) then

                scaled_pivot = abs(A(i, icol) / maxval(A(i,icol:nrow)))

                if (scaled_pivot > max_scaled_pivot) then
                    max_scaled_pivot = scaled_pivot
                    max_scaled_pivot_index = i
                end if                    

            end if                   

        end do

        if(max_scaled_pivot_index /= order(icol)) then
                
            temp_row = order(icol)
            temp_index = find_index(max_scaled_pivot_index, order, icol)
            ! print *, "temp_row:   ", temp_row
            ! print *, "temp_index: ", temp_index
            order(icol) = max_scaled_pivot_index
            order(temp_index) = temp_row
            
        end if

        ! print *, "order after swap: ", order
        
        ! Eliminate values under pivot column
        do irow = icol + 1, nrow

            scalar = A(order(irow),icol) / A(max_scaled_pivot_index,icol)

            ! Set the column we are eliminating to 0               
            A(order(irow),icol) = 0.0

            ! Perform row - scalar * pivot row
            A(order(irow),icol+1:) = A(order(irow),icol+1:) - scalar * A(max_scaled_pivot_index,icol+1:)           

        end do

    END PROCEDURE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! FIND_INDEX 
    ! ---------
    !
    !   Find the index of an integer value in an integer array. If the value 
    !   is not in the array, return -1
    !
    INTEGER PURE FUNCTION find_index(value, int_array, start_index) RESULT(index)

        INTEGER, INTENT(IN) :: value
        INTEGER, DIMENSION(:), INTENT(IN) :: int_array
        INTEGER, OPTIONAL, INTENT(IN) :: start_index
            
        INTEGER :: array_size, i
       
        index = -1

        array_size = SIZE(int_array)

        do i = start_index, array_size

            if (int_array(i) == value) then
                index = i
                return
            end if

        end do

    END FUNCTION

end submodule