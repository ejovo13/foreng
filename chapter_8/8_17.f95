! 1. State the problem

! Temperature Distribution on a Metallic Plate - Find the steady-state temperature
! of a metallic plate as described on page 403

! 2. Define input and output

! There is no input, but the output should be the steady-state solution of the temperature
! at each "cell"

! 3. Describe the algorithm

! Create a 10 by 10 array. Initialize all of the values to 50 degrees. Initialize the boundary
! values to 20 degrees. Set the value of cell (3,8) to be 100 degrees. 
! Calculate a new estimate of each cell by taking the average of it's 4 cardinal neighbors.

program steady_state
! Purpose:
!   To calculate the steady state temperature of a metal plate
implicit none

! Data dictionary: declare variables and their units
real, parameter :: EPSILON = 0.01
real, dimension(10,10) :: Tprev, Tcurrent, Tdiff
logical, dimension(10,10) :: Tlogical
integer :: i, j, nsteps = 0

! Initialize the metal plate
Tprev = 50
Tlogical = .false.
! Set the boundaries to 20 degrees
forall(i = 1:10, j = 1:10, is_boundary(i,j,10))
    Tlogical(i,j) = .true.
    Tprev(i,j) = 20
end forall
! Set (3,8) to 100 degrees
Tprev(3,8) = 100
Tcurrent = Tprev

! call print_log(Tlogical)
! call print_mat(Tprev)
! print *, "--------------------------------------------"
! call print_mat(Tcurrent)


! ------------------------------------------------- ! 

! Now let's calculate the next value

! This is one iteration

do 
    call diffuse(Tprev, Tcurrent, Tdiff)
    if (is_steady(Tdiff)) exit 
    nsteps = nsteps + 1

    if (nsteps > 150) then
        print *, "You done fucked up"
        exit
    end if

    
end do

print *, "Exited the do loop with ", nsteps, " iterations"
print *, "The steady state matrix is: "
print 999
call print_mat(Tcurrent)
print 111
print *, "The temperature of index (5,5) is: ", Tcurrent(5,5)



999 format(100("*"))
111 format(/)






! ------------------------------------------------ !

contains 

subroutine diffuse(Tprev, Tcurrent, Tdiff)
    real, dimension(10,10), intent(inout) :: Tprev, Tcurrent, Tdiff
    
    ! print *, "Prev matrix is: "
    ! print 999
    ! print 111
    ! call print_mat(Tprev)
    ! print 111
    Tprev = Tcurrent

    ! print *, "Current matrix is: "
    ! print 999
    ! call print_mat(Tcurrent)
    ! print 111
    

    forall(i = 2:9, j = 2:9)

        Tcurrent(i,j) = calc_next(i,j,Tprev)


    end forall       
    Tcurrent(3,8) = 100
    Tdiff = Tcurrent - Tprev

    ! print *, "Next matrix is: "
    ! print 999
    ! call print_mat(Tcurrent)
    ! print 111

    ! print *, "Difference matrix is: "
    ! print 999
    ! call print_mat(Tdiff)

end subroutine

logical function is_steady(Tdiff)
    real, dimension(10,10), intent(in) :: Tdiff
    logical, dimension(10,10) :: Tdiff_logical
    Tdiff_logical = abs(Tdiff) <= EPSILON
    if (all(Tdiff_logical)) then
        is_steady = .true.
        ! call print_log(Tdiff_logical)
    else 
        is_steady = .false.
    end if
end function

subroutine print_mat(arr) 
    real, dimension(10,10), intent(in) :: arr
    integer :: i
    do i = 1,10
        print *, arr(i,:)
    end do

end subroutine

subroutine print_log(arr) 
    logical, dimension(10,10), intent(in) :: arr
    integer :: i
    do i = 1,10
        print *, arr(i,:)
    end do

end subroutine


real pure function calc_next(i, j, array)

    integer, intent(in) :: i, j
    real, dimension(:,:), intent(in) :: array
    real, dimension(4) :: cardinal_neighbors

    cardinal_neighbors(1) = array(i-1,j)
    cardinal_neighbors(2) = array(i,j+1)
    cardinal_neighbors(3) = array(i+1,j)
    cardinal_neighbors(4) = array(i,j-1)

    calc_next = sum(cardinal_neighbors)/4

end function

logical pure function is_boundary(i, j, upper_bound)

    integer, intent(in) :: i, j, upper_bound    

    if (i == 1 .or. j == 1 .or. i == upper_bound .or. j == upper_bound) then 
        is_boundary = .true.
    else 
        is_boundary = .false.
    end if
end function

end program steady_state