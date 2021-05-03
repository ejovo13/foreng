! Minima and Maxima of a Function. Write a subroutine that attempts to locate 
! the maximum and minimum values of an arbitrary function over a certain range.
! The function being evaluated should be passed the the subroutine as a calling argument. 
! The subroutine should have the following input arguments:

! first_value
! last_value
! num_steps
! func

! and the following output arguments:

! xmin
! min_value
! xmax
! max_value

! 7-22 ::
! Write a test driver program for the subroutine using the function:
!
! f(x) = x^3 - 5x^2 + 5x + 2 and search for the min and max in 200 steps over the range -1 <= x <= 3

program min_max
implicit none

    real, external :: curve

    real :: first_value = -1.0, last_value = 3
    integer :: num_steps = 200
    real :: xmin, min_value, xmax, max_value

    call find_min_max(first_value, last_value, num_steps, curve, xmin, min_value, xmax, max_value)

    print *, "x_min = ", xmin
    print *, "x_max = ", xmax

contains 

    subroutine find_min_max(first_value, last_value, num_steps, func, &
                            xmin, min_value, xmax, max_value)
        ! Dummy arguments
        real, intent(in) :: first_value, last_value
        integer, intent(in) :: num_steps
        real, external :: func
        real, intent(out) :: xmin, min_value, xmax, max_value

        ! Local variables
        real :: step_size, x_i, f_x
        integer :: i

        step_size = (last_value - first_value)/(num_steps - 1)

        ! Initialize min and max values
        xmin = first_value
        min_value = func(xmin)
        xmax = first_value
        max_value = func(xmax)

        do i = 1,num_steps
            x_i = first_value + ((i - 1) * step_size)
            f_x = func(x_i)

            if (f_x <= min_value) then
                xmin = x_i
                min_value = f_x
            end if

            if (f_x >= max_value) then
                xmax = x_i
                max_value = f_x
            end if

            print 99, x_i, f_x
            99 format("f(", F8.3, ") = ", F12.3)
        end do

    end subroutine

end program


pure real function curve(x)
    real, intent(in) :: x
    curve = x**3 - (5*(x**2)) + (5*x) + 2
end function