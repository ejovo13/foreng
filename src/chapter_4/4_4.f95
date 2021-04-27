program four_four
! Write the Fortran statements required to calculate y(t) from the equation:
    ! 
    !   y(t) = ------ -3t^2 + 5    t >= 0
    !           \---   3t^2 + 5    t < 0
    ! 
implicit none
real :: y, t

print *, "Please enter a t value that you would like to compute"
read *, t

if (t >= 0) then
    y = -3*(t**2) + 5
else 
    y = 3*(t**2) + 5
end if 

print 100, "y(t) with t = ", t, " is equal to ", y
100 format(A, ES9.2, A, ES9.2)

end program