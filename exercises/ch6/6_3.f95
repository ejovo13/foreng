! Execute the following Fortran program on your computer with both bounds checking turned on and bounds
! checking turned off. What happens in each case. Hint, for gfortran use the -fcheck=bounds switch.

program bounds 
    implicit none
    real, dimension(5) :: test = [ 1., 2., 3., 4., 5. ]
    real, dimension(5) :: test1
    integer :: i
    do i = 1, 5
        test1(i) = sqrt(test(i))
        write (*, 100) 'SQRT(', test(i), ') = ', test1(i)
        100 format (A, F6.3, A, F14.4)
    end do
end program bounds
