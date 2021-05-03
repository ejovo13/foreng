program four_one
! Write a Fortran program to evaluate the equation y(x) = x^2 - 3x + 2 for all values between -1 and 3,
!  in steps of 0.1.

! Puprose: to solve exercise 4.3
! Data dictionary :: declared variables 
implicit none 

integer, parameter :: start_index = -1, end_index = 3
real, parameter :: step_size = 0.1
integer, parameter :: array_size = int((end_index - start_index)/step_size)
integer :: i
real, dimension(array_size) :: output = [(0.1*i + -1, i = 0,(array_size - 1))]
integer :: x

do x = 1,array_size
    output(x) = x**2 - 3*x + 2
end do
write(*,100) output
100 format(ES9.2)

end program