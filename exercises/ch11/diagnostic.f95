! Play around with kinds

program kind_test

use new_types

implicit none

integer :: i
real :: r1
real(8) :: r2
integer, parameter :: my_type = selected_real_kind(20, 100), my_other_type = selected_real_kind(30)
type (vector) :: v1
complex(8) :: C1
real(my_type) :: r3


print 99, "r1", kind(r1)
print 111, "r1", huge(r1)
print 99, "r2", kind(r2)
print 111, "r2", huge(r2)
print 111, "r3", huge(r3)
print 99, "i", kind(i)
print 112, "i", huge(i)
print 99, "my_type", my_type
print 99, "my_type", my_other_type

C1 = (10.0, 5.0)

111 format("Max value of ", A9, " is: " , ES30.7)
112 format("Max value of ", A9, " is: " , I0)
99 format("The kind of ", A10, " is: ", I0)

v1%x = 10.0
v1%y = 11.0

print *, "v1 = ", v1
print *, "c1 = ", c1

print *, huge(r3)
print *, huge(r2)

r1 = 10.0
r2 = 10.0
r3 = 10.0

print *, r1
print *, r2
print *, r3



end program

module new_types
implicit none

type vector
    real :: x
    real :: y
    character(10) :: name = "default"
end type


end module