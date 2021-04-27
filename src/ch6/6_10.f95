! Polar to Rectangular Conversion.

! Write a program that reads the polar coordinates of a 2D vector into a rank
! 1 array POLAR. polar(1) will contain the mgnitude and polar(2) will contain the 
! angle theta in degrees, and converts the vector from
! polar to rectangular form, storing the result in a rank1 array RECT.
! Rect(1) should be the x component and rect(2) will be the y component.

program rect_conversion
implicit none

real(8), parameter :: PI = 3.141592653589793
real(8), dimension(2) :: polar, rect
real(8) :: rads
integer :: ierr
character(100) :: errmsg

print *, "Please enter the magintude and angle (degrees) of a point in polar coordinates"

read(*,*,iostat=ierr, iomsg=errmsg) polar(1), polar(2)

if (ierr == 0) then ! No error occurred
    rads = (polar(2)/180)*(PI)
    print *, polar(2), " in rads is: ", rads
    rect(1) = polar(1) * cos(rads)
    rect(2) = polar(1) * sin(rads)
end if

print *, "The polar coordinates ", polar, " in rectangular form are:"
print *, rect

end program
