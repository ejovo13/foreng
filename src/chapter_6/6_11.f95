! Rectangular to Polar Conversion

! Write a program that reads the rectangular components of a 2D vector into a 
! rank 1 array rect.


program polar_conversion
    implicit none
    
    real(8), parameter :: PI = 3.141592653589793
    real(8), dimension(2) :: polar, rect
    real(8) :: rads, mag
    integer :: ierr
    character(100) :: errmsg
    
    print *, "Please enter the x and y components of a point in rectangular coordinates"
    
    read(*,*,iostat=ierr, iomsg=errmsg) rect(1), rect(2)
    
    if (ierr == 0) then ! No error occurred
        rads = atan2(rect(2),rect(1))
        polar(2) = rads*(180)/PI
        mag = sqrt((rect(1)**2 + rect(2)**2))
        print *, rads, " in degrees is ", polar(2)
        polar(1) = mag
    end if
    
    print *, "The rect coordinates ", rect, " in polar form are:"
    print *, polar
    
    end program