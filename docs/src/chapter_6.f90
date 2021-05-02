module chapter_6
!! Arrays
use iso_fortran_env

implicit none

contains

    subroutine p6_1() 
    !! ** DISCUSSION QUESTION **
    !! How may arrays be declared?

        integer, dimension(1:10) :: my_array
        integer, dimension(-10:-1) :: my_neg_array
        double precision, dimension(10) :: my_product
        my_array = 3
        my_neg_array = -2
        my_product = real(my_array) ** real(my_neg_array)

        print *, my_product

    end subroutine

    subroutine p6_3()
    !! ** DISCUSSION QUESTION **
    !! Execute this subroutine on your computer with both bounds checking turned on and 
    !! bounds checking turned off. What happens?

        real, dimension(5) :: test = [ 1., 2., 3., 4., 5. ]
        real, dimension(5) :: test1
        integer :: i
        do i = 1, 5
            test1(i) = sqrt(test(i))
            write (*, 100) 'SQRT(', test(i), ') = ', test1(i)
            100 format (A, F6.3, A, F14.4)
        end do

    end subroutine

    subroutine p6_4()
    !! ** DISCUSSION QUESTION **
    !! Determine the shape and size of the arrays specified by the following statements:

        ! (a)
        ! character(len=80), dimension(60) :: line
        ! An array that is 1 dimensionsal, has 60 entries, and each entry is a charactar of length 80.

        ! (b)
        ! integer, parameter :: ISTART = 32
        ! INTEGER, PARAMETER :: ISTOP = 256
        ! INTEGER, DIMENSION(ISTART:ISTOP) :: char
        ! char is an array of size (256-32 + 1) = 225

        ! (c)
        ! INTEGER, PARAMETER :: NUM_CLASS = 3
        ! INTEGER, PARAMETER :: NUM_STUDENT = 35
        ! LOGICAL, DIMENSION(NUM_STUDENT, NUM_CLASS) :: passfail
        ! passfail is an array of size 33.

    end subroutine

    subroutine p6_5()
    !! ** DISCUSSION QUESTION **
    !! Determine which of the following Fortran program fragments are valid. For each valid statement,
    !! specify what will happen in the program. (Assume default typing for any variables that are not 
    !! explicitly typed within the program fragments.)

        ! (a)
        ! INTEGER, DIMENSION(100) :: icount, jcount
        !. . .
        ! icount = [ (i, i=1, 100)] ! implied do loop
        ! jcount = icount + 1
        ! (a) has valid syntax, this will create two arrays, icount which is a size 100 array that
        ! contains the sequence of natural numbers from 1 to 100, and jcount contains the sequence of 
        ! natural numbers from 2 to 101.

        ! print *, icount
        ! print *, jcount

        ! (b)
        ! REAL, DIMENSION(10) :: value 
        ! value(1:10:2) = [5., 4., 3., 2., 1. ]
        ! value(2:11:2) = [10., 9., 8., 7., 6. ]
        ! write (*, 100) value
        ! 100 format ('Value = ',/,(F10.2))

        ! (c)
        ! INTEGER, DIMENSION(6) :: a
        ! INTEGER, DIMENSION(6) :: b
        ! a = [1, -3, 0, -5, -9, 3]
        ! b = [-6, 6, 0, 5, 2, -1]
        ! WRITE (*, *) a > b
        
        ! All the fragments are valid!
        print *, "All code fragments are valid"

    end subroutine

    subroutine p6_6()
    !! ** DISCUSSION QUESTION ** 
    !! What is meant by each of the following array terms? Size, shape, extent, rank, conformable.

        integer, parameter :: DEFINITION_LENGTH = 65
        character(DEFINITION_LENGTH) :: sizeDef, shapeDef, extentDef, rankDef, conformableDef

        sizeDef = "Number of elements of A"
        shapeDef = "Return the dimensions of A"
        extentDef = "The number of elements in a single dimension"
        rankDef = "The number of dimensions"
        conformableDef = "Two arrays that have the same shape and extent are conformable"

        print 100, sizeDef, shapeDef, extentDef, rankDef, conformableDef
        100 Format("size: ", T30, A, /, "shape: ", T30, A, /, "extent: ", T30, A, /, "rank: ", T30, A, /, "conformable: ", T30, A)

    end subroutine

    subroutine p6_7()
    !! ** DISCUSSION QUESTION ** 
    !! Given an array my_array defined as shown, determine whether each of the following
    !! array sections is valid

        REAL, DIMENSION(-2:7) :: my_array = [-3, -2, -1, 0, 1, 2, 3, 4, 5, 6]
        INTEGER, DIMENSION(5) :: list = [ -2, 1, 2, 4, 2 ]

        ! (a)
        ! print *, my_array(-3, 3)

        ! (b)
        print *, my_array(-2:2)

        ! (c) 
        print *, my_array(1:5:2)

        ! (d)
        print *, my_array(list)

    end subroutine

    subroutine p6_8()
    !! ** DISCUSSION QUESTION **
    !! What will the output from each of the WRITE statements in the following subroutine be?
    !! Why is the output of the two statements different?

        INTEGER, DIMENSION(0:7) :: my_data
        INTEGER :: i, j
        my_data = [ 1, 2, 3, 4, 5, 6, 7, 8 ]
        DO i = 0,1
        WRITE (*,100) (my_data(4*i+j), j=0,3) ! Will write two lists of 4 items
        100 FORMAT (6(1X,I4))
        END DO
        WRITE (*,100) ((my_data(4*i+j), j=0,3), i=0,1) ! Will write one line with 6 items and a second line with 2 itmes

    end subroutine

    subroutine p6_10()
    !! Polar to Rectangular Conversion

    !! Write a program that reads the polar coordinates of a 2D vector into a rank
    !! 1 array POLAR. polar(1) will contain the mgnitude and polar(2) will contain the 
    !! angle theta in degrees, and converts the vector from
    !! polar to rectangular form, storing the result in a rank1 array RECT.
    !! Rect(1) should be the x component and rect(2) will be the y component.

        real(real64), parameter :: PI = 3.141592653589793
        real(real64), dimension(2) :: polar, rect
        real(real64) :: rads
        integer :: ierr
        character(100) :: errmsg

        print *, "Please enter the magnitude and angle (degrees) of a point in polar coordinates"

        read(*,*,iostat=ierr, iomsg=errmsg) polar(1), polar(2)

        if (ierr == 0) then ! No error occurred
            rads = (polar(2)/180)*(PI)
            print *, polar(2), " in rads is: ", rads
            rect(1) = polar(1) * cos(rads)
            rect(2) = polar(1) * sin(rads)
        end if

        print *, "The polar coordinates ", polar, " in rectangular form are:"
        print *, rect

    end subroutine

    subroutine p6_11()
    !! Rectangular to Polar Conversion

    !! Write a program that reads the rectangular components of a 2D vector into a rank 1
    !! array rect.

        real(real64), parameter :: PI = 3.141592653589793
        real(real64), dimension(2) :: polar, rect
        real(real64) :: rads, mag
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

    end subroutine
    

end module