module chapter_5
!! All of the programming exercises for Chapter 5: Basic IO
use iso_fortran_env

contains 

    subroutine p5_1()
    !! ** DISCUSSION QUESTION ** 
    !! What is the purpose of a format? What are the three ways to define a format?

        print *, "The purpose of a format is to have complete control of the output/input of a program."
        print *, "We can, for example, style the output into a table using formats. "

        print *, "We can define a format three different ways."

        print 99, 1, "Using a raw format string in the call: "
        print 111, "write(*, '(F5.3, A)') my_float, my_character"
        print 99, 2, "Using a label combined with a format call:"
        print 111, "100 FORMAT(F5.3, A) ... write(*,100) my_float, my_character"
        print 99, 3, "Storing the format with a variable:"
        print 111, "FMT = '(F5.3,A)' ... write(*, FMT) my_float, my_character"

        111 format(T15, a)
        99 format(I0, "). ", a)

    end subroutine

    subroutine p5_2()
    !! ** DISCUSSION QUESTION **
    !! What is printed out by the following Fortran statements?
    
        print*, "(a)"
        print *, "integer :: i"
        print *, "character(len=20) :: fmt"
        print *, "fmt = '('i = ', I8.5)'"
        print *, "i = -123"
        print *, "write(*,fmt) i"
        print *, "write(*, '(I0)') i"

        ! (b)
        ! real :: a, b, sum, difference
        ! a = 1.0020E6
        ! b = 1.0001E6
        ! sum = a + b
        ! difference = a - b
        ! write (*, 101) a, b, sum, difference
        ! 101 FORMAT('A = ', ES14.6, ' B = ', E14.6, &
        ! ' Sum = ', E14.6, ' Diff = ', F14.6)

        ! (c)
        ! integer :: i1, i2
        ! i1 = 10
        ! i2 = 4**2
        ! write (*, 300) i1 > i2
        ! 300 FORMAT ('Result = ', L6)


    end subroutine

    subroutine p5_3()
    !! ** DISCUSSION QUESTION **
    !! What is printed out by the following Fortran statements?

        real :: a = 1.602E-19, b = 57.2957795, c = -1

        print *, "File:"
        print *, "----"
        print *, "real :: a = 1.602E-19, b = 57.2957795, c = -1"
        print *, "write (*, '(ES14.7, 2(1X, E13.7))') a, b, c"

        print *
        print *, "Output:"
        print *, "-------"
    
        write (*, '(ES14.7, 2(1X, E13.7))') a, b, c

    end subroutine

    subroutine p5_4(file_name)
    !! ** DISCUSSION QUESTION **
    !! Read in 5_4.dat and find out what the output will be for the following statements:
    
        character(*), intent(in) :: file_name
        Character(5) :: a
        Character(10) :: b
        character(15) :: c
        character(100) :: errmsg
        integer :: ierr

    
        open(unit=10, file=file_name, iostat=ierr, iomsg=errmsg, status="old")

        if (ierr /= 0) then 
            print *, "ERR: ", errmsg
        else 

            READ(unit=10, fmt='(3A10)') a, b, c
            close(unit=10)
        
            print *, "a: ", a , " b: ", b, " c: ", c
        end if

    end subroutine

    subroutine p5_5(file_name)
    !! ** DISCUSSION QUESTION **
    !! Read in 5_5.dat and find out what the value of each variable will be when the 
    !! READ statements have been completed

        character(*), intent(in) :: file_name
        integer :: item1, item2, item3, item4, item5
        integer :: item6, item7, item8, item9, item10
        integer :: ierr
        character(100) :: errmsg
        open(unit=10, file=file_name, status='old', action='read', iostat=ierr, iomsg=errmsg)

        if (ierr /= 0) then
            print *, "ERR: ", errmsg
        else 
        
            ! (a)
            read(10,*) item1, item2, item3, item4, item5, item6
            read(10,*) item7, item8, item9, item10

            ! (b)
            ! read (10, 8) item1, item2, item3, item4, item5, item6
            ! read (10, 8) item7, item8, item9, item10
            ! 8 FORMAT(4I10)

            print *, item1, item2, item3, item4, item5, item6, item7, item8, item9, item10
            close(unit=10)

        end if


    end subroutine

    subroutine p5_6()
        !! Generate a table of the base 10 logarithms between 1 and 10 in steps of 0.1

        integer :: ipower, jpower, i
        real, dimension(10,0:9) :: result

        ones_place: do ipower = 1, 10
            tenths_place: do jpower = 0, 9
                result(ipower, jpower) = log10(ipower + 0.1*jpower)
            end do tenths_place
        end do ones_place

        write (*, 200) (i, i = 0,9)
        write (*, 300)
        write (*, 100) (i, result(i,:), i = 1,10)
        100 format(I3, ".0  | ", 10F7.4)
        200 format(7(" "), "|", 10(4X, "X.", I0))
        300 format(80("-"))

    end subroutine

end module chapter_5