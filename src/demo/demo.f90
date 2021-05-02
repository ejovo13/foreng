program demo

use chapter_4

integer:: i=0, j=0, ierr
character(100) :: errmsg

print*  ! empty line
print 123
print 121, "| Fortran for Engineers demo suite - Evan Voyles |"
print 123

121 format(T16, a)
123 format(T16, 50("-"))

mainloop: do


    call chapter_select()

    print*, "[4] Chapter 4: Loops & Character Manipulation"
    print*, "[5] Chapter 5: Basic IO"
    print*, "[6] Chapter 6: Arrays"
    print*, "[7] Chapter 7: Procedures"
    print*, "[8] Chapter 8: Additional Features of Arrays"
    print*, "[9] Chapter 9: Additional Features of Procedures"
    print*, "[10] Chapter 10: Character Variables"

    print*
    print*, "0 to exit"

    read(unit=5, fmt=*, iostat=ierr, iomsg=errmsg) i

    if(ierr /=0) then
        print *, "please enter a valid integer from [0:10]"
        cycle mainloop
    end if


    select case(i)
    
        case(4)
            
            call chapter_selected(i)

            chapter4: do

                
                print *, "Problem 1: Legal expressions"
                print *, "Problem 2: Squares of even integers"
                print *, "Problem 3: Function evaluation"
                print *, "Problem 4: Piecewise function evaluation"
                print *, "Problem 5: Factorial"
                print *, "Problem 6: Difference between CYCLE and EXIT"
                print *, "Problem 7: Simple statistics"
                print *, "Problem 8: Loop execution counts"
                print *, "Problem 9: Loop execution values"
                print *, "Problem 10: Loop execution values (pt II)"
                print *, "Problem 11: Flight of ball"
                print *, "Problem 12: Flight of ball (pt II)"
                print *, "Problem 13: Day of the year"
                print *, "Problem 14: Logarithmic function evaluation"
                print *, "Problem 15: Uppercase to lowercase"
                print *, "Problem 16: Calculating orbits"
                print *, "Problem 17: Capitalize first letter"
                print *, "Problem 18: Current through a diode"
                print *, "Problem 19: Binary to decimal"

                call problem_select()

                read(unit=5, fmt=*, iostat=ierr, iomsg=errmsg) j

                if(ierr /=0) then
                    print *, "please enter a valid integer from [-1:19]"
                    cycle chapter4
                end if

                call program_start()

                select case(j)
                case(1)
                    call p4_1()
                case(2)
                    print *, "Problem 2 selected"
                case(3)
                    call p4_3()
                case(4)         
                    call p4_4()       
                case(5)
                    call p4_5()
                case(6)
                    call p4_6()
                case(7)
                    call p4_7()
                case(8)
                    call p4_8()
                case(9)
                    call p4_9()
                case(10)
                    call p4_10()
                case(11)
                    call p4_11()
                case(12)
                    call p4_12()
                case(13)
                    call p4_13()
                case(14)
                    call p4_14()
                case(15)
                    call p4_15()
                case(16)
                    call p4_16()
                case(17)
                    call p4_17()
                case(18)
                    call p4_18()
                case(19)
                    call p4_19()
                case(-1)
                    print *, "Going to chapter select"
                    exit chapter4
                case(0)
                    print *, "Exiting demo"
                    exit mainloop
                case default 
                    print *, "Please enter a valid number 1-19, -1 to go back, or 0 to exit"
                end select                
                
                call program_end()


            end do chapter4































        case(5)
            print *, "Chapter 5 selected"
        case(6)
            print *, "Chapter 6 selected"
        case(7)
            print *, "Chapter 7 selected"
        case(8)
            print *, "Chapter 8 selected"
        case(9)
            print *, "Chapter 9 selected"
        case(10)
            print *, "Chapter 10 selected"
        case(11)
            print *, "Chapter 11 selected"
        case(12)
            print *, "Chapter 12 selected"
        case(13)
            print *, "Chapter 13 selected"
        case(14)
            print *, "Chapter 14 selected"
        case(15)
            print *, "Chapter 15 selected"
        case(16)
            print *, "Chapter 16 selected"
        case(17)
            print *, "Chapter 17 selected"
        case(0)
            print *, "Exiting succesfully"
            exit mainloop
        case default 
            print *, "Please enter a number 1-17"
    end select






















    ! print*, "Example 1: Demo for xy plot"
    ! print*, "Example 2: Line specification"
    ! print*, "Example 3: Plot several data series at the same time"
    ! print*, "Example 4: Plot four data series at the same time"
    ! print*, "Example 5: Use line style, line colors and more..."
    ! print*, "Example 6: An interesting plot, sin(x) and its zero on the same plot"
    ! print*, "Example 7: Plot a matrix against a vector"
    ! print*, "Example 8: Plot a matrix against a vector and set the linespec and legend"
    ! print*, "Example 9: Use gnuplot for animation"
    ! print*, "Example 10: Use ogpf options"
    ! print*, "Example 11: simple polar plot"
    ! print*, "Example 12: A plot with logarithmic x axis"
    ! print*, "Example 13: A matrix plot with logarithmic y axis"
    ! print*, "Example 14: A loglog plot"
    ! print*, "Example 15: Plotting a function"
    ! print*, "Example 16: Save the gnuplot script into a file for future use"
    ! print*, "Example 17: Multi window plots, using script"
    ! print*, "Example 18: Running an external script file"
    ! print*, "Example 19: Multiple linestyle in matrix plot"
    ! print*, "Example 20: Scatter plot"
    ! print*, "Example 21: Stem plot"
    ! print*, "Example 22: Stem plot animation"
    ! print*, "Example 23: Another animation using matrix plot"
    ! print*, "Example 24: Multiplot layout"
    ! print*, "Example 25: Multiplot layout followed by simple plot"
    ! print*, "Example 26: Plot matrix vs. matrix"

    ! print*, "Example 27: Using secondary y axis"
    ! print*, "Example 28: Using secondary x and y axis"
    ! print*, "Example 29: Using color and size for title and labels"
    ! print*, "Example 30: More on labels color and size with secondary axes"
    ! print*
    ! print*, "***   Surface and Contour Plots ***"
    ! print*
    ! print*, "Example 101: Simple 3D plot using surf"
    ! print*, "Example 102: Surface plot and color palette "
    ! print*, "Example 103: Surface plot with hidden details and its contour"
    ! print*, "Example 104: Cylindrical mapping"
    ! print*, "Example 105: More contour plot"
    ! print*, "Example 106: Animation of 3D plots"
    ! print*, "Example 106: Multiplot layout in 3D"
    ! print*, "Example 107: Multiplot layout for 3D data"
    ! print*, "Example 108: Plot a 2D grid"

    ! print*
    ! write (unit=*, fmt='(a)') "2D plots: select an example: 1 through 30"
    ! write (unit=*, fmt='(a)') "3D plots: select an example: 101 through 108"
    ! write (unit=*, fmt='(a)', advance='no') "enter 0 for exit:  "
    ! read*, i

    ! select case(i)
    !     case(1)
    !         call exmp01
    !     case(2)
    !         call exmp02
    !     case(3)
    !         call exmp03
    !     case(4)
    !         call exmp04
    !     case(5)
    !         call exmp05
    !     case(6)
    !         call exmp06
    !     case(7)
    !         call exmp07
    !     case(8)
    !         call exmp08
    !     case(9)
    !         call exmp09
    !     case(10)
    !         call exmp10
    !     case(11)
    !         call exmp11
    !     case(12)
    !         call exmp12
    !     case(13)
    !         call exmp13
    !     case(14)
    !         call exmp14
    !     case(15)
    !         call exmp15
    !     case(16)
    !         call exmp16
    !     case(17)
    !         call exmp17
    !     case(18)
    !         call exmp18
    !     case(19)
    !         call exmp19
    !     case(20)
    !         call exmp20
    !     case(21)
    !         call exmp21
    !     case(22)
    !         call exmp22
    !     case(23)
    !         call exmp23
    !     case(24)
    !         call exmp24
    !     case(25)
    !         call exmp25
    !     case(26)
    !         call exmp26
    !     case(27)
    !         call exmp27
    !     case(28)
    !         call exmp28
    !     case(29)
    !         call exmp29
    !    case(30)
    !         call exmp30

    !         ! 3D plots

    !     case(101)
    !         call exmp101
    !     case(102)
    !         call exmp102
    !     case(103)
    !         call exmp103
    !     case(104)
    !         call exmp104
    !     case(105)
    !         call exmp105
    !     case(106)
    !         call exmp106
    !     case(107)
    !         call exmp107
    !    case(108)
    !         call exmp108


    !     case (0)
    !         print*, "Program terminated successfully"
    !         exit mainloop
    !     case default
    !         print*, "Try again, use a valid example number"
    !         print*, "Enter 0 to exit"
    ! end select
    ! print*
    ! print*, "press any key to continue..."
    ! read*
end do mainloop





contains 

    subroutine chapter_select()

        999 format(80("*"))
        111 format("*", T34, a, T80, "*")
        print *
        print 999
        print 111, "Select chapter"
        print 999
        print *

    end subroutine

    subroutine chapter_selected(chp)
        integer, intent(in) :: chp
        999 format(80("*"))
        111 format("*", T34, a, I0, a, T80, "*")
        print *
        print 999
        print 111, "Chapter ", chp, " selected"
        print 999
        print *

    end subroutine

    subroutine problem_select()
        999 format(80("*"))
        111 format("*", T34, a, T80, "*")
        print *
        print 999
        print 111, "Select problem"
        print 999
        print *
        print *, "  0 to exit, -1 to go back"

    end subroutine

    subroutine program_start()

        999 format(80("*"))
        111 format("*", T34, a, T80, "*")
        print *
        print 999
        print 111, "Program start"
        print 999
        print *

    end subroutine

    subroutine program_end()

        999 format(80("*"))
        111 format("*", T34, a, T80, "*")
        print *
        print 999
        print 111, "Program ended"
        print 999
        print *

        call to_continue()

    end subroutine

    subroutine to_continue()

        print *, "Press return to continue...."
        read *

    end subroutine



end program
