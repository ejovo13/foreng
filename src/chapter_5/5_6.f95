program logarithm_table
! Write a Fortran program to generate a table of the base 10 logarithms between 1 and 10 in steps 
! of 0.1. The table should include a title describing the table and row and column headings. The 
!  table should be organized as shown in exercise 5-6


    implicit none
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
end program