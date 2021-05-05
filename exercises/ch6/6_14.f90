!! Dot product

program dot_product
implicit none

!! Write a program that will read in two arrays and compute the dot product

    integer :: ierr
    character(100) :: errmsg
    real, dimension(3) :: V_1
    real, dimension(3) :: V_2
    real :: dprod

    print *, "Please enter the values for 2 Vectors with dimension 3"

    read: do

        read (*,*, iostat=ierr, iomsg=errmsg) V_1
        read (*,*, iostat=ierr, iomsg=errmsg) V_2

        if (ierr == 0) then
            exit read
        else 
            print *, "ERR: ", errmsg
            print *, "Please input valid real numbers"
        end if

    end do read

    !! Compute dot product
    dprod = sum(V_1 * V_2)

    print *, "The dot product of vectors:"
    print *, "V_1: ", V_1
    print *, "V_2: ", V_2
    print *, " = ", dprod


end program