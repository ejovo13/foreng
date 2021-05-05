!! Set theory - Union and Intersection

!!

program set_theory

use, intrinsic :: ieee_arithmetic, only: IEEE_value, IEEE_QUIET_NAN
use ch6_sets


implicit none

    integer :: ierr
    character(100) :: errmsg
    integer, dimension(:), allocatable :: set1_, uni_
    integer, dimension(:), allocatable :: set2_, inter_
    integer :: this_input
    

    open(unit=99, status="old", file="inputA.dat", iostat=ierr, iomsg=errmsg)

    if (ierr /= 0) then
        print *, "ERR: ", errmsg
        stop
    end if

    open(unit=98, status="old", file="inputB.dat", iostat=ierr, iomsg=errmsg)

    if (ierr /= 0) then
        print *, "ERR: ", errmsg
        stop
    end if

    print *, "Opened both files"

    read_set1: do 

        read(99,*, iostat=ierr, iomsg=errmsg) this_input
        ! print *, "Read statement called"

        if(ierr == 0) then
            !! good input

            if (allocated(set1_)) then
                set1_ = [set1_, this_input]
            else
                allocate(set1_(1))
                set1_ = this_input                
            end if

            cycle read_set1

        else if(ierr < 0) then
            print *, "Finished processing 'inputA.dat'"
            exit read_set1
        else
            print *, "ERR: ", errmsg
        end if

    end do read_set1

    read_set2: do 

        read(98,*, iostat=ierr, iomsg=errmsg) this_input

        if(ierr == 0) then
            !! good input

            if (allocated(set2_)) then
                set2_ = [set2_, this_input]
            else
                allocate(set2_(1))
                set2_ = this_input
            end if

            cycle read_set2
            
        else if(ierr < 0) then
            print *, "Finished processing 'inputA.dat'"
            exit read_set2
        else
            print *, "ERR: ", errmsg
        end if

    end do read_set2

    print *, "Set A read in as: ", set1_
    print *, "Set B read in as: ", set2_

    print *, "Does -11 belong to set A? ", belongs_to(-11, set1_)
    print *, "Does -11 belong to set B? ", belongs_to(-11, set2_)
    print *, "Does 11 belong to set A? ", belongs_to(11, set1_)
    print *, "Does 11 belong to set B? ", belongs_to(11, set2_)


    uni_ = union(set1_, set2_)
    inter_ = intersection(set1_, set2_)

    print *, "Union:        ", uni_
    print *, "Intersection: ", inter_




    close(unit=99)
    close(unit=98)


end program


