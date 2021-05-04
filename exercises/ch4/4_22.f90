!! Fibonacci Numbers. Write a program to calculate and write out the nth Fibonacci number
!! where n > 2

!! Use a for loop


program fibonacci
    use iso_fortran_env
    implicit none

    integer(int64) :: fn !! nth number you wish to compute
    integer :: ierr
    character(100) :: errmsg

    read: do 

        print *, "Please enter a value of n to compute the nth Fibonacci number [0 to quit]"

        read(*,*, iostat=ierr, iomsg=errmsg) fn

        if (ierr /= 0) then
            print *, "ERR: ", errmsg
            print *, "Please enter a valid natural number"
            cycle read
        else if (fn == 0) then
            print *, "Exiting successfully"
            stop
        else 
            print*
            print 111, fn, fibonacci_loop(fn)
            111 format("The nth Fibonacci number (n = ", I0, "): ", I0)
            print*
            cycle
        end if

    end do read

contains 

    integer(int64) function fibonacci_loop(n) result(f_n)

        integer(int64), intent(in) :: n!! Nth fibonacci to compute
        
        integer(int64) :: f_n_1, f_n_2 !! Previous two fibonacci numbers
        integer(int64) :: i !! Looping index

        if (n == 1 .or. n == 2) then
            f_n = 1
            return
        else if (n > 2) then 

            f_n_1 = 1
            f_n_2 = 1

            do i = 3, n

                f_n = f_n_1 + f_n_2 !! Update current F number
                f_n_2 = f_n_1 
                f_n_1 = f_n !! Update previous two numbers

            end do            

        else 
            print *, "N must be a natural number"
        end if      

    end function


end program

