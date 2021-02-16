program four_five
    ! Write a Fortran program to calculate the factorial function. Be sure to handle the special 
    ! cases of 0! and of illegal input values.

    implicit none
    integer(16) :: input, factorial, i = 1,  ierr
    ! logical :: value_accepted = .false.

    print *, "Please enter an integer to find it's factorial, max value 33!"
    read (*, '(i10)', iostat=ierr) input

    do 
        if(ierr == 0) then
            exit 
        end if 

        print *, "Value not accepted, please enter an integer"
        read (*, '(i10)', iostat=ierr) input
    end do



    if (input == 0) then
        factorial = 1
    end if 
    do i = 1,input
        if (i == 1) then
            factorial = 1
        else
            factorial = factorial * i
        end if 
        if (factorial < 0) then
            error stop "Overflow error, input value too large"
        end if 
    end do

    print *, input, " factorial is: ", factorial
end program
