! For the Fortran statements and input given below, state what the values of each variable will be 
! when the READ me statement has been completed.

program read_me
    Character(5) :: a
    Character(10) :: b
    character(15) :: c

    open(unit=10, file='5_4.dat')
    READ(unit=10, fmt='(3A10)') a, b, c
    close(unit=10)

    print *, "a: ", a , " b: ", b, " c: ", c

end program
    
