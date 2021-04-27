! Determine the contents of each variable in the following code fragment
! after the code has been executed. How does the behavior of this code gragment differ
! from the behavior of the one in Exercise 10-1?


program test_ten_two
    CHARACTER(len=16) :: a = '1234567890123456'
    CHARACTER(len=16) :: b = 'ABCDEFGHIJKLMNOP', c
    IF ( LGT(a,b) ) THEN
    c = a(1:6) // b(7:12) // a(13:16)
    ELSE
    c = b(7:12) // a(1:6) // a(13:16)
    END IF
    a(7:9) = '='

    print *, "a = ", a
    print *, "b = ", b
    print *, "c = ", c
    
end program

! The result is the exact same but the program calls the Lexical comparison operator rather
! than the regular <= for numeric comparisons.
