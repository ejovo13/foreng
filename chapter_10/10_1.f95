! Determine the contents of each variable in the following code fragement

program test_code

    CHARACTER(len=16) :: a =    '1234567890123456'   
    CHARACTER(len=16) :: b =    'ABCDEFGHIJKLMNOP', c
    IF ( a > b ) THEN
    c = a(1:6) // b(7:12) // a(13:16)
    ELSE
    c = b(7:12) // a(1:6) // a(13:16)
    END IF
    a(7:9) = '='

    print *, "a = ", a
    print *, "b = ", b
    print *, "c = ", c

    ! The first value of 1 (49) is less than A (93)
end program
