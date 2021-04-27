! Examine the followin while loops and determine the value of ires at the end of each of 
! the loops.

program loop_test_2
    ! (a)
    !  ires = 0
    ! loop1: DO
    !     ires = ires + 1
    !     IF ( (ires / 10 ) * 10 == ires ) EXIT
    ! END DO loop1
    ! (b) 
    ! count = 0
    ! ires = 2
    ! loop2: DO
    !     ires = ires**2
    !     count = count + 1
    !     IF ( ires > 200 ) EXIT
    ! END DO loop2
    ! ! (c) 
    ! count = 0
    ! ires = 2
    ! DO WHILE ( ires > 200 )
    ! ires = ires**2
    ! count = count + 1
    ! END DO

    print *, "ires = ",ires, " count = ", count

end program
