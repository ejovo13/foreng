! Examine the following loops, determine the value of ires and the number of times
! each loop executes

program loop_tests

    ! (b)
    ! ires = 0
    ! loop1: DO index1 = 1, 20, 5
    !     IF ( index1 <= 10 ) CYCLE
    !     loop2: DO index2 = index1, 20, 5
    !         ires = ires + index2
    !     END DO loop2
    ! END DO loop1

    ! (c)
    ! ires = 0
    ! loop1: do index1 = 10, 4, -2
    !     loop2: do index2 = 2, index1, 2
    !         if (index2 > 6) exit loop2
    !         ires = ires + index2
    !     end do loop2
    ! end do loop1

    ! (d)
    ! ires = 0
    ! loop1: do index1 = 10, 4, -2
    !     loop2: do index2 = 2, index1, 2
    !         if (index2 > 6) exit loop1
    !         ires = ires + index2
    !     end do loop2
    ! end do loop1

    !! answers
    !! (b) ires = 43
    !! (c) ires = 42
    !! (d) loop2 is run 3 times, loop1 exits during it's first iteration, ires = 12

    print *, "Ires = ", ires
end program