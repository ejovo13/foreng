! Write a subroutine that uses subroutine random0 to generate a random
! number in the range [-1.0, 1.0).

program random_sub_test
    use ran001
    implicit NONE
    real :: test_num

!    call seed(100)
    call random0(test_num)

    print *, "The random number is:", test_num

end program

    