! This program tests the basic ideas presented in chapter 13.

program testing123
implicit none

interface test

    subroutine test_i(int1)
        integer, intent(in) :: int1
    end subroutine
    subroutine test_r(r1)
        real, intent(in) :: r1
    end subroutine

end interface


call say_hello
call say_hello(1.0)
call say_hello(arg1=1.0)
call test(1)
call test(1.0)


contains

    subroutine say_hello(arg1)
        real, intent(in), optional :: arg1
        if (present(arg1)) then
            print *, "Arg1 = ", arg1
        else 
            print *, "Hello world"
        end if
    end subroutine

end program

subroutine test_i
    print *, "integer called"
end subroutine

subroutine test_r
    print *, "real called"
end subroutine