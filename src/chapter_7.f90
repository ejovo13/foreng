module chapter_7
!! All of the programming exercises for Chapter 7: Procedures
use iso_fortran_env
implicit none

contains

    subroutine p7_1()
    !! ** DISCUSSION QUESTION ** <br>
    !! What is the difference between a subroutine and a function?

        print *, "A subroutine can modify arguments that are passed to it, but it will never have"
        print *, "a return value. Functions in Fortran, just like their mathematical definition,"
        print *, "necessarily have a return value. A subroutine and function is similar with"
        print *, "respect to the fact that they both are a tool to repeat and reuse code."

    end subroutine

    subroutine p7_2()
    !! ** DISCUSSION QUESTION ** <br>
    !! When a subroutine is called, how is data passed from the calling program to the subroutine,
    !! and how are the results of the subroutine returned to the calling program?

        print *, "Parameters are passed around inside subroutines as references, that is to say"
        print *, "that there is not a block of memory that is copied to the subroutine, instead"
        print *, "the argument is passed with a pointer to the block of memory that stores the value."

    end subroutine

    subroutine p7_3()
    !! ** DISCUSSION QUESTION ** <br>
    !! What are the advantages and disadvantages of the pass-by-reference scheme used in Fortran?

        print *, "Advantages: "
        print *, " - Don't have to copy large amounts of data (which can be slow)"
        print *, "Disadvantages: "
        print *, " - Data address might be far away from the caller, leading to slow load times"
        print *, " - Modifiying the data in the subroutine can affect the original parameter"

    end subroutine

    subroutine p7_4()
    !! ** DISCUSSION QUESTION ** <br>
    !! What are the advantages and disadvantages of each procedure array type?

        print *, "Explicit shaped dummy arrays:"
        print *, " - Compiler knows exactly the shape and size of an argument, and"
        print *, "can therefore check for out-of-bounds errors during compilation"
        print *, " - We have to pass the size of the array when calling the procedure"
        print *
        print *, "Assumed-shape dummy arrays:"
        print *, " - Allow the compiler to check SOME indexing errors, and avoids having"
        print *, "to pass a size parameter"
        print *, " - can be used in full-matrix statements (e.g. A + B)"
        print *
        print *, "Assumed-size dummy arrays:"
        print *, " - Should NEVER be used because the compiler can't check for errors."

    end subroutine

    subroutine p7_5()
    !! ** DISCUSSION QUESTION **<br>
    !! Suppose a 15-element array is passed to a subroutine. What happens if the subroutine attempts
    !! to write to element a(16)?

        print *, "If the array is explicit-shaped, then there will be an error at compile time."
        print *, "If the array is assumed-shape, then there will be an error at run time."
        print *, "An assumed-size error will allow the unsafe access to be performed."

    end subroutine

    subroutine p7_6() 
    !! **DISCUSSION QUESTION** <br>
    !! If a real value is passed to a subroutine in an argument that is declared to be an integer,
    !! what happens?

        print *, "Sometimes the compiler will give a warning, but still allow the real to be passed"
        print *, "In this case, however, my gfortran compiler produced an error so I can't even pass"
        print *, "a real value into a subroutine expecting an integer."
    
        ! integer, intent(in) :: i
        ! write(*,*) " I = ", i    

    end subroutine

    subroutine p7_7()
    !! ** DISCUSSION QUESTION** <br>
    !! How can p7_6 be modified so that the compiler catches the argument mismatch between
    !! the actual argument and the dummy argument in the subroutine?


        print *, "We can ensure that an int will be passed to the subroutine by wrapping"
        print *, "the parameter with the INT(x) function upon calling"

    end subroutine

    subroutine p7_8()
    !! **DISCUSSION QUESTION** <br>
    !! What is the purpose of the INTENT attribute? Where can it be used? Why should it be used?
    
        print *, "The INTENT attribute signals to the compiler whether or not the variables are"
        print *, "read, write, or read write. This attribute is specified where the variables are"
        print *, "being declared in a procedure. They should be used so that you don't"
        print *, "accidentally alter a variable that you didn't want/expect to."

    end subroutine 

    subroutine p7_9()
    !! **NOT APPLICABLE**

    end subroutine

    subroutine p7_10()
    !! **NOT APPLICABLE**

    end subroutine











end module chapter_7