module chapter_4
!! All of the programming exercises for Chapter 4: Loops and Character Manipulation
use iso_fortran_env
implicit none

contains 

    ! //TODO CLEAN UP OUTPUT OF ALL PROGRAMS
    subroutine p4_1()
    !! Test if the following equalities are valid statements, and if they are,
    !! evaluate them

        logical :: a, b
        character(5) :: c, d
        a = '123' > 'abc'
        ! b = '9478' == 9478 

        c = ACHAR(65) // ACHAR(95) // ACHAR(72)
        d = ACHAR(IACHAR('j') + 5)

        print *, "a: ", a, "b: ", b,  "c: ", c, "d: ", d

    end subroutine

    subroutine p4_3()
    !! Write a Fortran program to evaluate the equation y(x) = x^2 - 3x + 2 for all values between -1 and 3,
    !! in steps of 0.1.
    ! // TODO add latex support
        ! Puprose: to solve exercise 4.3
        ! Data dictionary :: declared variables 

        integer, parameter :: start_index = -1, end_index = 3
        real, parameter :: step_size = 0.1
        integer, parameter :: array_size = int((end_index - start_index)/step_size)
        integer :: i
        real, dimension(array_size) :: output = [(0.1*i + -1, i = 0,(array_size - 1))]
        integer :: x

        do x = 1,array_size
            output(x) = x**2 - 3*x + 2
        end do
        write(*,100) output
        100 format(ES9.2)

    end subroutine

    subroutine p4_4()    
    !! Write the Fortran statements required to calculate y(t) from the equation:
    !! 
    !!   y(t) = ------ -3t^2 + 5    t >= 0
    !!           \---   3t^2 + 5    t < 0
    !! 
    ! //TODO print function to stdin
        real :: y, t

        print *, "Please enter a t value that you would like to compute"
        read *, t

        if (t >= 0) then
            y = -3*(t**2) + 5
        else 
            y = 3*(t**2) + 5
        end if 

        print 100, "y(t) with t = ", t, " is equal to ", y
        100 format(A, ES9.2, A, ES9.2)

    end subroutine 

    subroutine p4_5()
    !! Write a Fortran program to calculate the factorial function. Be sure to handle the special 
    !! cases of 0! and of illegal input values.
        
        ! integer, parameter :: MY_INT_64 = selected_int_kind(38)
        integer(int64) :: input, factorial, i = 1,  ierr
        ! logical :: value_accepted = .false.
    
        print *, "Please enter an integer to find it's factorial, max value 33!"
        read (*, '(i10)', iostat=ierr) input
    
        do 
            if(ierr == 0) then
                exit 
            end if 
    
            print *, "Value not accepted, please enter an integer"
            read (*, '(i2)', iostat=ierr) input
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
    
        print 99, input, factorial
        99 format (I0, " factorial is: ", I0)

    end subroutine

    subroutine p4_6()
    !! What is the difference in behavior between a cycle statement and an exit statement?

        print *, "A cycle statement sends the program control to the start of the loop, whereas the exit statement"
        print *, "exits the loop entirely"
    
    end subroutine

    subroutine p4_7()
    !! Modify program stats_2 to use the DO WHILE construct instaed of the while construct currently in the program
    !!
    !!     A modified statistical analysis program that avoids the divide-by-zero problems inherent in
    !! program stats_1.
    !!
    !! Purpose:
    !!
    !! To calculate mean and the standard deviation of an input
    !!
    !!  data set containing an arbitrary number of input values.
    !!
    !! Record of revisions:
    !!
    !!  Date      Programmer     Description of change
    !!  ====      ==========     =====================
    !! 11/10/15  S. J. Chapman   Original code
    !! 11/12/15  S. J. Chapman   Correct divide-by-0 error if
    !!                           0 or 1 input values given.
    !! 02/16/21  E. J. Voyles    Add do-while construct
    !!
        IMPLICIT NONE
        ! Data dictionary: declare variable types, definitions, & units
        INTEGER :: n = 0
        ! The number of input samples.
        REAL :: std_dev = 0. ! The standard deviation of the input samples.
        REAL :: sum_x = 0.
        ! The sum of the input values.
        REAL :: sum_x2 = 0. ! The sum of the squares of the input values.
        REAL :: x = 0.
        ! An input data value.
        REAL :: x_bar
        ! The average of the input samples.
        ! While Loop to read input values.
        DO
        ! Read in next value
        WRITE (*,*) 'Enter number: '
        READ (*,*) x
        WRITE (*,*) 'The number is ', x
        ! Test for loop exit
        IF ( x < 0 ) EXIT
            ! Otherwise, accumulate sums.
            n = n + 1
            sum_x = sum_x + x
            sum_x2 = sum_x2 + x**2
        END DO
        ! Check to see if we have enough input data.
        IF ( n < 2 ) THEN ! Insufficient information
            WRITE (*,*) 'At least 2 values must be entered!'
        ELSE ! There is enough information, so
        ! calculate the mean and standard deviation
            x_bar = sum_x / real(n)
            std_dev = sqrt( (real(n) * sum_x2 - sum_x**2) / (real(n)*real(n-1)))
            ! Tell user.
            WRITE (*,*) 'The mean of this data set is:', x_bar
            WRITE (*,*) 'The standard deviation is: ', std_dev
            WRITE (*,*) 'The number of data points is:', n
        END IF
    end subroutine

    subroutine p4_8()    
    !! Examine the following DO statements and determine how many times each loop will
    !! be exectued.

        implicit NONE
        integer :: irange, j, kount, i !, index
        integer :: count = 0

        a: do irange = -32768, 32767
            count = count + 1
        end do a

        print *, "(a) = ", count
        count = 0

        b: do j = 100, 1, -10
            count = count + 1
        end do b

        print *, "(b) = ", count

        count = 0
        c: do kount = 2, 3, 4
            count = count + 1
        end do c

        print *, "(c) = ", count
        count = 0

        ! d: do index = -4,-7
        !     count = count + 1
        ! end do d

        print *, "(d) = ", count
        count = 0
        
        e: do i = -10, 10, 10
            count = count + 1
        end do e

        print *, "(e) = ", count
        count = 0

        ! f: do i = 10, -2, 0
        !     count = count + 1
        ! end do f

        !!!!! This for loop shows an error, since the increment is 0 !!!!!!!!!!

        ! print *, "(f) = ", count
        ! count = 0

    end subroutine

    subroutine p4_9()
    !! Examine the following loops, determine the value of ires and the number of times
    !! each loop executes

        integer :: ires

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

    end subroutine

    subroutine p4_10()
    !! Examine the followin while loops and determine the value of ires at the end of each of 
    !! the loops.

        integer :: ires, count
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

    end subroutine

    subroutine p4_11()    
    !! Modify program ball from example 4-7 to read in the acceleration due to gravity
    !! at a particular location, and to calculate the maximum range of the ball for that acceleration.
    !! After modifying the program, run it with accelerations of -9.8, -9.7, and -9.6m/s2.
    !! 
    !! What effect does the reduction in gravitational attraction have on the range ofthe ball?
    !! What effect does the reduction in gravitational attraction have on the best angle theta at which to throw the ball?
    
        ! Purpose:
        !
        !       To test how the flight of a ball changes in different gravitational fields
        !
        !
        ! Data Dictionary: Declare constants and variables
        implicit none
        real :: g                                       ! The gravitational constant, g. This value will be read in at the start of every execution
        real :: vx                                      ! The horizontal velocity of the ball
        real :: vy                                      ! The vertical velocity of the ball
        real :: v0 = 20                                 ! The initial velocity of the ball
        real :: tf                                      ! Time when the ball lands
        real :: max_distance = 0                        ! Maximum distance traveled
        integer :: theta = 0                            ! Angle at which the ball is thrown
        integer :: max_theta                            ! The theta for which the ball travels the most distance
        real, dimension(0:90) :: distance_traveled      ! The distance traveled for each theta value
        character(50) :: FMT                            ! The format of the output table
        real, parameter :: PI = 3.1415927               ! Mathematical constant pi
        integer :: ierr                                 ! flag for reading in the gravitational value
        character(50) :: errmsg                         ! the error message for reagding in
        
        FMT = "(I3, A, F5.2, A, F6.2, A, F5.2)"
        
        100 print *, "Please enter a real, negative value for the gravitational constant"
        read (*,*, iostat = ierr, iomsg = errmsg) g
        if (ierr /= 0) then
            print *, "ierr = ", ierr, " iomsg = ", errmsg
            goto 100
        end if 
        
        print *, "Theta | time in air(s) | distance traveled(m) | vy"
        print *, "--------------------------------------------------"
        
        do theta = 0, 90, 1
            vx = v0 * cos((theta/180.)*PI)
            vy = v0 * sin((theta/180.)*PI)
            ! Find tf
            tf = vy/(-g/2)
        
            distance_traveled(theta) = tf * vx
        
            if (distance_traveled(theta) > max_distance) then
                max_distance = distance_traveled(theta)
                max_theta = theta
            end if 
        
            print (FMT), theta, " | ", tf, " | ", distance_traveled(theta), " | ", vy
        
        end do
        
        print *, "Max distance traveled: ", max_distance, "With theta: ", max_theta

    end subroutine

    subroutine p4_12()
    !! Modify program ball from Example 4-7 to read in the initial velocity with which the ball
    !! is thrown. After modifying the program, run it with initial velocities of 10, 20, and 20 
    !! m/sec. What effect does changin the initial velocity have on the range of the ball? What
    !! effect does it have on the best angle theta at which to throw the ball?

        ! Purpose:
        !
        !       To test how the flight of a ball changes with different initial velocites
        !
        !
        ! Data Dictionary: Declare constants and variables
        implicit none
        real :: g = -9.8                                ! The gravitational constant, g. 
        real :: vx                                      ! The horizontal velocity of the ball, m/sec
        real :: vy                                      ! The vertical velocity of the ball
        real :: v0                                      ! The initial velocity of the ball, this will be read in upon execution
        real :: tf                                      ! Time when the ball lands
        real :: max_distance = 0                        ! Maximum distance traveled
        integer :: theta = 0                            ! Angle at which the ball is thrown
        integer :: max_theta                            ! The theta for which the ball travels the most distance
        real, dimension(0:90) :: distance_traveled      ! The distance traveled for each theta value
        character(50) :: FMT                            ! The format of the output table
        real, parameter :: PI = 3.1415927               ! Mathematical constant pi
        integer :: ierr                                 ! flag for reading in the gravitational value
        character(50) :: errmsg                         ! the error message for reagding in
        
        FMT = "(I3, A, F5.2, A, F6.2, A, F5.2)"
        
        100 print *, "Please enter a real value for the initial velocity"
        read (*,*, iostat = ierr, iomsg = errmsg) v0
        if (ierr /= 0) then
            print *, "ierr = ", ierr, " iomsg = ", errmsg
            goto 100
        end if 
        
        print *, "Theta | time in air(s) | distance traveled(m) | vy"
        print *, "--------------------------------------------------"
        
        do theta = 0, 90, 1
            vx = v0 * cos((theta/180.)*PI)
            vy = v0 * sin((theta/180.)*PI)
            ! Find tf
            tf = vy/(-g/2)
        
            distance_traveled(theta) = tf * vx
        
            if (distance_traveled(theta) > max_distance) then
                max_distance = distance_traveled(theta)
                max_theta = theta
            end if 
        
            print (FMT), theta, " | ", tf, " | ", distance_traveled(theta), " | ", vy
        
        end do
        
        print *, "Max distance traveled: ", max_distance, "With theta: ", max_theta

    end subroutine

    subroutine p4_13()

        print *, "Work in progress"

    end subroutine

    subroutine p4_14()
    !! Write a Fortran program to evaluate the function 
    !! y(x) = ln(1/1-x)
    !! for any user-specified value of x, where ln is the natural logarithm.
    !! Write the program with a while loop, so that the program repeats the calculation 
    !! for each legal value of x entered into the program. When an illegal value of x is entered,
    !! terminate the program. 

        !   Purpose:
        !       This program calculates the value of ln(1/(1-x)) for all valid
        !       values of x entered by the user
        !
        !
        IMPLICIT NONE
        
        ! Data dictionary: declare variable types, definitions, & units.
        real :: x                   ! The x value to be evaluated
        integer :: ierr = 0         ! The error status when reading in the value
        character(100) :: errmsg    ! The error message generated when reading in a value
        
        
        do while(ierr == 0)
            print *, "Please enter a real number greater than 1"
            read (*,*, iostat = ierr, iomsg = errmsg) x
            if (x <= 1 .or. ierr /= 0) exit
            print 100, x, log((1)/(x - 1))
        end do
        
        100 Format("f(", f10.2, ") = ", f7.3)

    end subroutine

    subroutine p4_15()
    !! Write a Fortran program to convert all uppercase characters in a user-supplied character
    !! string to lowercase, without changing the uppercase and nonalphabetic characters in the string.
    !! Assume that your computer uses the ASCII collating sequence.

        !   Purpose:
        !       This program converts all uppercase letters to lowercase letters
        IMPLICIT NONE
        
        ! Data Dictionary: declare variable types, definitions, and units
        integer, parameter :: MAX_LENGTH = 30
        character(len=MAX_LENGTH) :: start_string
        character(MAX_LENGTH) :: end_string
        integer :: i
        integer :: this_char_index
        character :: this_char
        
        print *, "Please enter a string you would like to convert to lowercase"
        read '(30A)', start_string
        
        do i = 1, MAX_LENGTH 
            this_char = start_string(i:i)
            this_char_index = iachar(this_char)
        
            if (this_char_index >= 65 .and. this_char_index <= 90) then
                end_string(i:i) = achar(this_char_index + 32)
            else
                end_string(i:i) = this_char
            end if
        
        end do
        
        print *, "Converted string: ", start_string, " to string: ", end_string    

    end subroutine

    subroutine p4_16()
    !! Calculating Orbits. 
    !!
    !! When a satellinte orbits the Eath, the satellite's orbit will form an ellipse with the 
    !! Earth located at one of the focal points of the ellipse. The satellite's orbit can be 
    !! expressed in polar coordintaes as
    !!
    !! r = p/(1 - \epsilon * cos( \theta ))
    !!
    !! Where r and theta are the distance and angle of the satellite from the center of the earth,
    !! p is a parameter specifying the size of the orbit, and \epsilon is a parameter representing the
    !! eccentricity of the orbit. A circular orbit has an eccentricity of zero. An elliptical orbit has 
    !! an eccentricity of 0 <= e <= 1. If \epsilon > 1, the satellite follows a hyperbolic path and escapes
    !! from Earth's gravitational field.
    !!
    !! Consider a satellite with a size parameter p = 1200 km. Write a program to calculate the 
    !! distance of the satellite from the center of the Earth as a function of theta if the satellite has 
    !! an eccentricity of (a) e = 0; (b) e = 0.25; (c) e = 0.5. Write a single program in which
    !! r and e are both input values. 
    !!
    !! How close does each orbit come to the earth? How far away does each orbit get from the earth?

        !   Purpose:
        !      To calculate the position of a satellite orbiting the Earth.
        
        ! Data dictionary: declare variable types, definitions, and units
        integer, parameter :: P = 1200                      ! Size of the orbit, in km.
        integer, parameter :: NTHETA = 200
        real, parameter :: PI = 3.1415927
        real, parameter :: THETA_STEP = (2*PI)/NTHETA
        real :: eccentricity                                ! Eccentricity of the orbit, 0 <= e <= 1
        real :: theta                                       ! Polar coordinate for the satellite.
        real :: r                                           ! Distance from the center of the earth.
        real :: apoapsis = P                                ! Farthest distance from the center of the earth
        real :: periapsis = P                               ! Closest distance from the center of the earth
        integer :: ierr
        character(30) :: errmsg
        integer itheta
        
        
        print *, "Please enter an eccentricity value, between 0 and 1."
        read (*,*, iostat=ierr, iomsg=errmsg) eccentricity
        
        do while (ierr /= 0 .and. (eccentricity > 1 .or. eccentricity < 0)) 
            print *, "Input invalid, please enter a real value between 0 and 1."
            read (*,*, iostat=ierr, iomsg=errmsg) eccentricity
        end do
        
        do itheta = 1, NTHETA
            theta = itheta*THETA_STEP
            r = p/(1 - (eccentricity * cos(theta)))
            if (r < periapsis) periapsis = r
            if (r > apoapsis) apoapsis = r
            print *, theta, r
        end do
        
        print *, "The periapsis is: ", periapsis, "km, and the apoapsis is: ", apoapsis, "km"

    end subroutine

    subroutine p4_17()
    !! Write a program caps that reads in a character string, searches for all of the words 
    !! within the string, and capitalizes the first letter of each word, while shifting the 
    !! remainder of the word to lowercase. Assume that all nonalphabetic and nonumeric characters
    !! can mark the boundaries of a word within the character variable (periods, commas,etc.). 
    !! Nonalphabetic characters should be left unchanged.

        ! 2. Define the input and output

        ! input: A character string from the terminal.
        ! output: The same string with the only the first letter per word capitalized.

        ! 3. Describe the algorithm

        ! We should first read in the string
        ! Alphabetic character ranges:
        !   lowercase: [97, 122]
        !   uppercase: [65, 90]
        ! Numeric character ranges:
        !   [48, 57]
        !
        ! We assume that all nonalphabetic and nonnumeric characters can mark the boundaries of a word.

        ! Purpose:
        !   To capitalize the first letter per word in a character string.
        
        ! Data dictionary:
        integer, parameter :: MAX_STR_LEN = 500
        character(:), allocatable :: input_str
        character(30) :: FMT
        integer :: i
        integer :: num_chars
        character(1) :: prev_char, this_char
        
        print *, "Please enter a the number of characters to record"
        read (*,"(I10)") num_chars
        
        
        allocate(character(num_chars) :: input_str)
        write(FMT, '("(A", I0, ")")') num_chars
        
        ! print *, "Format statement saved as: ", FMT
        
        print *, "Now please enter the string you would like to transform"
        read (*,FMT) input_str
        
        print *, "String before transformation: ", input_str
        
        if (is_alph(input_str(1:))) then
            call to_upper(input_str(1:))
        end if
        
        do i = 2,MAX_STR_LEN
            prev_char = input_str(i-1:)
            this_char = input_str(i:)
        
            ! If the character is the start of the word, then capitalize it
            if (.not. is_alphnum(prev_char)) then   ! If the previous character is a word boundary
                if(is_lower(this_char)) then        ! And this character is lowercase
                    call to_upper(input_str(i:))    ! Then capitalize it
                end if
            else
                if(is_upper(this_char)) then
                    call to_lower(input_str(i:))
                end if
            end if
        end do
        
        print *, "String after transformation: ", input_str

    end subroutine

    subroutine p4_18()
    !!Write a computer program to calculate the current flowing through this diode
    !! for all voltages from -1.0V to +0.6 V, in 0.1 V steps. Repeat this process for the 
    !! following temperatures: 75F, 100F, and 125F

        ! Purpose: 
        !   To calculate the current flowing through a semiconductor diode
    
    
        real(real64), dimension(3) :: T_f = [75, 100, 125]
        real(real64), dimension(3) :: T_k
        real(real64), parameter :: I_O = 2.0E-6
        real(real64), dimension(17) :: v_D
        real(real64), dimension(3,17) :: i_D
        integer :: i, j
    
        do i = 1,17
            v_D(i) = -1.0 + (0.1 * (i-1))
        end do
        
        ! print *, v_D
    
    
        T_k = to_kelvin(T_f)
    
        print *, " "
        print *, "Calculating the current flowing through a semiconductor diode for three separate temperatures"
        print *, "and voltages ranging from -1.0 to 0.6 V"
        print *, " "
    
        print 999, v_D
        print 100
    
        100 format(250("-"))
    
        do i = 1,3
            do j = 1,17
                i_D(i,j) = calc_current_flow(v_D(j), I_O, T_k(i))            
            end do
            print 99, T_k(i), i_D(i,:)
            99 format(F9.3, 2X, " | ", 17(ES10.3, 3X))
            999 format("  T(K) \ V ", " | ", 17(F6.3, 7X))
        end do

    end subroutine

    subroutine p4_19()
    !! Write a program that promps a user for a binary number, which will be entered as a string
    !! of 0s and 1s in a character variable. This program should be able to handle numbers whos
    !! binary representation is up to 10 bits, corresponding to values from 0 to 1023.

        ! Purpose:
        !   To convert binary numbers to decimal.
        implicit none
        
        ! Data dictionary
        integer, parameter :: MAX_LEN = 10
        integer, parameter :: ZERO_ASCII = 48
        integer, parameter :: ONE_ASCII = 49
        character(MAX_LEN) :: binary_representation
        character(:), allocatable :: binary_trimmed
        integer :: len_trim = 0
        logical :: input_is_valid = .true.
        integer :: i = 0
        character(1) :: this_char = ''
        integer :: char_value = 0
        real :: decimal_representation = 0
        integer :: this_bit = 0
        
        ! print *, "decimal_representation = ", decimal_representation
        
        print *, "Please enter a binary number that is between 1 and 10 bits"
        read (*,"(A10)") binary_representation
        
        
        ! print *, "Converting binary number: ", binary_representation
        binary_trimmed = trim(binary_representation)
        len_trim = len(binary_trimmed)
        
        ! First let's validate the input
        do i = 1,len_trim
            this_char = binary_trimmed(i:)
            char_value = ichar(this_char)
            if (char_value /= ZERO_ASCII .and. char_value /= ONE_ASCII) then
                input_is_valid = .false.
                print *, "invalid char = ", this_char
                print *, "invalid val = ", char_value
                print *, "invalid pos = ", i
        
                print *, ""
                print *, "ZERO_ASCII = ", ZERO_ASCII
                print *, "ONE_ASCII = ", ONE_ASCII
                exit
            end if
        end do
        
        if (input_is_valid) then
            ! print *, "input is valid, converting now"
        
            do i = 1,len_trim
                read(binary_representation(i:),"(I1)") this_bit
                ! print *, "Current working bit: ", this_bit
                decimal_representation = (this_bit * 2 ** (len_trim - i)) + decimal_representation
            end do
        else
            print *, "input is invalid, please try restarting the program"
        end if
        
        print 99,  binary_representation, int(decimal_representation)
        99 format("binary num: ", T15, A10, /, 'decimal:', T15, I0 )

        decimal_representation = 0

    end subroutine

    !-------------------------------------!
    !------- HELPER FUNCTIONS ------------!
    !-------------------------------------!
    logical function is_alphnum(char)
    !! Test whether an ASCII character is alpha-numeric
        character(1), intent(in) :: char

        if(is_num(char) .or. is_alph(char)) then
            is_alphnum = .true.
        else 
            is_alphnum = .false.
        end if

    end function


    logical function is_alph(char)
    !! Test whether an ASCII character is a letter
        character(1), intent(in) :: char
        
        if(is_upper(char) .or. is_lower(char)) then
            is_alph = .true.
        else 
            is_alph = .false.
        end if

    end function

    logical function is_num(char)
    !! Test whether an ASCII character is numeric

        character(1), intent(in) :: char
        integer :: char_value

        char_value = iachar(char)
        if(char_value >= 48 .and. char_value <= 57) then
            is_num = .true.
        else
            is_num = .false.
        end if 

    end function

    logical function is_lower(char)
    !! Test whether an ASCII character is a lowercase letter
        character(1), intent(in) :: char
        integer :: char_value !! ASCII value

        char_value = iachar(char)
        if(char_value >= 97 .and. char_value <= 122) then
            is_lower = .true.
        else
            is_lower = .false.
        end if      

    end function

    logical function is_upper(char)
    !! Test whether an ASCII character is an uppercase letter 

        character(1), intent(in) :: char
        integer :: char_value !! ASCII value

        char_value = iachar(char)
        if(char_value >= 65 .and. char_value <= 90) then
            is_upper = .true.
        else
            is_upper = .false.
        end if        

    end function

    subroutine to_lower(char)
    !! Convert a single character to lowercase

        character(1), intent(inout) :: char

        if (is_upper(char)) char = achar((iachar(char) + 32))        

    end subroutine

    subroutine to_upper(char)
    !! Convert a single character to uppercase

        character(1), intent(inout) :: char 

        if (is_lower(char)) char = achar((iachar(char) - 32)) 

    end subroutine

    elemental function calc_current_flow(v_D, I_O, T) result(i_D)
    ! Data Dictionary
    ! // TODO add proper documentation
        real(real64) :: i_D                              !! The current flow through the diode in amperes
        real(real64), intent(in) :: v_D                  !! The voltage across the diode, in volts
        real(real64), intent(in) :: I_O                  !! The leakage current of the diode, in amperes
        real(real64), intent(in) :: T                    !! Temperature, in kelvins (K)
        real(real64), parameter :: q = 1.602E-19         !! The charge on an electron, 1.602 X 10-19 Coulombs
        real(real64), parameter :: k = 1.38E-23          !! Boltzmann's constant, 1.38 X 10-23 J/K
        
        i_D = I_O * (exp((q * v_D)/(k*T)) - 1)

    end function

    elemental real(real64) function to_kelvin(F)  
    !! Convert a temperature in Fahrenheit to Kelvin
        real(real64), intent(in) :: F
        to_kelvin = to_celcius(F) + 273
    end function

    elemental real(real64) function to_celcius(F)
    !! Convert a temperature from Fahrenheit to celcius
    real(real64), intent(in) :: F
        to_celcius = (F - 32) * (5./9.)
    end function

end module