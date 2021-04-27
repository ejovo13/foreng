! A point can be located in a Cartesian plane by two coordinates. Create a 
! derived data type called "point" whose components are x and y. A line can
! be represented in a Cartesian plane by the equation
! y = mx + b

! Create a derived data type called "line" whose components are m and b

program line_test

    use ejovo_types

    type(line) :: l1
    l1 = line(10, 5)

    call l1%print

end program

! Done 