! Determine which one of the valid Fortran Programs are valid.
! For each valid statement, specify what will happen in the program.

! (a)
! The statements are valid, and I believe that the program takes the transpose of the matrix.

! (b)
! This program syntax is valid. This program declares a new 1D array with extent 9, then for any value that is greater than
! zero, the program assigns that index the negative of the element. Any value that is below 0 will be multiplied
! by 3.

! (c)
! This program is also valid. This program writes to stdout the logical value that is the result of 
! checking the array info whose elements are less than or equal to 0.

! (d)
! I believe that this syntax is valid. This program will create a 4 by 4 array, initialize all of the 
! elements to 0, then assign each element (i,j) the value (i-j)

program main
    Integer, dimension(4,4) :: z = 0

    forall (i = 1:4, j = 1:4)
        z(i,j) = abs(i-j)
    end forall

    print *, z
end program