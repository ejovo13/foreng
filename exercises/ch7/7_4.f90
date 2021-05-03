! What are the advantages and disadvantages of using explicit-shape dummy arrays in procedures?
! What are the advantages and disadvantages of using assumed-shape dummy arrays?
! Why should assumed-size dummy arrays never be used?

! With explicit-shape dummy arrays, the compiler knows exactly the size and shape of the argument
! and can check for out-of-bounds errors during compiling time. The disadvantage is that we have
! to pass the size of the array when calling the procedure.

! Assumed-shape dummy arrays allow the compiler to check some shape indexing errors, and allows 
! a variable sized array to be used. This allows assumed-shape arrays to be used in full-matrix statements.

! Assumed-size dummy arrays should never be used because the compiler can't check for any errors.