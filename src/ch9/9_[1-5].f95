! 9-1
! What are the advatnages and disadvantages of using explicit-shape dummy arrays in procedures?
! What are the advantages and disadvantages of using assumed-shape dummy arrays?
! Why should assumed-size dummy arrays never be used?

! Explicit-shape arrays are dope because we have all the information required about an array to 
! effectively manipulate it. The disadvantage is that we have to pass this information to the 
! function

! Assumed-shape arrays are nice because we don't have to pass the size to the function and the 
! compiler can still add arrays and gather information about the bounds of the array. The downside
! is that we still have to assume the shape of the array.

! Assumed-size arrays can't even have their bounds checked by the compiler, so that's a no-go.


! 9-2
! What are the differences between internal procedures and external procedures?
! When should an internal procedure be used instead of an external procdure?

! Internal procedures are defined in a program/procedure, and they can ONLY be used inside of that program. They can't
! be used by any other translation unit. External procedures can be used by any translation unit.
! An internal procedure should be used to carry out low-level tinkering that is specific only to the program in which
! it is defined.

! 9-3 
! What is the purpose of the SAVE attribute? When should they be used?

! The SAVE attribute is similar to the static attribute in other languages. It allows data to persist between
! function calls. They should be used when data needs to not be wiped out. Perhaps we would like to keep track of the
! number of times that a function is called, internally.

! 9-4 
! Is the following program correct or not? If it is correct, what is printed out when it executes?
! If not, what is wrong with it?

! I believe that you can't pass the name of the program as a dummy argument, but I could be wrong.

! 9-5
! What is printed out when the following code is executed? What are the values of x,y,i, and j
! at each point in the program? If a value changes during the course of execution, explain why 
! it changes.

! Before calling the internal function exec, x = 12.000, y = -3.000, i = 6, j = 4.
! Inside the function call, the value of x is set to y, so: x = -3.000, y = -3.000, i = 6, and j = 6