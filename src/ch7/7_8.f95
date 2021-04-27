! What is the purpose of the INTENT attribute? Where can it be used? Why should it be used?

! The INTENT attribute signals to the compiler whether or not the variables are read, write, or read write.
! This attribute is specified where the variables are being declared in a procedure. They should be used
! so that you don't accidentally alter a variable that you didn't want/expect to.