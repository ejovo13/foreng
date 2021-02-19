! What are the advantages and disadvantages of the pass-by-reference scheme used in Fortran.

! Advantage: Don't have to pass around large amounts of data (which can be slow)
! 
! Disadvantage: - the data address might be far away from where the program is executing, this can 
!               slow loading times of the data
!               - modifying the data in the subroutine will affect the original parameter.