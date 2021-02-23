! Given a 5x5 array my_array containing the values shown below, determine the shape and contents  
! of each of the following array sections

! (a) my_array(3,:)
! This array is the 3rd row of the matrix, containing the elements: [ 11 12 13 14 15 ]

! (b) my_array(:,2)
! This array contains the 2nd column of the matrix, which is: 
! 2
! 7
! 12
! 17
! 22

! (c) my_array(1:5:2, :)
! This array contains the columns 1, 3, and 5 of my_array.
! 1  3  5
! 6  8  10
! 11 13 15
! 16 18 20
! 21 23 25

! (d) my_array(:,2:5:2)
! This array contains rows 2 and 4 of my_array

! (e) my_array(1:5:2, 1:5:2)
! This array contains a 3 by 3 array containing row entries 1,3,5 and column entries 1,3,5
! Scheme:
! 1,1 1,3 1,5
! 3,1 3,3 3,5
! 5,1 5,3 5,5

! (f) integer, dimension(3) :: list = [ 1, 2, 4]
! my_array(:,list)
! This array is 5 x 3, and contains the columns 1, 2, and 4

