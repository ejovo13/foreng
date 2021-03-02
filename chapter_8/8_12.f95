! 1. Describe the problem

! As a part of a meteorological experiment, average annual temperature measurements were collected
! at 36 locations specified by latitude and longitude as shown in the chart on page 400 of 
! Fortran for Scientists and Engineers. Write a Fortran program that calculates the average
! annual temperature for all of the locations in the experiment, and that averages the temperaturs
! along the latitude and longitude

! 2. Define the inputs and outputs.

! The temperature data is stored in the file temp.dat, in ROW-MAJOR order. We want to create
! a matrix that is 6 x 6. We want the output to be 2 vectors and one number. One vector will store
! the average temperature across latitude, and another across longitude

program meteorology
! Purpose:
!   To calculate the average temperature given the data in temp.dat across latitude and longitude
!
implicit none

! Data dictionary
real, dimension(6), parameter :: LONG = [ 90.0, 90.5, 91.0, 91.5, 92.0, 92.5 ]      ! The longitude values found in the table on page 400
real, dimension(6), parameter :: LAT = [ 30.0, 30.5, 31.0, 31.5, 32.0, 32.5 ]       ! The latitude values found in the table on page 400
real, dimension(6,6) :: avg_temp_arr                                                ! The average temperature at each longitude and latitude
real, dimension(6) :: long_avg, lat_avg
real :: avg_all
integer :: i

open(unit=10, file='temp.dat', status='old', action='read')

read(10,*) (avg_temp_arr(i,:), i = 1,6)


print 221
print *, "Here are the contents of the temperature data:"
print 221
print 15, (avg_temp_arr(i, :), i = 1,6)
15 Format(6(F6.3, X))
print 16
16 Format(/,/)

! Now that the array is properly loaded, let's sum along the latitude and longitudes.

! Latitudes are the rows, longitudes are the colums
do i = 1,6
    long_avg(i) = sum(avg_temp_arr(:,i)) / 6.0
    lat_avg(i) = sum(avg_temp_arr(i,:)) / 6.0

end do

avg_all = sum(avg_temp_arr) / 36

! print *, "The averages along longitude (cols) lines are: ", long_avg
! print *, "The averages along latitutde (rows) lines are: ", lat_avg
! print *, "The average for all locations is: ", avg_all

! Format the printing of the longitude and the average value

print 111, LONG
print 112
print 113, long_avg

111 Format(20X, "Longitude |", 6(F7.3, " |"))
112 Format(20X, 65("-"))
113 Format(20X, "Avg temp  |", 6(F7.3, " |"))

! Format the printing of the latitude and average values

print 221
print 223
print 224
do i = 1,6
    print 222, LAT(i), lat_avg(i)
end do
print 221

221 Format(/)
222 Format(F8.3, " |", F7.3)
223 Format("Latitude | Avg temp ")
224 Format(18("-"))


end program



