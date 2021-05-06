program test_stats
    
use foreng_math

implicit none

    integer(int32), dimension(8) :: x_i32 = [10, 12, 23, 23, 16, 23, 21, 16]
    integer(int64), dimension(8) :: x_i64 = [10, 12, 23, 23, 16, 23, 21, 16]
    real(real32), dimension(8) :: x_r32 = [10, 12, 23, 23, 16, 23, 21, 16]
    real(real64), dimension(8) :: x_r64 = [10, 12, 23, 23, 16, 23, 21, 16]

    print *, "std_dev(x_i32) = ", std_dev(x_i32)
    print *, "std_dev(x_i64) = ", std_dev(x_i64)
    print *, "std_dev(x_r32) = ", std_dev(x_r32)
    print *, "std_dev(x_r64) = ", std_dev(x_r64)
    

    
end program 

