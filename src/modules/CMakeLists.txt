add_library(ch7 SHARED ejovo.f90 rand001.f90)
add_library(trig SHARED trig.f90 trig_degrees.f90 hyperbolics.f90)

add_library(ch9 SHARED
    prototype.f90 
    prototype@least_squares.f90 
    prototype@print_functions.f90 
    my_matmul.f90
    my_matmul_explicit.f90
    my_matmul_sub.f90
    my_matmul_mod_parent.f90
    ) #trig.f90 trig_degrees.f90 trig_conversion.f90)