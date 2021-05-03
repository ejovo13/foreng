set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${MY_EX}/ch5)
make_directory(${MY_EX}/ch5)

add_executable(5_1 5_1.f90)
add_executable(5_3 5_3.f90)
add_executable(5_4 5_4.f90)
add_executable(5_5 5_5.f90)
add_executable(5_6 5_6.f90)

configure_file(5_4.dat ${MY_EX}/ch5 COPYONLY)
configure_file(5_5.inp ${MY_EX}/ch5 COPYONLY)
