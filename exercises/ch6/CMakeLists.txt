set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${MY_EX}/ch6)

make_directory(${MY_EX}/ch6)

add_library(ch6_points ch6_points.f90)
add_library(ch6_sets ch6_sets.f90)

add_executable(6_6 6_6.f90)
add_executable(6_8 6_8.f90)
add_executable(6_10 6_10.f90)
add_executable(6_11 6_11.f90)
add_executable(6_12 6_12.f90)
add_executable(6_13 6_13.f90)
add_executable(6_14 6_14.f90)
add_executable(6_15 6_15.f90)
add_executable(6_16 6_16.f90)
add_executable(6_17 6_17.f90)
add_executable(6_19 6_19.f90)
add_executable(6_20 6_20.f90)

target_link_libraries(6_12 ch9)
target_link_libraries(6_13 ch9)
target_link_libraries(6_16 ch6_points)
target_link_libraries(6_17 ch6_points)
target_link_libraries(6_19 ch6_sets)
target_link_libraries(6_20 ch6_points)

configure_file(inputA.dat ${MY_EX}/ch6 COPYONLY)
configure_file(inputB.dat ${MY_EX}/ch6 COPYONLY)