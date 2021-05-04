set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${MY_EX}/ch6)

add_executable(6_6 6_6.f90)
add_executable(6_8 6_8.f90)
add_executable(6_10 6_10.f90)
add_executable(6_11 6_11.f90)
add_executable(6_12 6_12.f90)
add_executable(6_13 6_13.f90)

target_link_libraries(6_12 ch9)
target_link_libraries(6_13 ch9)