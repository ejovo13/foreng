set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${MY_EX}/ch11)
make_directory(${MY_EX}/ch11)

add_executable(diagnostic diagnostic.f90)
add_executable(11_6 11_6.f90)
add_executable(11_7 11_7.f90)

target_link_libraries(11_6 foreng)
target_link_libraries(11_7 foreng)

target_link_libraries(diagnostic ch11)
