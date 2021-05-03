set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${MY_EX}/ch11)
make_directory(${MY_EX}/ch11)

add_executable(diagnostic diagnostic.f90)

target_link_libraries(diagnostic ch11)
