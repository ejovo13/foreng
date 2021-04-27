cmake_minimum_required(VERSION 3.1 FATAL_ERROR)

project(Fortran_For_Engineers)

enable_language(Fortran)
set(CMAKE_Fortran_COMPILER ifort)


include_directories(./modules)


# add_subdirectory(modules)

add_subdirectory(ch4)
# add_subdirectory(ch5)
# add_subdirectory(ch6)
# add_subdirectory(ch7)
# add_subdirectory(ch8)
# add_subdirectory(ch9)
# add_subdirectory(ch10)
# add_subdirectory(ch11)
# add_subdirectory(ch12)
# add_subdirectory(ch13)
add_subdirectory(ch17)