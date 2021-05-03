cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

project(Fortran_For_Engineers)

enable_language(Fortran)
set(CMAKE_Fortran_COMPILER gfortran)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/../include)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})


# add_subdirectory(modules)

add_library(all_chapters chapter_4.f90 chapter_5.f90 chapter_6.f90 chapter_7.f90)

add_subdirectory(demo)
# add_subdirectory(ch7)
# add_subdirectory(ch8)
# add_subdirectory(ch9)
# add_subdirectory(ch10)
# add_subdirectory(ch11)
# add_subdirectory(ch12)
# add_subdirectory(ch13)
# add_subdirectory(ch17)