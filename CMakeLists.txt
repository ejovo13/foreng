cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

project(Fortran_For_Engineers VERSION 0.1)

enable_language(Fortran)
enable_testing()
set(CMAKE_Fortran_COMPILER gfortran)
set(MY_BIN ${CMAKE_BINARY_DIR}/../bin)
set(MY_EX ${MY_BIN}/exercises)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/../include)
set(CMAKE_INSTALL_PREFIX /usr/local/foreng-${PROJECT_VERSION})

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

add_subdirectory(src)
add_subdirectory(exercises)