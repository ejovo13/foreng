set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../bin)

make_directory(${CMAKE_Fortran_MODULE_DIRECTORY})
make_directory(${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

add_library(all_chapters chapter_4.f90 chapter_5.f90 chapter_6.f90 chapter_7.f90)


add_subdirectory(foreng)
add_subdirectory(modules)
add_subdirectory(demo)
add_subdirectory(tests)